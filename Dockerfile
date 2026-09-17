# ==================================================================
# TMS — Transport Management System
#
# Self-hosted replacement for the shinyapps.io deployment. Runs Shiny
# directly rather than under shiny-server: a single app on a single port
# needs no vhost layer, and Cloudflare Tunnel terminates TLS in front.
# ==================================================================
FROM rocker/r-ver:4.4.2

# IST, matching the Sys.setenv(TZ=) pin in global.R. Set at the OS level too
# so container logs and file timestamps agree with what the app reports.
ENV TZ=Asia/Kolkata
ENV DEBIAN_FRONTEND=noninteractive

# System libraries. P3M ships precompiled R packages but not the C libraries
# they link against, so these are runtime requirements, not build tooling:
# curl/openssl/xml2 back the http stack, icu backs stringr's collation,
# png/jpeg/tiff/freetype back plotly and leaflet rendering, zlib backs
# openxlsx's zip container, and libuv backs fs -> sass -> bslib (without it
# every page 500s on bs_theme_dependencies, which is the app's entire theme).
#
# Deliberately NOT installed: gdal, geos, proj and udunits. leaflet lists sf
# among its Imports, which drags in sf/terra/raster, but this app only ever
# passes plain lng/lat data frames to leaflet::addCircleMarkers — sf's
# namespace is never loaded at runtime (verified), and those four libraries
# would add roughly a gigabyte to the image for code that never executes.
RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        libcurl4-openssl-dev \
        libuv1-dev \
        libssl-dev \
        libxml2-dev \
        libicu-dev \
        zlib1g-dev \
        libpng-dev \
        libjpeg-dev \
        libtiff-dev \
        libfontconfig1-dev \
        libfreetype6-dev \
        libharfbuzz-dev \
        libfribidi-dev \
        tzdata \
    && ln -snf "/usr/share/zoneinfo/$TZ" /etc/localtime \
    && echo "$TZ" > /etc/timezone \
    && rm -rf /var/lib/apt/lists/*

# R packages get their own layer, copied before the app source. Editing a
# module then rebuilds in seconds instead of reinstalling 20 packages.
COPY docker/install_r_deps.R /tmp/install_r_deps.R
RUN Rscript /tmp/install_r_deps.R && rm -f /tmp/install_r_deps.R

WORKDIR /srv/tms

COPY . /srv/tms

# The committed CSVs double as the seed snapshot; see docker/entrypoint.sh.
RUN cp -a /srv/tms/data /srv/tms/data-seed

# Run unprivileged. The app writes only to data/, so that is the only path
# the runtime user needs to own.
RUN useradd --create-home --shell /bin/bash tms \
    && chown -R tms:tms /srv/tms
USER tms

# Pre-compile the bslib theme so the Inter / JetBrains Mono webfonts are baked
# into the image. R/theme.R asks for them with font_google(local = TRUE), which
# otherwise downloads from Google on the first page render of every fresh
# container — a visible stall for whoever loads the app first, and a hard
# runtime dependency on Google being reachable from the Mac mini. Warming it
# here as the tms user puts the cache in /home/tms/.cache/R/sass, where the
# running app looks for it.
RUN cd /srv/tms && Rscript -e "\
      suppressMessages({source('global.R'); source('R/theme.R')}); \
      invisible(bslib::bs_theme_dependencies(app_theme()))"

EXPOSE 3838

# Shiny answers on / once the session manager is up; a 200 here means R has
# finished sourcing global.R, the 31 modules and the seed check.
HEALTHCHECK --interval=30s --timeout=5s --start-period=90s --retries=3 \
    CMD curl -fsS http://127.0.0.1:3838/ > /dev/null || exit 1

ENTRYPOINT ["/srv/tms/docker/entrypoint.sh"]

# 0.0.0.0 rather than the 127.0.0.1 default — the cloudflared sidecar reaches
# this over the compose network, so binding to loopback would black-hole it.
CMD ["R", "--quiet", "--no-save", "-e", \
     "shiny::runApp('/srv/tms', host = '0.0.0.0', port = 3838, launch.browser = FALSE)"]
