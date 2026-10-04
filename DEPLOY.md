# Self-hosting TMS on a Mac mini

Deployed from the `minimal-setup` branch, living at `~/apps/TMS-Transport`,
serving <https://tms.katxel.in>.

Replaces the shinyapps.io deployment with a container on your own hardware,
published through a Cloudflare Tunnel. Three things change for the better:

- **Data persists.** shinyapps.io hands every instance an ephemeral filesystem,
  so a booking made on the demo vanishes on restart. Here `data/` is a Docker
  volume that survives restarts, rebuilds and reboots.
- **No inbound ports.** The tunnel dials *out* to Cloudflare. Nothing is
  forwarded on the router, no static IP, no firewall hole, and the Mac mini's
  home IP is never exposed.
- **Real TLS on your own domain**, terminated at Cloudflare's edge.

```
browser ──https──> Cloudflare edge ──tunnel──> cloudflared ──http──> app:3838
                                               └──────── Mac mini ────────┘
```

---

## What runs where

| Piece | What it is |
|---|---|
| Colima | The Docker daemon on macOS (a Linux VM). Docker Desktop is not installed. |
| `tms-app` | R 4.4.2 + Shiny, serving the app on port 3838 |
| `tms-cloudflared` | Outbound tunnel connector |
| `tms-data` volume | The 30 CSV tables the app reads and writes |

---

## First-time setup

### 1. Start the VM

```bash
colima start --cpu 3 --memory 4 --disk 40 --vm-type vz
```

Once created, `colima start` alone is enough — the settings stick.

### 2. Authenticate cloudflared to your Cloudflare account

```bash
cloudflared tunnel login
```

Opens a browser. Pick the domain you want to serve from. This writes
`~/.cloudflared/cert.pem`, which authorises creating tunnels and DNS records
for that zone. One-time.

### 3. Create the tunnel

```bash
cloudflared tunnel create tms
```

Prints a UUID and writes `~/.cloudflared/<UUID>.json` — the tunnel's
credentials. Copy that file next to the compose file:

```bash
cp ~/.cloudflared/<UUID>.json cloudflared/
```

### 4. Point a hostname at it

```bash
cloudflared tunnel route dns tms tms.example.com
```

Creates a proxied CNAME in Cloudflare DNS. No A record, no public IP.

### 5. Write the ingress config

```bash
sed -e "s/REPLACE_WITH_TUNNEL_ID/<UUID>/g" \
    -e "s/REPLACE_WITH_HOSTNAME/tms.example.com/g" \
    cloudflared/config.yml.example > cloudflared/config.yml
```

### 6. Bring it up

```bash
docker compose up -d --build
```

The first build installs ~20 R packages as precompiled Linux binaries from
Posit Package Manager — several minutes. Rebuilds after a code change take
seconds, because the package layer is cached above the source copy.

---

## Day-to-day

`tms` wraps the lot. It lives in `deploy/tms`, is symlinked into
`~/.local/bin`, and works from any directory:

| Command | What it does |
|---|---|
| `tms status` | Colima, both containers, and whether local and live answer |
| `tms start` | Starts Colima if needed (with recovery), then the containers |
| `tms restart` | Restarts both containers |
| `tms rebuild` | Rebuilds the image from source and restarts — use after a code change |
| `tms stop` | Stops the containers, leaves Colima running |
| `tms halt` | Stops the containers *and* Colima — the end-of-day one |
| `tms down` | **Removes** the containers (keeps the data volume) — only before a rebuild |
| `tms logs` | Follows the app log (`tms logs cloudflared` for the tunnel) |
| `tms doctor` | Everything above plus the Lima instance, the boot job, auto-login and recent errors |
| `tms url` | Prints the public address |

Data survives all of these — it is in the `tms-data` volume, not the
containers.

The same thing by hand, if you would rather not use the wrapper:

```bash
cd /Users/mac/apps/TMS-Transport
colima status                      # is the VM up at all
docker compose ps                  # what is running and healthy
docker compose logs -f app         # Shiny's own output
docker compose logs -f cloudflared # tunnel connections
docker compose restart app         # restart just the app
docker compose up -d --build       # after a git pull
docker compose down                # stop the containers (data volume survives)
colima stop                        # stop the VM as well
```

Use `docker compose stop`, not `down`, for an ordinary shutdown. `down`
*removes* the containers, so a later `colima start` gives you a running VM
with nothing in it, the tunnel container no longer exists, and Cloudflare
answers 1033 — which looks exactly like a fault and is not one. `tms stop`
and `tms halt` use `stop` for this reason.

Local check without going through Cloudflare, from the Mac mini itself:

```bash
curl -I http://localhost:3838
```

---

## Updating the app

```bash
git pull
docker compose up -d --build
```

The `tms-data` volume is untouched by a rebuild, so live records survive.

---

## The data volume

```bash
# Back up
docker run --rm -v tms-data:/data -v "$PWD:/backup" alpine \
  tar czf /backup/tms-data-$(date +%F).tar.gz -C /data .

# Restore
docker run --rm -v tms-data:/data -v "$PWD:/backup" alpine \
  sh -c "rm -rf /data/* && tar xzf /backup/tms-data-YYYY-MM-DD.tar.gz -C /data"

# Reset to the seed snapshot shipped in the repo
docker compose down && docker volume rm tms-data && docker compose up -d
```

`docker/entrypoint.sh` copies the repo's committed CSVs into the volume the
first time it is empty, so a fresh volume starts on the same dataset the
README describes rather than an empty app.

---

## Surviving a reboot

The containers use `restart: unless-stopped`, so Docker restores them once the
VM is up. The VM itself needs a nudge at login:

```bash
cp deploy/com.tms.colima.plist ~/Library/LaunchAgents/
launchctl load -w ~/Library/LaunchAgents/com.tms.colima.plist
```

LaunchAgents fire at **login**, not at boot. For a headless Mac mini, turn on
System Settings → Users & Groups → *Automatically log in as*, or the app stays
down after a power cut until someone signs in.

---

## Configuration

`.env` (copy from `.env.example`):

| Variable | Default | Effect |
|---|---|---|
| `TMS_DEMO_LOGIN` | `true` | `false` hides the login screen's quick-preview role chips and requires real credentials |

Set it to `false` before this is reachable by anyone outside your team — the
seeded accounts all share the password in the README.

---

## GitHub data sync is deliberately off

`R/github_sync.R` pushes runtime CSVs back into the repo once a minute. That
module exists to survive shinyapps.io, which hands every instance an ephemeral
filesystem — without the push, a day's bookings are simply gone when the
instance idles out.

This deployment does not have that problem. `data/` is a Docker volume; it
survives restarts, rebuilds and reboots. Switching sync on here would commit
every booking, POD and GPS ping to a **public** GitHub repo on a one-minute
timer, which is a downgrade, not a feature.

So `TMS_GITHUB_PAT` is left unset. The module treats unconfigured as a
supported state and stays completely inert — no writes, no errors, the Settings
badge just reads "Not configured." To enable it anyway, set the three
`TMS_GITHUB_*` variables in `.env` (gitignored). Never commit the token.

---

## Static reference data

`data/reference/` holds the pincode, city and alias masters — 19k rows built
offline by `tools/build-pincodes.R`. It is static master data, not runtime
state, which is why `SYNC_SKIP` in `R/github_sync.R` refuses to push it.

It sits *inside* the volume mount path, so a volume created before it existed
would permanently shadow it and fail every pincode lookup.
`docker/entrypoint.sh` therefore refreshes `reference/` from the image on every
boot while leaving the runtime tables alone. Updating the masters is a matter
of rebuilding the image, not hand-editing a volume.

---

## Switching branches

The dataset schema is branch-specific — `minimal-setup` has ten columns on
`bookings` that `main` does not, plus an `ewaybill_partb` table. Pointing the
app at a volume seeded from the other branch will fail on the missing columns.
Reset the volume when you switch:

```bash
docker compose down && docker volume rm tms-data && docker compose up -d --build
```

---

## Troubleshooting

**`cloudflared` logs show `dial tcp: lookup app`** — the app container is not
up. `docker compose logs app`.

**Page renders unstyled, or the layout looks broken after a deploy** — almost
certainly a stale stylesheet at Cloudflare's edge, not a CSS bug. Compare what
the edge serves against the origin:

```bash
curl -sI https://tms.katxel.in/styles.css | grep -i cf-cache-status
curl -s https://tms.katxel.in/styles.css | wc -c
curl -s http://localhost:3838/styles.css | wc -c
```

Different byte counts with `cf-cache-status: HIT` means the edge is serving the
previous release. `asset_url()` in `global.R` is what prevents this — it appends
a content hash to `styles.css` and the logo, so a changed file gets a changed
URL. If you add another asset in `www/` and reference it by stable name, route
it through `asset_url()` too, or it will hit exactly this.

**Browser shows "Disconnected from server"** — Shiny holds a WebSocket open for
the life of a session. The `originRequest` keepalive settings in
`cloudflared/config.yml` exist to stop the origin hop timing it out; if it
persists, check that the Cloudflare dashboard has WebSockets enabled for the
zone (Network → WebSockets).

**Error 1033 / tunnel not found** — `cloudflared/config.yml` and the credentials
JSON disagree on the UUID, or the JSON was not copied into `cloudflared/`.

**Build fails on an R package** — the install is deliberately fatal on failure
(`docker/install_r_deps.R`) rather than producing an image that breaks at
runtime. The package name is in the last lines of the build output; it usually
means a missing system library to add to the `apt-get` list in the Dockerfile.

**Cloudflare Error 1033 after a reboot** — the tunnel has nothing to reach:
Colima is down. Check it first:

```bash
colima status
LIMA_HOME=~/.colima/_lima limactl list
```

A status of **Broken** with `vz driver is running but host agent is not` is the
known aftermath of an unclean shutdown. Colima records the VM's process id in
`~/.colima/_lima/colima/vz.pid` and checks on the next boot whether that pid
exists — but after a reboot the number has usually been reissued to something
unrelated, so Colima believes the VM is already up and refuses to start it.

`deploy/colima-boot.sh` handles this and is what the LaunchAgent runs. To do it
by hand: confirm nothing owns the pid, delete the file, start again.

```bash
ps -p "$(cat ~/.colima/_lima/colima/vz.pid)" -o comm=
rm ~/.colima/_lima/colima/vz.pid
colima start
```

The containers carry `restart: unless-stopped`, so they come back on their own
once the VM is up; there is nothing else to restart.

**`docker: command not found` in a launchd job or cron** — those get a minimal
PATH; use the absolute `/Users/mac/.local/bin/docker`.
