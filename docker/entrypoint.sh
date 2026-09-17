#!/bin/sh
# ------------------------------------------------------------------
# Prepare the data volume, then hand off to the app.
#
# data/ is a mounted volume so that bookings, PODs and approvals survive a
# restart — the one thing the shinyapps.io deployment could not do, and the
# reason R/github_sync.R (which pushes runtime CSVs back to the repo to dodge
# that limitation) is left switched off here.
#
# A fresh volume mounts empty and masks the CSVs baked into the image, so the
# seed snapshot is kept at data-seed/ and copied across once.
# ------------------------------------------------------------------
set -e

DATA_DIR=/srv/tms/data
SEED_DIR=/srv/tms/data-seed

if [ -z "$(ls -A "$DATA_DIR" 2>/dev/null)" ]; then
  echo "[tms] $DATA_DIR is empty — installing the seed dataset"
  cp -a "$SEED_DIR"/. "$DATA_DIR"/
else
  echo "[tms] $DATA_DIR already populated — preserving runtime data"
fi

# data/reference/ (pincodes, cities, city_aliases) is static master data built
# offline by tools/build-pincodes.R, not runtime state — R/github_sync.R's
# SYNC_SKIP list says as much by refusing to ever push it back.
#
# It is refreshed from the image on every boot rather than seeded once. Two
# reasons: a volume created before this reference data existed would otherwise
# permanently shadow it and fail every pincode lookup, and updating the master
# list should be a matter of rebuilding the image, not hand-editing a volume.
if [ -d "$SEED_DIR/reference" ]; then
  echo "[tms] refreshing static reference data from the image"
  mkdir -p "$DATA_DIR/reference"
  cp -a "$SEED_DIR/reference"/. "$DATA_DIR/reference"/
fi

exec "$@"
