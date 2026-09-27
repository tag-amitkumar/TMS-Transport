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

# Tables added by a release that the running volume has never seen.
#
# The seed above only fires on a completely empty volume, which is right for
# runtime data — nobody wants last week's bookings replaced on restart. But it
# means a release introducing a NEW table installs nothing: the volume is not
# empty, so the branch, rate card and product masters added in September would
# simply be absent, and the app would run with an empty rate card and no
# serviceability lists rather than failing loudly.
#
# So: install any seed table the volume does not have, and never touch one it
# does. A file that exists is the operator's data and is left alone.
for seed_file in "$SEED_DIR"/*.csv; do
  [ -e "$seed_file" ] || continue
  name=$(basename "$seed_file")
  if [ ! -e "$DATA_DIR/$name" ]; then
    echo "[tms] new table since this volume was created — installing $name"
    cp -a "$seed_file" "$DATA_DIR/$name"
  fi
done

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
