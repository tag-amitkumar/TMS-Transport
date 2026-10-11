#!/bin/bash
# ------------------------------------------------------------------
# Bring Colima up at login, recovering from an unclean shutdown.
#
# `colima start` on its own is not enough after a power cut, which is exactly
# when it is needed. Colima records the VM process id in vz.pid and, on the
# next boot, checks whether that pid exists. After a reboot the number has
# almost always been handed to something else — on 3 Oct 2026 it belonged to
# CrashReporterSupportHelper — so Colima concluded the VM was already running,
# reported the instance as Broken, and refused to start it. The tunnel stayed
# down for a day and a half until somebody looked.
#
# So: if the instance is Broken and no Lima process is actually alive, clear
# the orphaned pid file and start normally. Both conditions are checked,
# because deleting that file while a VM really is running would let a second
# one start on the same disk image.
# ------------------------------------------------------------------
set -u
export PATH="$HOME/.local/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"
LIMA_HOME="$HOME/.colima/_lima"
export LIMA_HOME
PIDFILE="$LIMA_HOME/colima/vz.pid"

log() { echo "[colima-boot] $(date '+%Y-%m-%d %H:%M:%S') $*"; }

if colima status >/dev/null 2>&1; then
  log "already running"
  exit 0
fi

if limactl list 2>/dev/null | grep -q "Broken"; then
  log "instance reports Broken"
  if pgrep -f "limactl hostagent" >/dev/null 2>&1; then
    log "a Lima host agent IS running — leaving the pid file alone"
  elif [ -f "$PIDFILE" ]; then
    pid=$(cat "$PIDFILE" 2>/dev/null || echo "")
    # Only clear it when nothing owns the pid, or what owns it plainly is not
    # the VM. A recycled pid belongs to some unrelated process.
    if [ -z "$pid" ] || ! ps -p "$pid" -o comm= 2>/dev/null | grep -qiE "limactl|colima|vz"; then
      log "clearing orphaned pid file (pid '$pid' is not a Lima process)"
      rm -f "$PIDFILE"
    else
      log "pid $pid looks like a live Lima process — not touching it"
    fi
  fi
fi

log "starting colima"
colima start 2>&1 || { log "colima start FAILED"; exit 1; }
log "colima is up"
