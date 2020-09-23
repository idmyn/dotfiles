#!/usr/bin/env bash
set -euo pipefail

LOGFILE="${HOME}/.backup.log"

if [ ! -e $LOGFILE ] ; then
    touch $LOGFILE
fi

echo "Beginning backup at `date`" >> $LOGFILE

source .backup.env

/usr/local/bin/restic backup ~     \
    --exclude="**/node_modules"    \
    --exclude="${HOME}/desk"       \
    --exclude="${HOME}/files"      \
    --exclude="${HOME}/.asdf"      \
    --exclude="${HOME}/.Trash"     \
    --exclude="${HOME}/Library"    \
    --exclude="${HOME}/Documents"  \
    --exclude="${HOME}/Pictures"   \
    --exclude="${HOME}/Public"     \
    --exclude="${HOME}/Movies"     \
    --exclude="${HOME}/Music"      \
    --exclude="${HOME}/Tresors"    \
    >> $LOGFILE 2>&1

printf "\n*******************************************************\n\n" >> $LOGFILE
