#!/usr/bin/env bash
set -euo pipefail

LOGFILE="${HOME}/.backup.log"

if [ ! -e $LOGFILE ] ; then
    touch $LOGFILE
fi

echo "Beginning backup at `date`" >> $LOGFILE

source .backup.env

restic -r b2:idmyn-mbp-restic:backup backup ~ \
    --exclude="**/node_modules"               \
    --exclude="${HOME}/desk"                  \
    --exclude="${HOME}/files"                 \
    --exclude="${HOME}/.asdf"                 \
    --exclude="${HOME}/.Trash"                \
    --exclude="${HOME}/Library"               \
    >> $LOGFILE

printf "\n*******************************************************\n\n" >> $LOGFILE
