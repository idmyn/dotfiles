#!/bin/sh
set -euf

pass backup.env > tmp.sh
. tmp.sh
rm tmp.sh

LOGFILE="${HOME}/.backup.log"

if [ ! -e "$LOGFILE" ] ; then
    touch "$LOGFILE"
fi

echo "Beginning backup at $(date)" >> "$LOGFILE"

/usr/local/bin/restic backup ~       \
    --exclude="**/node_modules"      \
    --exclude="**/straight/repos"    \
    --exclude="${HOME}/desk"         \
    --exclude="${HOME}/files"        \
    --exclude="${HOME}/mail"         \
    --exclude="${HOME}/.asdf"        \
    --exclude="${HOME}/.cache"       \
    --exclude="${HOME}/.stack"       \
    --exclude="${HOME}/.cargo"       \
    --exclude="${HOME}/.rustup"      \
    --exclude="${HOME}/.emacs.d"     \
    --exclude="${HOME}/.Trash"       \
    --exclude="${HOME}/Library"      \
    --exclude="${HOME}/Desktop"      \
    --exclude="${HOME}/Documents"    \
    --exclude="${HOME}/Applications" \
    --exclude="${HOME}/Pictures"     \
    --exclude="${HOME}/Public"       \
    --exclude="${HOME}/Movies"       \
    --exclude="${HOME}/Music"        \
    --exclude="${HOME}/Tresors"      \
    >> "$LOGFILE" 2>&1

printf "\n*******************************************************\n\n" >> "$LOGFILE"
