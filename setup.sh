#!/usr/bin/env bash

# install doom emacs if it isn't installed already
if [ ! -f "$HOME/.emacs.d/bin/doom" ]; then
  git clone https://github.com/hlissner/doom-emacs "$HOME/.emacs.d"
  "$HOME/.emacs.d/bin/doom" install
fi
