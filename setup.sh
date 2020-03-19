#!/usr/bin/env bash

antibody bundle < ~/.zsh_plugins.txt > ~/.zsh_plugins.sh

fish --command="fisher"
fish --command="set -Ua fish_user_paths ~/.bin"

# install doom emacs if it isn't installed already
if [ ! -f "$HOME/.emacs.d/bin/doom" ]; then
  git clone https://github.com/hlissner/doom-emacs "$HOME/.emacs.d"
  "$HOME/.emacs.d/bin/doom" install
fi
