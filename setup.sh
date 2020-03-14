#!/usr/bin/env bash

# install homebrew if it isn't installed already
if ! hash brew 2>/dev/null; then
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
fi

brew bundle

# use commands for git config, rather than version controlling ~/.gitconfig?
git config --global core.excludesfile ~/.gitignore_global

fish --command="fisher"
fish --command="set -Ua fish_user_paths ~/.bin"

# install doom emacs if it isn't installed already
if [ ! -f "$HOME/.emacs.d/bin/doom" ]; then
  git clone https://github.com/hlissner/doom-emacs "$HOME/.emacs.d"
  "$HOME/.emacs.d/bin/doom" install
fi
