#!/usr/bin/env bash

# install homebrew if it isn't installed already
if ! hash brew 2>/dev/null; then
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
fi

brew bundle

stow . --target="$HOME"

fish --command="fisher"
fish --command="set -Ua fish_user_paths ~/.bin"

# change default shell to fish if it's just been installed
if ! grep -Fxq "/usr/local/bin/fish" /etc/shells; then
  echo /usr/local/bin/fish | sudo tee -a /etc/shells
  chsh -s /usr/local/bin/fish
fi
