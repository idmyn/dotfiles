#!/bin/bash

cd ~
cp ~/GitHub/Active/dotfiles/.vimrc .
cp ~/GitHub/Active/dotfiles/.tmux.conf .

cd ~/.config
cp -R ~/GitHub/Active/dotfiles/.config/fish .
cp -R ~/GitHub/Active/dotfiles/.config/nvim .
cp ~/GitHub/Active/dotfiles/.config/karabiner.edn .
