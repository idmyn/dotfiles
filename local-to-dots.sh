#!/bin/bash

cd ~/GitHub/Active/dotfiles
cp ~/.vimrc .
cp -R ~/.tmux .

cd ~/GitHub/Active/dotfiles/.config
cp -R ~/.config/fish .
cp -R ~/.config/nvim .
cp ~/.config/karabiner.edn .
