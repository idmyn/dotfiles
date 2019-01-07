#!/bin/bash

cd ~/GitHub/Active/dotfiles
cp ~/.vimrc .
cp ~/.tmux.conf .

cd ~/GitHub/Active/dotfiles/.config
cp -R ~/.config/fish .
cp -R ~/.config/nvim .
