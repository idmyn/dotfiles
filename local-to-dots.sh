#!/bin/bash

cd ~/GitHub/Active/dotfiles
cp ~/.vimrc .
cp ~/.zshrc .

cd ~/GitHub/Active/dotfiles/.emacs.d
cp ~/.emacs.d/init.el .

cd ~/GitHub/Active/dotfiles/.config
cp -R ~/.config/nvim .

cd ~/GitHub/Active/dotfiles/.config/karabiner
cp ~/.config/karabiner/karabiner.json .

cd ~/GitHub/Active/dotfiles/.tmux
cp ~/.tmux/.tmux.conf .
cp ~/.tmux/tmuxline .