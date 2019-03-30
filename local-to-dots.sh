#!/bin/bash

cd ~/GitHub/Active/dotfiles
cp ~/.vimrc .

cd ~/GitHub/Active/dotfiles/.tmux
cp ~/.tmux/.tmux.conf .
cp ~/.tmux/tmuxline .

cd ~/GitHub/Active/dotfiles/.emacs.d
cp ~/.emacs.d/init.el .

cd ~/GitHub/Active/dotfiles/.config
cp -R ~/.config/fish .
cp -R ~/.config/nvim .
cp ~/.config/karabiner.edn .
