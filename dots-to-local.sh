#!/bin/bash

cd ~
cp ~/GitHub/Active/dotfiles/.vimrc .

cd ~/.emacs.d
cp ~/GitHub/Active/dotfiles/.emacs.d/init.el .

cd ~/.config
cp -R ~/GitHub/Active/dotfiles/.config/nvim .

cd ~/.config/karabiner
cp ~/GitHub/Active/dotfiles/.config/karabiner/karabiner.json .

cd ~/.tmux
cp ~/GitHub/Active/dotfiles/.tmux/.tmux.conf .
cp ~/GitHub/Active/dotfiles/.tmux/tmuxline .
