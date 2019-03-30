#!/bin/bash

cd ~
cp ~/GitHub/Active/dotfiles/.vimrc .

cd ~/.tmux
cp ~/GitHub/Active/dotfiles/.tmux/.tmux.conf .
cp ~/GitHub/Active/dotfiles/.tmux/tmuxline .

cd ~/.emacs.d
cp ~/GitHub/Active/dotfiles/.emacs.d/init.el .

cd ~/.config
cp -R ~/GitHub/Active/dotfiles/.config/fish .
cp -R ~/GitHub/Active/dotfiles/.config/nvim .
cp ~/GitHub/Active/dotfiles/.config/karabiner.edn .
