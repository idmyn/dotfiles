#!/bin/bash

cd ~
cp ~/GitHub/Active/dotfiles/.vimrc .
cp -R ~/GitHub/Active/dotfiles/.tmux .

cd ~/.config
cp -R ~/GitHub/Active/dotfiles/.config/fish .
cp -R ~/GitHub/Active/dotfiles/.config/nvim .
cp ~/GitHub/Active/dotfiles/.config/karabiner.edn .
