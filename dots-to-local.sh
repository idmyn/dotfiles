#!/bin/bash

cd ~
cp ~/GitHub/Active/dotfiles/.vimrc .

cd ~/.config
cp -R ~/GitHub/Active/dotfiles/.config/fish .
cp -R ~/GitHub/Active/dotfiles/.config/nvim .
