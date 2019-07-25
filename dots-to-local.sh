#!/bin/bash

cd ~
cp ~/GitHub/Active/dotfiles/.vimrc .
cp ~/GitHub/Active/dotfiles/.zshrc .

cd ~/.emacs.d
cp ~/GitHub/Active/dotfiles/.emacs.d/init.el .

cd ~/.config/nvim
cp ~/GitHub/Active/dotfiles/.config/nvim/init.vim .

cd ~/.config/karabiner
cp ~/GitHub/Active/dotfiles/.config/karabiner/karabiner.json .

cd ~/.config/
cp ~/GitHub/Active/dotfiles/.config/karabiner.edn .

cd ~/.config/tridactyl
cp ~/GitHub/Active/dotfiles/.config/tridactyl/tridactylrc .

cd ~/.tmux
cp ~/GitHub/Active/dotfiles/.tmux/.tmux.conf .
cp ~/GitHub/Active/dotfiles/.tmux/tmuxline .
