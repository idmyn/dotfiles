export UPDATE_ZSH_DAYS=13
plugins=(
  git
  zsh-autosuggestions
)
ZSH=$HOME/.oh-my-zsh
source $ZSH/oh-my-zsh.sh

PS1='
%~
  $ '

bindkey -e

function chpwd() {
    emulate -L zsh
    echo && ls -a
}

setopt autocd

LS_COLORS=$LS_COLORS:'di=0:fi=1:ln=4' ; export LS_COLORS; ls

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
export TERM=xterm-256color

alias zshrc="nvim ~/.zshrc"
alias kittyrc="nvim ~/.config/kitty/kitty.conf"
alias vimrc="nvim ~/.vimrc"
alias nvimrc="nvim ~/.config/nvim/init.vim"

alias dt="/Users/david/Dropbox/System/Desktop"
alias projects="/Users/david/Documents/Local\ Projects"
alias ghTest="/Users/david/GitHub/Active/test"
alias exP="/Users/david/Exercism/python"
alias exR="/Users/david/Exercism/ruby"
