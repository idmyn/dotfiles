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

export LSCOLORS=AxFxcxdxbxegfhabagacEx
# colors via https://geoff.greer.fm/lscolors/

function chpwd() {
    emulate -L zsh
    echo && ls -a
}
setopt autocd

source $HOME/.aliases
