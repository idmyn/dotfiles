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

source /usr/local/opt/chruby/share/chruby/chruby.sh
chruby ruby-2.6.3
eval "$(pyenv init -)"

source $HOME/.aliases

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"

# Function for killing servers running at particular ports
function kp { kill $(sudo lsof -t -i:"$1"); }
