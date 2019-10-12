export UPDATE_ZSH_DAYS=13
plugins=(
  git
  zsh-autosuggestions
)
ZSH=$HOME/.oh-my-zsh
source $ZSH/oh-my-zsh.sh

# https://arjanvandergaag.nl/blog/customize-zsh-prompt-with-vcs-info.html
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git svn
precmd() {
    vcs_info
}
setopt prompt_subst
zstyle ':vcs_info:git*' formats "%{$fg[blue]%}%b%{$reset_color%}%m%u%c%{$reset_color%}"

PS1='
%~
 ${vcs_info_msg_0_} $ '

bindkey -e

export LSCOLORS=AxFxcxdxbxegfhabagacEx
# colors via https://geoff.greer.fm/lscolors/

function chpwd() {
    emulate -L zsh
    echo && ls -a
    print -Pn "\e]51;A$(pwd)\e\\"; # Directory tracking for emacs-libvterm
}
setopt autocd

eval "$(pyenv init -)"

source $HOME/.aliases

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"

# Function for killing servers running at particular ports
function kp { kill $(sudo lsof -t -i:"$1"); }
