autoload -U colors && colors

autoload -Uz compinit
if [ $(date +'%j') != $(stat -f '%Sm' -t '%j' ~/.zcompdump) ]; then
  compinit
else
  compinit -C
fi

source <(antibody init)
antibody bundle < ~/.zsh_plugins.txt

ENHANCD_FILTER=fzy; export ENHANCD_FILTER

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

# colors via https://geoff.greer.fm/lscolors/
export LSCOLORS=AxexcxdxbxegfhabagacEx

function chpwd() {
    emulate -L zsh
    echo && ls -GFA
    print -Pn "\e]51;A$(pwd)\e\\"; # Directory tracking for emacs-libvterm
}
setopt autocd
alias ls='ls -GF'

eval "$(pyenv init -)"

source $HOME/.aliases

# Function for killing servers running at particular ports
function kp { kill $(sudo lsof -t -i:"$1"); }
function e { emacsclient -a '' -c $1 &; }

source /usr/local/opt/chruby/share/chruby/chruby.sh
RUBIES+=(~/.rvm/rubies/*)
chruby ruby-2.6.1
