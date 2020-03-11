autoload -U colors && colors

autoload -Uz compinit
if [ $(date +'%j') != $(stat -f '%Sm' -t '%j' ~/.zcompdump) ]; then
  compinit
else
  compinit -C
fi

source ~/.aliases

. $(brew --prefix asdf)/asdf.sh

source ~/.zsh_plugins.sh

# https://arjanvandergaag.nl/blog/customize-zsh-prompt-with-vcs-info.html
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git svn
precmd() {
    vcs_info
}
setopt prompt_subst
zstyle ':vcs_info:git*' formats "%{$fg[blue]%}%b%{$reset_color%}%m%u%c%{$reset_color%}"

bindkey -e
bindkey -M emacs '^L' history-substring-search-up
bindkey -M emacs '^K' history-substring-search-down

# colors via https://geoff.greer.fm/lscolors/
export LSCOLORS=AxexcxdxbxegfhabagacEx

function chpwd() {
    emulate -L zsh
    echo && ls -GFA
    print -Pn "\e]51;A$(pwd)\e\\"; # Directory tracking for emacs-libvterm
}
setopt autocd
alias ls='ls -GF'

# Function for killing servers running at particular ports
function kp { kill $(sudo lsof -t -i:"$1"); }
