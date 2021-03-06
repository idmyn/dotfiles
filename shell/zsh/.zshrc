autoload -U colors && colors

autoload -Uz compinit
if [ $(date +'%j') != $(stat -f '%Sm' -t '%j' ~/.zcompdump) ]; then
  compinit
else
  compinit -C
fi

source ~/.aliases
source ~/.secret-aliases

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

# colors via https://geoff.greer.fm/lscolors/ based on exa defaults
export LSCOLORS=ExgxcxdxbxegfhabagacEx

function chpwd() {
    emulate -L zsh
    echo && ls -GFA
    print -Pn "\e]51;A$(pwd)\e\\"; # Directory tracking for emacs-libvterm
}
setopt autocd
alias ls='ls -GF'

# Function for killing servers running at particular ports
function kp { kill $(sudo lsof -t -i:"$1"); }

eval "$(direnv hook zsh)"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/davidmy/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/davidmy/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/davidmy/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/davidmy/google-cloud-sdk/completion.zsh.inc'; fi

export PHP_VERSIONS="/Applications/MAMP/bin/php"
source $(brew --prefix php-version)/php-version.sh
php-version 5.6
