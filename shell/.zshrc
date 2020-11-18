source ~/.secret-aliases

export EDITOR="nvim"

PROMPT="
%~
 $ "

alias q="exit"
alias ls="echo; exa"

setopt autocd
function chpwd() {
    emulate -L zsh
    ls
}

zstyle ':completion:*' menu select

if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh/site-functions:$FPATH

  autoload -Uz compinit
  compinit
fi

source <(antibody init)
antibody bundle "agkozak/zsh-z"
