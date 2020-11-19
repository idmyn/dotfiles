source ~/.aliases
source ~/.secret-aliases

PROMPT="
%~
 $ "

setopt autocd
function chpwd() {
    emulate -L zsh
    echo; exa
}

zstyle ':completion:*' menu select # prettier tab completion

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND='fd --type f'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

source <(antibody init)
antibody bundle "skywind3000/z.lua"
export _ZL_MATCH_MODE=1
alias zi="z -I"
alias zh='z -I -t .'
alias b='z -b'

if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh/site-functions:$FPATH

  autoload -Uz compinit
  compinit
fi
