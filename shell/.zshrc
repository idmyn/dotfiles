source ~/.aliases
source ~/.secret-aliases

PROMPT="
%~
-> "

SHOW_K8S_PROMPT=0
function kp() {
  ((SHOW_K8S_PROMPT ^= 1)) # https://unix.stackexchange.com/a/364489
}
precmd() { [ "$SHOW_K8S_PROMPT" = 1 ] && RPROMPT="$(~/.bin/kubesummary)" || RPROMPT="" }

setopt autocd
function chpwd() {
    emulate -L zsh
    echo; exa
}

# https://stackoverflow.com/a/8595614
function eb() {
  perl -MFile::Temp -MFile::Copy -e \
  'copy *STDIN, $file = File::Temp->new; system "emacsclient", $file';
}

zstyle ':completion:*' menu select # prettier tab completion

setopt BANG_HIST # Treat the '!' character specially during expansion
setopt SHARE_HISTORY # append to history after each command
setopt HIST_IGNORE_ALL_DUPS
HISTSIZE=10000
SAVEHIST=10000

source ~/.zinit/bin/zinit.zsh

zinit ice wait lucid
zinit light skywind3000/z.lua
export _ZL_MATCH_MODE=1
alias zi="z -I"
alias zh='z -I -t .'
alias b='z -b'

zinit ice wait lucid
zinit light zsh-users/zsh-history-substring-search
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
zle -N history-substring-search-up
zle -N history-substring-search-down

if [ -d "$HOME/.asdf" ]; then
  zinit ice wait lucid
  zinit light asdf-vm/asdf
fi

zinit as"program" make'!' atclone'./direnv hook zsh > zhook.zsh' \
    atpull'%atclone' pick"direnv" src"zhook.zsh" for \
        direnv/direnv

zinit light-mode for \
  id-as'fzf/completion' https://github.com/junegunn/fzf/blob/master/shell/completion.zsh \
  id-as'fzf/key-bindings' https://github.com/junegunn/fzf/blob/master/shell/key-bindings.zsh
export FZF_DEFAULT_COMMAND='fd --type f'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

zinit wait lucid atload"zicompinit; zicdreplay" blockf for \
    zsh-users/zsh-completions
