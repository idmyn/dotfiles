bind \ck down-or-search
bind \cl up-or-search

alias git="hub"
alias b="buku"
alias e="open -a emacs"
alias otp="pass otp -c"
alias doom="~/doom-emacs/bin/doom"

# for compatibility with eshell-z
set -Ux Z_DATA ~/.z

source /usr/local/opt/asdf/asdf.fish

if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end
