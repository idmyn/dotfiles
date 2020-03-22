bind \ck down-or-search
bind \cl up-or-search

source ~/.aliases
source ~/.config/fish/secrets.fish

# for compatibility with eshell-z
set -Ux Z_DATA ~/.z

source /usr/local/opt/asdf/asdf.fish

eval (direnv hook fish)

if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end

test -f ~/google-cloud-sdk/path.fish.inc && source ~/google-cloud-sdk/path.fish.inc
test -f ~/google-cloud-sdk/completion.fish.inc && source ~/google-cloud-sdk/completion.fish.inc
