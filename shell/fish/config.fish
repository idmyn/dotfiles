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

# for emacs-libvterm
function vterm_printf;
    if [ -n "$TMUX" ]
        # tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
    else if string match -q -- "screen*" "$TERM"
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$argv"
    else
        printf "\e]%s\e\\" "$argv"
    end
end

contains $fish_user_paths ~/.ghcup/env; or set -Ua fish_user_paths ~/.ghcup/bin
contains $fish_user_paths ~/.local/bin; or set -Ua fish_user_paths ~/.local/bin
contains $fish_user_paths ~/.bin; or set -Ua fish_user_paths ~/.bin

test -f ~/google-cloud-sdk/path.fish.inc && source ~/google-cloud-sdk/path.fish.inc
test -f ~/google-cloud-sdk/completion.fish.inc && source ~/google-cloud-sdk/completion.fish.inc

[ -f ~/.config/tabtab/fish/__tabtab.fish ]; and . ~/.config/tabtab/fish/__tabtab.fish; or true
