set -Ux EDITOR nvim
set -Ux NNN_USE_EDITOR 1

fish_vi_key_bindings

# Fisher
if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end

# Base16 Shell
if status --is-interactive
    set BASE16_SHELL "$HOME/.config/base16-shell/"
    source "$BASE16_SHELL/profile_helper.fish"
end

# nnn 'cd and quit'
export NNN_TMPFILE="/tmp/nnn"
function n --description 'support nnn quit and change directory'
        nnn $argv

        if test -e $NNN_TMPFILE
                source $NNN_TMPFILE
                rm $NNN_TMPFILE
        end
end

# Aliases
#alias ruby="/usr/local/Cellar/ruby/2.6.0_1/bin/ruby"
alias groff="cd /usr/local/Cellar/groff/1.22.3/bin/groff"

alias fishrc="nvim ~/.config/fish/config.fish"
alias vimrc="nvim ~/.vimrc"
alias nvimrc="nvim ~/.config/nvim/init.vim"
alias alacrittyrc="nvim ~/.config/alacritty/alacritty.yml"

alias dt="cd /Users/david/Dropbox/System/Desktop"
alias diss="cd /Users/david/GitHub/Active/dissertation"
alias projects="cd /Users/david/Documents/Local\ Projects"
alias ghTest="cd /Users/david/GitHub/Active/test"
alias exP="cd /Users/david/Exercism/python"
alias exR="cd /Users/david/Exercism/ruby"
alias exJ="cd /Users/david/Exercism/java"
set -g fish_user_paths "/usr/local/sbin" $fish_user_paths

status --is-interactive; and source (rbenv init -|psub)
