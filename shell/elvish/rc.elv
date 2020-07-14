# inspo: https://github.com/muesli/dotfiles/blob/master/shell/rc.elv

use epm
epm:install &silent-if-installed=$true \
    github.com/zzamboni/elvish-modules

use github.com/zzamboni/elvish-modules/terminal-title

paths = [/usr/local/opt/asdf/bin ~/.asdf/shims /usr/local/bin $@paths]

E:EDITOR="emacs -nw"

# prompt
edit:prompt = { put "\n"; tilde-abbr $pwd; put "> " }
edit:rprompt = { put "" }

# keybindings
edit:insert:binding[Alt+Backspace]=$edit:kill-small-word-left~

# aliases
fn q { exit }
fn e [@a]{ open -a Emacs $@a }
fn ls [@a]{ e:ls -GF $@a }
fn la [@a]{ ls -GFa $@a }
fn ll [@a]{ ls -GFalh $@a }
fn otp [@a]{ pass otp -c $@a }

# run this once: direnv hook elvish > ~/.elvish/lib/direnv.elv
use direnv
