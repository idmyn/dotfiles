# inspo: https://github.com/muesli/dotfiles/blob/master/shell/rc.elv

use epm
epm:install &silent-if-installed=$true \
    github.com/zzamboni/elvish-modules

use github.com/zzamboni/elvish-modules/terminal-title

E:EDITOR="emacs -nw"

E:ANDROID_HOME=$E:HOME"/Library/Android/sdk"

paths = [
  $E:ANDROID_HOME/emulator
  $E:ANDROID_HOME/tools
  $E:ANDROID_HOME/tools/bin
  $E:ANDROID_HOME/platform-tools
  ~/.cargo/bin
  ~/.asdf/shims
  /usr/local/opt/asdf/bin
  /usr/local/bin
  $@paths
]

# prompt
edit:prompt = { put "\n"; tilde-abbr $pwd; put "> " }
edit:rprompt = { put "" }

# keybindings
edit:insert:binding[Alt+Backspace]=$edit:kill-small-word-left~

# aliases
fn q { exit }
fn e [@a]{ open -a Emacs $@a }
fn gs [@a]{ git status $@a }
fn ls [@a]{ e:ls -GF $@a }
fn la [@a]{ ls -GFa $@a }
fn ll [@a]{ ls -GFalh $@a }
fn otp [@a]{ pass otp -c $@a }

# run this once: direnv hook elvish > ~/.elvish/lib/direnv.elv
use direnv
