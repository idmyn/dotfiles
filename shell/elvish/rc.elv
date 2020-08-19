# inspo: https://github.com/muesli/dotfiles/blob/master/shell/rc.elv

use str
use epm
epm:install &silent-if-installed=$true ^
    github.com/zzamboni/elvish-modules

use github.com/zzamboni/elvish-modules/terminal-title

E:EDITOR="emacsclient -n -a ''"

E:ANDROID_HOME=$E:HOME"/Library/Android/sdk"

paths = [
  $E:ANDROID_HOME/emulator
  $E:ANDROID_HOME/tools
  $E:ANDROID_HOME/tools/bin
  $E:ANDROID_HOME/platform-tools
  ~/.cargo/bin
  ~/.asdf/shims
  ~/.nix-profile/bin
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

fn e [@a]{
  if (str:contains (osascript -e 'application "emacs" is running') true) {
    if (> (osascript -e 'tell application "emacs" to get number of windows') 0) {
      osascript -e 'tell application "emacs" to activate first window'
      emacsclient -n $@a
    } else {
      emacsclient -nc $@a
      osascript -e 'tell application "emacs" to activate first window'
    }
  } else {
    emacs --daemon; emacsclient -nc $@a
  }
}

fn q { exit }
fn k { kubectl }
fn b [@a]{ buku --suggest --colors aaeca --db ~/Dropbox/System/bookmarks.db $@a }
fn gs [@a]{ git status $@a }
fn ls [@a]{ e:ls -GF $@a }
fn la [@a]{ ls -GFa $@a }
fn ll [@a]{ ls -GFalh $@a }
fn otp [@a]{ pass otp -c $@a }

use direnv
