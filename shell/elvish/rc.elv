# inspo: https://github.com/muesli/dotfiles/blob/master/shell/rc.elv

use str
use epm
epm:install &silent-if-installed=$true ^
    github.com/zzamboni/elvish-modules

use github.com/zzamboni/elvish-modules/terminal-title

E:EDITOR="emacsclient -q -c -a ''"

E:ANDROID_HOME=$E:HOME"/Library/Android/sdk"

paths = [
  $E:ANDROID_HOME/emulator
  $E:ANDROID_HOME/tools
  $E:ANDROID_HOME/tools/bin
  $E:ANDROID_HOME/platform-tools
  ~/google-cloud-sdk/bin
  ~/.cargo/bin
  ~/.asdf/shims
  ~/.bin
  ~/.local/bin
  ~/.emacs.d/bin
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
  if (> (ps -ax | rg -c emacs) 1) {
    if (> (emacsclient -e '(length (frame-list))') 1) {
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
fn b [@a]{ buku --suggest --colors aaeca --db $E:HOME"/Tresors/System Config/bookmarks.db" $@a }
fn gs [@a]{ git status $@a }
fn ls [@a]{ echo; exa -F -I 'Desktop|Documents|Downloads|Library|Movies|Music|Pictures|Public|Tresors' $@a }
fn la [@a]{ ls -a $@a }
fn ll [@a]{ ls -alh $@a }
fn cd [@a]{ cd $@a; la }
fn otp [@a]{ pass otp -c $@a }
fn ssh [@a]{ kitty +kitten ssh $@a }
fn glow [@a]{ e:glow -s light $@a }
fn jjet [@a]{ jet --from json --keywordize --to edn --pretty $@a }
fn jjetq [@a]{ jet --from json --keywordize --to edn --pretty --query $@a }

use nix
use direnv
use secrets

fn crm-vpn { secrets:crm-vpn }
fn dev { secrets:dev }
fn preprod { secrets:preprod }
fn prod { secrets:prod }
