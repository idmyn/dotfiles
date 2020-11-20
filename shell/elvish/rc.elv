# inspo: https://github.com/muesli/dotfiles/blob/master/shell/rc.elv

use str
use epm
epm:install &silent-if-installed=$true ^
    github.com/zzamboni/elvish-modules

use github.com/zzamboni/elvish-modules/terminal-title

E:EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"
E:EMACSCLIENT="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
E:EDITOR="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -q -c -a ''"

E:GIT_DUET_GLOBAL="true"
E:GIT_DUET_CO_AUTHORED_BY="1"

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

fn emacs [@a]{ /Applications/Emacs.app/Contents/MacOS/Emacs $@a }
fn emacsclient [@a]{ /Applications/Emacs.app/Contents/MacOS/bin/emacsclient $@a }

fn e [@a]{
  if (> (ps -ax | rg -c -i emacs) 1) {
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
fn tf [@a]{ terraform $@a }
fn k [@a]{ kubectl $@a }
fn kns [@a]{ kubens $@a }
fn kdebug { kubectl run -i --rm --tty debug --image=praqma/network-multitool --restart=Never -- sh }
fn b [@a]{ buku --suggest --colors aaeca --db $E:HOME"/.config/cloud/bookmarks.db" $@a }
fn gs [@a]{ git status $@a }
fn gb [@a]{ git branch $@a }
fn ls [@a]{ echo; exa -F -I 'Applications|Desktop|Documents|Downloads|Library|Movies|Music|Pictures|Public|Tresors' $@a }
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
use k8s

fn kprompt { k8s:toggle-prompt }

fn crm-vpn { secrets:crm-vpn }
fn dev { secrets:dev }
fn dev-test { secrets:dev-test }
fn preprod { secrets:preprod }
fn prod { secrets:prod }
