export PATH=$HOME/.asdf/shims:/usr/local/opt/asdf/bin:$HOME/.nix-profile/bin:/run/current-system/sw/bin:$HOME/Library/Android/sdk/emulator:$HOME/Library/Android/sdk/tools:$HOME/Library/Android/sdk/tools/bin:$HOME/Library/Android/sdk/platform-tools:$HOME/google-cloud-sdk/bin:$HOME/.cargo/bin:$HOME/.bin:$HOME/.local/bin:$HOME/.emacs.d/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin

export EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"
export EMACSCLIENT="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
export EDITOR="$EMACSCLIENT -q -c -a ''"

. /usr/local/opt/asdf/asdf.sh
if [ -e /Users/david/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/david/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
