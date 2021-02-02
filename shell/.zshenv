export PATH=$HOME/.asdf/shims:/usr/local/opt/asdf/bin:/run/current-system/sw/bin:$HOME/Library/Android/sdk/emulator:$HOME/Library/Android/sdk/tools:$HOME/Library/Android/sdk/tools/bin:$HOME/Library/Android/sdk/platform-tools:$HOME/google-cloud-sdk/bin:$HOME/.cargo/bin:$HOME/.bin:$HOME/.local/bin:$HOME/.emacs.d/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin

export ANDROID_SDK_ROOT=$HOME/Library/Android/sdk

export GLAMOUR_STYLE=light

export EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"
export EMACSCLIENT="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
export EDITOR="$EMACSCLIENT -q -c -a ''"

export GIT_DUET_GLOBAL="true"
export GIT_DUET_CO_AUTHORED_BY="1"

. /usr/local/opt/asdf/asdf.sh
if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then . ~/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
export NIX_PATH="~/.nix-defexpr/channels:darwin-config=~/.nixpkgs/darwin-configuration.nix"
export NIX_SSL_CERT_FILE="/nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt"
