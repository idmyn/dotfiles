source ~/.secret-aliases

export EDITOR="vi"

PROMPT="
%~
 $ "

setopt autocd
alias q="exit"
alias ls="echo; ls"

. /usr/local/opt/asdf/asdf.sh
if [ -e /Users/david/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/david/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
