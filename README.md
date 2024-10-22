# my dots

To install these dotfiles: (assuming you're on macOS) install
[Nix](https://nixos.org), [cachix](https://docs.cachix.org/installation), and
home manager, clone this repo into
`~/.config/home-manager`, run `nix-shell -p git-crypt --run "git-crypt unlock"` to
decrypt the encypted files, and run `home-manager switch`

Note: I've initialised [git-crypt](https://github.com/AGWA/git-crypt) in this
repo to encrypt all files with the word `secret` anywhere in their name.
