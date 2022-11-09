# my dots

To install these dotfiles: (assuming you're on macOS) install
[Nix](https://nixos.org), [cachix](https://docs.cachix.org/installation), and
[nix-darwin](https://github.com/LnL7/nix-darwin), clone this repo into
`~/.config/nixpkgs`, run `nix-shell -p git-crypt --run "git-crypt unlock"` to
decrypt the encypted files, and run `darwin-rebuild switch --flake
path:$HOME/.config/nixpkgs#mbp`

Note: I've initialised [git-crypt](https://github.com/AGWA/git-crypt) in this
repo to encrypt all files with the word `secret` anywhere in their name.
