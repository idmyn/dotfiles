# my dots

To install these dotfiles: install [Nix](https://nixos.org) and [home-manager](https://github.com/nix-community/home-manager), clone this repo into
`~/.config/nixpkgs` and run `home-manager switch`. If you're on macOS,
additionally install [nix-darwin](https://github.com/LnL7/nix-darwin) and run
`darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix`

Note: I've initialised [git-crypt](https://github.com/AGWA/git-crypt) in this
repo to encrypt all files with the word `secret` anywhere in their name.
