{ config, pkgs, lib, ... }:

let
  doom-emacs = pkgs.callPackage (builtins.fetchTarball {
    url = https://github.com/vlaci/nix-doom-emacs/archive/master.tar.gz;
  }) {
    doomPrivateDir = ../dotfiles/dot-doom.d;
  };
in

{
  imports = [ <home-manager/nix-darwin> ];
  users.users.davidmy.home = "/Users/davidmy";
  home-manager.useGlobalPkgs = true; # not sure I need this line anymore
  home-manager.users.davidmy = import ../home.nix { inherit config pkgs lib doom-emacs; };

  programs.zsh.enable = true;

  services.emacs = {
    enable = true;
    package = doom-emacs;
  };

  # darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  # Auto upgrade nix package and the daemon service
  # daemon info: https://github.com/LnL7/nix-darwin/issues/188#issuecomment-626132049
  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
