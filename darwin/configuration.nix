{ config, pkgs, lib, ... }:

{
  imports = [ <home-manager/nix-darwin> ];
  users.users.davidmy.home = "/Users/davidmy";
  home-manager.useGlobalPkgs = true;
  home-manager.users.davidmy = import ../home.nix;

  nixpkgs.config.allowUnfree = true;

  programs.zsh.enable = true;

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
