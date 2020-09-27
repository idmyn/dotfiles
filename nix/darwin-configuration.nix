{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    direnv
    niv
  ];

  services.lorri.enable = true; # installs automatically

  # Auto upgrade nix package
  nix.package = pkgs.nix;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
