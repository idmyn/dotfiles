{ config, pkgs, lib, ... }:

let
  isWorkLaptop = (builtins.getEnv "USER") == "davidmy";
in

{
  imports = [ <home-manager/nix-darwin> ];

  users.users = (if isWorkLaptop then {
    davidmy.home = "/Users/davidmy";
  } else {
    david.home = "/Users/david";
  });

  home-manager.users = (if isWorkLaptop then {
    davidmy = import ../home.nix;
  } else {
    david = import ../home.nix;
  });

  programs.zsh.enable = false;
  programs.fish.enable = true;

  services.lorri.enable = true;

  # darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  # Auto upgrade nix package and the daemon service
  # daemon info: https://github.com/LnL7/nix-darwin/issues/188#issuecomment-626132049
  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  homebrew = {
    enable = true;
    taps = [
      "yqrashawn/goku"
      "homebrew/cask-fonts"
      "d12frosted/emacs-plus"
      "federico-terzi/espanso"
      "bradyjoslin/sharewifi"
    ];

    brews = [
      "git"
      "goku"
      "espanso"
      "sharewifi"
      "tesseract"
      "trash"
    ];

    casks = [
      "sensiblesidebuttons"
      "karabiner-elements"
      "libreoffice"
      "appcleaner"
      "phoenix"
      "raycast"
      "kitty"

      "font-fantasque-sans-mono"
      "font-jetbrains-mono"
      "font-iosevka-ss09"
      "font-iosevka-aile"
    ];

    extraConfig = ''
      brew "emacs-plus@28", args:["with-native-comp", "with-modern-doom3-icon"]
    '';
  };
}
