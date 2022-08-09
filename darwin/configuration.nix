{ config, pkgs, lib, ... }:

let
  isWorkLaptop = true;
in

{
  imports = [ <home-manager/nix-darwin> ];

  users.users = {
    david.home = "/Users/david";
  };

  home-manager.users = {
    david = import ../home.nix;
  };

  programs.zsh.enable = false;
  programs.fish.enable = true;

  services.lorri.enable = true;

  # darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  # Auto upgrade nix package and the daemon service
  # daemon info: https://github.com/LnL7/nix-darwin/issues/188#issuecomment-626132049
  services.nix-daemon.enable = true;
  nix.package = pkgs.nixFlakes;

  nix.extraOptions = "experimental-features = nix-command flakes";

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
      "trash"
      "ffmpeg"
      "thefuck"
      "semgrep"
      "espanso"
      "exercism"
      "sharewifi"
    ];

    casks = [
      "karabiner-elements"
      "beekeeper-studio"
      "chromium"
      "phoenix"
      "raycast"
      "iina"

      "font-fantasque-sans-mono"
      "font-jetbrains-mono"
      "font-iosevka-ss09"
      "font-iosevka-aile"
      "font-ia-writer-mono"
      "font-ia-writer-duo"
      "font-ia-writer-quattro"
      "font-cooper-hewitt"
      "font-comic-mono"
      "font-inter"
    ];

    extraConfig = ''
      brew "emacs-plus", args:["with-native-comp", "with-elrumo2-icon"]
    '';
  };
}
