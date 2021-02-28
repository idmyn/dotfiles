{ config, pkgs, lib, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home = {
    username = builtins.getEnv "USER";
    homeDirectory = builtins.getEnv "HOME";
    sessionVariables = { SHELL = "${pkgs.fish}/bin/fish"; };
    sessionPath = [ "$HOME/.bin" ]; # TODO move scripts into home-manager

    packages = with pkgs; [
      nixfmt
      cachix
      niv
      restic

      nodejs
      yarn
      nodePackages.typescript-language-server
    ];
  };

  programs = {
    direnv = {
      enable = true;
      enableNixDirenvIntegration = true;
    };

    starship = {
      enable = true;
      settings = {
        format = lib.concatStrings [ "$directory" "$line_break" "$character" ];
        directory = { style = ""; };
        character = {
          success_symbol = "->";
          error_symbol = "->";
        };
      };
    };

    zoxide = { enable = true; };

    fish = {
      enable = true;

      shellAliases = { ls = "echo; exa -F"; };

      shellAbbrs = {
        q = "exit";
        la = "ls -a";
        ll = "ls -alh";
        gs = "git status";
        gb = "git branch";
        gl = "git log --oneline -n 10";
        tf = "terraform";
        k = "kubectl";
        kns = "kubens";
        kdebug =
          "kubectl run -i --rm --tty debug --image=praqma/network-multitool --restart=Never -- sh";
      };

      shellInit = ''
        set fish_greeting
        set -g fish_color_command black
        set -g fish_color_param black
        set -g fish_color_autosuggestion black -u
      '';

      plugins = [{
        name = "autols";
        src = pkgs.fetchFromGitHub {
          owner = "idmyn";
          repo = "fish-autols";
          rev = "d53851d32aaf25c94dde1d02f45ffd9c86d49446";
          sha256 = "0pplqkaq5iycwsr2rcji4hkilcir7y9633qyiqzg9wmpbx102vj0";
        };
      }];
    };
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.05";
}
