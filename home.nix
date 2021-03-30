{ config, pkgs, lib, ... }:

let
  my-scripts = import ./scripts pkgs;

  doom-emacs = pkgs.callPackage (builtins.fetchTarball {
    url = "https://github.com/vlaci/nix-doom-emacs/archive/master.tar.gz";
  }) { doomPrivateDir = ./dotfiles/doom; };

  gccemacs = (import (pkgs.fetchFromGitHub {
    owner = "twlz0ne";
    repo = "nix-gccemacs-darwin";
    rev = "6e58775e7eddfe4b3a2130029346f11f23d677b1";
    sha256 = "1dnyyz2jikvp28l4ayrgc9mvaivh42fndgy7sg7yxybgnslr2gqk";
  })).emacsGccDarwin;
in

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home = {
    username = builtins.getEnv "USER";
    homeDirectory = builtins.getEnv "HOME";

    sessionVariables = {
      ANDROID_SDK_ROOT = "$HOME/Library/Android/sdk";
      GLAMOUR_STYLE = "light";
    };

    sessionPath = [
      "$HOME/.local/bin"
      "$HOME/.emacs.d/bin"
      "$HOME/Library/Android/sdk/emulator"
      "$HOME/Library/Android/sdk/tools"
      "$HOME/Library/Android/sdk/tools/bin"
      "$HOME/Library/Android/sdk/platform-tools"
    ];

    packages = my-scripts ++ (with pkgs; [
      cachix
      nixfmt
      niv

      # doom-emacs
      gccemacs
      git-crypt
      ripgrep
      restic
      reflex
      ngrok
      just
      glow
      pup
      xsv

      pandoc
      tectonic

      google-cloud-sdk

      rustup

      gopls
      golint

      yarn
      nodePackages.nodemon
      nodePackages.eslint
      nodePackages.eslint_d
      nodePackages.prettier
      nodePackages.typescript
      nodePackages.typescript-language-server
    ]);
  };

  # TODO: clone doom to .config/emacs
  xdg.configFile."doom".source = dotfiles/doom;

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

    zoxide.enable = true;
    fzf.enable = true;

    zsh = {
      enable = true;
      enableAutosuggestions = true;
      plugins = [{
        name = "zsh-history-substring-search";
        file = "zsh-history-substring-search.zsh";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-history-substring-search";
          rev = "v1.0.2";
          sha256 = "0y8va5kc2ram38hbk2cibkk64ffrabfv1sh4xm7pjspsba9n5p1y";
        };
      }];
      sessionVariables = { ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE = "underline"; };
      initExtra = ''
        alias ls='echo; ${pkgs.exa}/bin/exa'
        bindkey '^[[A' history-substring-search-up
        bindkey '^[[B' history-substring-search-down
        [[ -e $HOME/.asdf/asdf.sh ]] && . $HOME/.asdf/asdf.sh
        ${builtins.readFile dotfiles/dot-zshrc}
      '';
    };

    tmux = {
      enable = true;
      plugins = with pkgs; [ tmuxPlugins.yank tmuxPlugins.pain-control ];
      extraConfig = ''
        ${builtins.readFile dotfiles/dot-tmux.conf}
      '';
    };

    kitty = {
      enable = true;
      extraConfig = ''
        shell ${pkgs.zsh}/bin/zsh -c ${pkgs.tmux}/bin/tmux
        ${builtins.readFile dotfiles/kitty.conf}
      '';
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
