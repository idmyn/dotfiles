{ config, pkgs, lib, ... }:

let
  my-scripts = import ./scripts pkgs;

  gccemacs = (import (pkgs.fetchFromGitHub {
    owner = "twlz0ne";
    repo = "nix-gccemacs-darwin";
    rev = "6e58775e7eddfe4b3a2130029346f11f23d677b1";
    sha256 = "1dnyyz2jikvp28l4ayrgc9mvaivh42fndgy7sg7yxybgnslr2gqk";
  })).emacsGccDarwin;

  isDarwin = pkgs.stdenv.isDarwin;
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
      "$HOME/.deta/bin"
      "$HOME/.cargo/bin"
      "$HOME/.local/bin"
      "$HOME/.config/emacs/bin"
      "$HOME/Library/Android/sdk/tools"
      "$HOME/Library/Android/sdk/tools/bin"
      "$HOME/Library/Android/sdk/emulator"
      "$HOME/Library/Android/sdk/platform-tools"
    ];

    packages = my-scripts ++ (with pkgs; [
      cachix
      nixfmt
      niv

      #gccemacs # using homebrew's emacs-plus instead because big-sur weirdness

      git-crypt
      moreutils
      tealdeer
      ripgrep
      restic
      reflex
      ispell
      ngrok
      navi
      tree
      just
      glow
      croc
      pup
      xsv
      jq
      fd

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

  programs = {
    direnv = {
      enable = true;
      # enableNixDirenvIntegration = true;
      # ^ I tried this, which uses https://github.com/nix-community/nix-direnv
      # but I found it messed up my emacs daemon/client setup
      # so I'm using lorri instead, which is configured in ./darwin/configuration.nix
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

    fish = {
      enable = true;

      shellAliases.ls = "echo; ${pkgs.exa}/bin/exa -F";

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
        set -g fish_color_operator black
        set -g fish_color_autosuggestion black -u
        set -g fish_color_redirection black
        set -g fish_color_end black
        set -g fish_color_search_match --reverse

        navi widget fish | source
        source ~/.asdf/asdf.fish
      '';

      functions = {
        kp = ''
          test "$SHOW_K8S_PROMPT" = 1; and set -g SHOW_K8S_PROMPT 0; or set -g SHOW_K8S_PROMPT 1
        '';
        fish_right_prompt = ''
          test "$SHOW_K8S_PROMPT" = 1; and kubesummary
        '';
      };

      plugins = [
        {
          name = "autols";
          src = pkgs.fetchFromGitHub {
            owner = "idmyn";
            repo = "fish-autols";
            rev = "d53851d32aaf25c94dde1d02f45ffd9c86d49446";
            sha256 = "0pplqkaq5iycwsr2rcji4hkilcir7y9633qyiqzg9wmpbx102vj0";
          };
        }
      ];
    };

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
        set-option -g default-command ${pkgs.fish}/bin/fish
        ${builtins.readFile dotfiles/dot-tmux.conf}
      '';
    };

    kitty = {
      enable = true;
      extraConfig = ''
        shell ${pkgs.fish}/bin/fish
        ${builtins.readFile dotfiles/kitty.conf}
      '';
    };
  };

  xdg.configFile = with lib;
    mkMerge [
      {
        "doom".source = dotfiles/doom;
        "git".source = dotfiles/git;
      }
      (mkIf isDarwin {
        "karabiner.edn".source = dotfiles/macOS/karabiner.edn;

        "phoenix/phoenix.js".text = ''
          const pathToShellInNixStore = "${pkgs.fish}/bin/fish"
          ${builtins.readFile dotfiles/macOS/phoenix.js}
        '';
      })
    ];

  # cloning like this because if I clone the repo through a Nix builtin then it's read-only which causes issues
  home.activation.cloneDoomEmacs = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    [ ! -d $HOME/.config/emacs ] && \
      $DRY_RUN_CMD git clone --depth 1 $VERBOSE_ARG \
        https://github.com/hlissner/doom-emacs ~/.config/emacs;
  '';

  home.activation.installAsdfVm = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    [ ! -d $HOME/.asdf ] && \
      $DRY_RUN_CMD git clone --branch v0.8.0 $VERBOSE_ARG \
        https://github.com/asdf-vm/asdf.git ~/.asdf;
  '';

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
