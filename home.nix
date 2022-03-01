{ config, pkgs, lib, ... }:

# inspo: https://github.com/gvolpe/nix-config

let
  sources = import nix/sources.nix;
  emacs-overlay = import sources.emacs-overlay;
  pkgs = import sources.nixpkgs-unstable { };
  stable-pkgs = import sources.nixpkgs { overlays = [ emacs-overlay ]; };

  # TODO move into sources?
  node-packages = import ./node-packages {};
  archiveboxPkgs = [
  #   pkgs.archivebox # broken in unstable and doesn't exist in stable
    node-packages.single-file
    node-packages.mercury-parser
  ];

  my-scripts = import ./scripts {
    pkgs = pkgs;
    isWorkLaptop = isWorkLaptop;
  };

  isDarwin = pkgs.stdenv.isDarwin;
  isWorkLaptop = (builtins.getEnv "USER") == "davidmy";

  archiveboxOutputDir = if isWorkLaptop then "" else (builtins.getEnv "HOME") + "/files/Documents/ArchiveBox";
in

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home = {
    username = builtins.getEnv "USER";
    homeDirectory = builtins.getEnv "HOME";

    sessionVariables = {
      LS_COLORS =
        "di=1;34:ln=36:so=32:pi=33:ex=1;32:bd=34;46:cd=35;47:su=30;41:sg=30;46:tw=30;42:ow=1;34";
      ANDROID_SDK_ROOT = "$HOME/Library/Android/sdk";
      GLAMOUR_STYLE = "light";
      EDITOR = "emacsclient -q -c -a ''";
      NOTES_DIR =
        if isWorkLaptop then "$HOME/Tresors/Documents/notes/work" else "";
      RIPGREP_CONFIG_PATH = "$HOME/.config/ripgrep.conf";
      ACHIVEBOX_OUTPUT_DIR = archiveboxOutputDir;

      JUST_SUPPRESS_DOTENV_LOAD_WARNING =
        "1"; # temporary: https://github.com/casey/just/issues/469
    };

    sessionPath = [
      "$HOME/.bin"
      "$HOME/.deta/bin"
      "$HOME/.cargo/bin"
      "$HOME/.local/bin"
      "$HOME/.config/emacs/bin"
      "$HOME/google-cloud-sdk/bin" # needs to be installed 'the google way' to use alpha features
      "$HOME/Library/Android/sdk/emulator" # this needs to be before /tools
      "$HOME/Library/Android/sdk/tools"
      "$HOME/Library/Android/sdk/platform-tools"
    ];

    packages = my-scripts ++ archiveboxPkgs ++ (with pkgs; [
      any-nix-shell
      cachix
      nixfmt
      niv

      stable-pkgs.visidata

      stable-pkgs.ripgrep
      magic-wormhole
      git-crypt
      moreutils
      watchexec
      tealdeer
      hadolint
      stable-pkgs.emacsGcc
      neovim
      stable-pkgs.httpie
      restic
      reflex
      ispell
      sqlite
      choose
      watch
      tree
      just
      glow
      pass
      nnn
      pup
      xsv
      jiq
      oil
      jq
      yq
      sd
      fd

      pandoc
      stable-pkgs.tectonic

      minikube

      rustup
      cargo-edit
      rust-script
      rust-analyzer

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
      # package = pkgs.starship.overrideDerivation (attrs: {
      #   cargoBuildFlags = [ "--features notify-rust" ];
      # });

      enable = true;
      enableZshIntegration = false;
    };

    zoxide.enable = true;

    fzf = {
      enable = true;
      defaultCommand = "fd --type f";
      fileWidgetCommand = "fd --type f";
    };

    fish = {
      enable = true;

      shellAliases = {
        ls = "echo; ${pkgs.exa}/bin/exa -F";
        r = "glow -p README.md 2>/dev/null || echo 'no readme :('";
        #archivebox = "OUTPUT_DIR=${archiveboxOutputDir} ${pkgs.archivebox}/bin/archivebox";
      };

      shellAbbrs = {
        q = "exit";
        la = "ls -a";
        ll = "ls -alh";
        gs = "git status";
        gb = "git branch";
        gl = "git log --oneline -n 10";
        tf = "terraform";
        k = "kubectl";
        kx = "kubectx";
        kns = "kubens";
        kdebug =
          "kubectl run -i --rm --tty debug --image=praqma/network-multitool --restart=Never -- sh";
        unset-context = "kubectl config unset current-context";
        teb = "tmux capture-pane -pS -1000000 | eb";
        rg = "rg -S";
        dig = "dig +short";
        nsn = "nix search nixpkgs";
        uuid = "uuidgen | tr -d '\\n' | tr '[:upper:]' '[:lower:]' | pbcopy";
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

        bind \cj down-or-search
        bind \ck up-or-search

        any-nix-shell fish --info-right | source

        thefuck --alias | source

        test -e ~/.config/fish/secret_work_functions.fish && source ~/.config/fish/secret_work_functions.fish

        source ~/.asdf/asdf.fish
      '';

      functions = {
        kp = ''
          function fish_right_prompt
              test "$SHOW_K8S_PROMPT" = 1; and kubesummary; or nix-shell-info
          end
          test "$SHOW_K8S_PROMPT" = 1; and set -g SHOW_K8S_PROMPT 0; or set -g SHOW_K8S_PROMPT 1
        '';
        # https://github.com/jarun/nnn/blob/fa7c19c4097bfb71eef16060724c5bd8da424a9e/misc/quitcd/quitcd.fish
        n = ''
          # Block nesting of nnn in subshells
          if test -n "$NNNLVL"
              if [ (expr $NNNLVL + 0) -ge 1 ]
                  echo "nnn is already running"
                  return
              end
          end

          # The behaviour is set to cd on quit (nnn checks if NNN_TMPFILE is set)
          # To cd on quit only on ^G, remove the "-x" as in:
          #    set NNN_TMPFILE "$XDG_CONFIG_HOME/nnn/.lastd"
          #    (or, to a custom path: set NNN_TMPFILE "/tmp/.lastd")
          # or, export NNN_TMPFILE after nnn invocation
          if test -n "$XDG_CONFIG_HOME"
              set -x NNN_TMPFILE "$XDG_CONFIG_HOME/nnn/.lastd"
          else
              set -x NNN_TMPFILE "$HOME/.config/nnn/.lastd"
          end

          # Unmask ^Q (, ^V etc.) (if required, see `stty -a`) to Quit nnn
          # stty start undef
          # stty stop undef
          # stty lwrap undef
          # stty lnext undef

          nnn $argv

          if test -e $NNN_TMPFILE
              source $NNN_TMPFILE
              rm $NNN_TMPFILE
          end
        '';
      };

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
      envExtra = ''
        export PATH="$PATH:/usr/local/bin"
      '';
    };

    tmux = {
      enable = true;
      plugins = with pkgs; [
        tmuxPlugins.yank
        tmuxPlugins.pain-control
        tmuxPlugins.resurrect
        tmuxPlugins.continuum
      ];
      extraConfig = ''
        set-option -g default-command ${pkgs.fish}/bin/fish
        set -g @resurrect-capture-pane-contents 'on'
        run-shell ~/.tmux/plugins/tmux-thumbs/tmux-thumbs.tmux
        ${builtins.readFile dotfiles/dot-tmux.conf}
      '';
    };

    kitty = {
      enable = true;
      settings.shell = "${pkgs.fish}/bin/fish";
      extraConfig = builtins.readFile dotfiles/kitty.conf;
    };
  };

  home.file = {
    ".vimrc".source = dotfiles/dot-vimrc;
    ".lein/profiles.clj".source = dotfiles/lein/profiles.clj;
  };

  xdg.configFile = with lib;
    mkMerge [
      {
        "starship.toml".text =
          ''format = "$directory$git_branch$line_break$cmd_duration$character"''
          + builtins.readFile dotfiles/starship.toml;
        "starship-with-gcloud.toml".text = ''
          format = "$gcloud$line_break$directory$git_branch$line_break$cmd_duration$character"''
          + builtins.readFile dotfiles/starship.toml;
        "doom".source = dotfiles/doom;
        "git".source = dotfiles/git;
        "espanso".source = dotfiles/espanso;
        "nvim".source = dotfiles/nvim;
        "ripgrep.conf".source = dotfiles/ripgrep.conf;
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
