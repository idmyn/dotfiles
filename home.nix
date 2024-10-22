{
  config,
  pkgs,
  lib,
  ...
}:
let
  my-scripts = import ./scripts { pkgs = pkgs; };

  # https://github.com/ncfavier/config/blob/954cbf4f569abe13eab456301a00560d82bd0165/modules/nix.nix#L12-L14
  # https://github.com/nix-community/home-manager/issues/676#issuecomment-1595827318
  configPath = "/Users/david/.config/home-manager";
  mkMutableSymlink =
    path:
    config.lib.file.mkOutOfStoreSymlink (configPath + lib.removePrefix (toString ./.) (toString path));
in
{
  home = {
    username = "david";
    homeDirectory = "/Users/david";
    stateVersion = "23.05";
    sessionVariables = {
      LS_COLORS = "di=1;34:ln=36:so=32:pi=33:ex=1;32:bd=34;46:cd=35;47:su=30;41:sg=30;46:tw=30;42:ow=1;34";
      GLAMOUR_STYLE = "light";
      #EDITOR = "emacsclient -q -c -a ''";
      VISUAL = "zed";
      RIPGREP_CONFIG_PATH = "$HOME/.config/ripgrep.conf";
      JUST_SUPPRESS_DOTENV_LOAD_WARNING = "1";
      LDFLAGS = "-L/usr/local/opt/python@3.10/lib"; # for x86_64 homebrew python
      KALEIDOSCOPE_DIR = "$HOME/src/personal/kaleidoscope";
      PNPM_HOME = "$HOME/.pnpm-bin";
      HUSKY = "0";
      ASDF_GOLANG_MOD_VERSION_ENABLED = "true";
    };
    sessionPath = [
      "$HOME/.nix-profile/bin"
      "/nix/var/nix/profiles/default/bin"
      "$HOME/.pnpm-bin"
      "$HOME/.local/bin" # for pipx
      "$HOME/.deno/bin"
      "/opt/homebrew/bin"
      "$HOME/.config/emacs/bin"
      "$HOME/google-cloud-sdk/bin"
      "$HOME/.orbstack/bin"
      "/usr/local/opt/python@3.10/bin" # for x86_64 homebrew
      "$KALEIDOSCOPE_DIR/bin"
      "/Applications/Emacs.app/Contents/MacOS"
      "/Applications/Emacs.app/Contents/MacOS/bin"
      "/Applications/IntelliJ IDEA CE.app/Contents/MacOS"
    ];

    packages =
      my-scripts
      ++ (with pkgs; [
        any-nix-shell
        #nixfmt
        nixfmt-rfc-style
        nixd
        niv

        poetry

        #visidata
        ripgrep
        #magic-wormhole
        diff-so-fancy # TODO fancydiff script = `diff -u file_a file_b | diff-so-fancy`
        sqlite-utils
        imagemagick
        shellcheck
        lazydocker
        git-crypt
        prettierd
        moreutils
        libgccjit
        watchexec
        exercism
        tealdeer
        lazygit
        jujutsu
        neovide
        zellij
        neovim
        httpie
        restic
        reflex
        ispell
        sqlite
        choose
        cmake
        gnupg
        watch
        htmlq
        dasel
        helix
        navi
        tree
        just
        yazi
        glow
        pass
        gitu
        nnn
        llm
        pup
        xsv
        jiq
        jq
        yq-go
        sd
        fd
        fx

        # JVM/Scala installed through sdkman
        metals

        golangci-lint

        rustup

        pandoc
        tectonic

        turso-cli
        flyctl
        heroku
      ]);
  };
  programs.home-manager.enable = true;

  programs = {
    direnv.enable = true;

    starship = {
      # package = pkgs.starship.overrideDerivation (attrs: {
      #   cargoBuildFlags = [ "--features notify-rust" ];
      # });

      enable = true;
      enableZshIntegration = false;
      enableNushellIntegration = true;
    };

    zoxide.enable = true;

    fzf = {
      enable = true;
      defaultCommand = "fd --type f";
      fileWidgetCommand = "fd --type f";
    };

    atuin = {
      enable = true;
      flags = [ "--disable-up-arrow" ];
      settings = {
        enter_accept = false;
      };
      enableNushellIntegration = true;
    };

    nushell = {
      # https://github.com/taotien/NOflake/blob/9b1d4d53c2dee74189ad24c5a8da054df47d6112/users/tao/HOME.nix#L37-L42
      # https://github.com/nushell/nushell/issues/9617#issuecomment-1707184218
      enable = true;
      # extraConfig = ''
      #   use /Users/david/src/clones/nu_scripts/themes/nu-themes/solarized-light.nu
      #   $env.config.color_config = (solarized-light)
      # '';
    };

    fish = {
      enable = true;

      shellAliases = {
        ls = "echo; ${pkgs.eza}/bin/eza -F";
        r = "glow -p README.md 2>/dev/null || echo 'no readme :('";
        confetti = "open raycast://confetti";
        nu = "${pkgs.nushell}/bin/nu";
      };

      shellAbbrs = {
        q = "exit";
        la = "ls -a";
        ll = "ls -alh";
        gs = "git status";
        gb = "git branch";
        gl = "git log --oneline -n 10";
        tf = "terraform";
        llm = "llm -t concise";
        k = "kubectl";
        kx = "kubectx";
        kns = "kubens";
        kdebug = "kubectl run -i --rm --tty debug --image=praqma/network-multitool --restart=Never -- sh";
        unset-context = "kubectl config unset current-context";
        teb = "tmux capture-pane -pS -1000000 | eb";
        rg = "rg -S";
        dig = "dig +short";
        nsn = "nix search nixpkgs";
        uuid = "uuidgen | tr -d '\\n' | tr '[:upper:]' '[:lower:]' | pbcopy";
        bks = "open -a 'Beekeeper Studio'";
        jiq = "jiq -q";
        prod-diff = "git fetch && git log (heroku releases -n 1 -a surfboard-app-prod --json | jq -r '.[].description' | choose 1)..origin/main --oneline";
        drs = "darwin-rebuild switch --flake path:$HOME/.config/nixpkgs#mbp";
        zj = "zellij";
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

        test -e ~/.config/fish/secret_work_functions.fish && source ~/.config/fish/secret_work_functions.fish

        source ~/.asdf/asdf.fish
        source ~/.asdf/plugins/golang/set-env.fish
        source ~/.zellij.fish

        test -e /opt/homebrew/Caskroom/miniforge/base/bin/conda && eval /opt/homebrew/Caskroom/miniforge/base/bin/conda "shell.fish" "hook" $argv | source

        test -e /opt/homebrew/bin/pyenv && pyenv init - | source
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
        {
          name = "sdkman-for-fish";
          src = pkgs.fetchFromGitHub {
            owner = "reitzig";
            repo = "sdkman-for-fish";
            rev = "555203d56e534d91cde87ad600cbbf6f2d112a03";
            sha256 = "7cgyR3hQ30Jv+9lJS5qaBvSaI/0YVT8xPXlUhDBTdFc=";
          };
        }
      ];
    };

    zsh = {
      enable = true;
      autosuggestion.enable = true;
      plugins = [
        {
          name = "zsh-history-substring-search";
          file = "zsh-history-substring-search.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "zsh-users";
            repo = "zsh-history-substring-search";
            rev = "v1.0.2";
            sha256 = "0y8va5kc2ram38hbk2cibkk64ffrabfv1sh4xm7pjspsba9n5p1y";
          };
        }
      ];
      sessionVariables = {
        ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE = "underline";
      };
      initExtra = ''
        alias ls='echo; ${pkgs.eza}/bin/eza'
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
        tmuxPlugins.tmux-thumbs
      ];
      extraConfig = ''
        set-option -g default-command ${pkgs.fish}/bin/fish
        set -g @resurrect-capture-pane-contents 'on'
        ${builtins.readFile dotfiles/dot-tmux.conf}
      '';
    };

    kitty = {
      enable = false;
      settings.shell = "${pkgs.fish}/bin/fish";
      extraConfig = builtins.readFile dotfiles/kitty.conf;
    };

    micro = {
      enable = true;
      settings.colorscheme = "simple";
    };
  };

  home.file = lib.mkMerge [
    {
      ".vimrc".source = dotfiles/dot-vimrc;
      ".lein/profiles.clj".source = dotfiles/lein/profiles.clj;
      ".asdfrc".text = "legacy_version_file = yes";
      ".sdkman/etc/config".source = dotfiles/sdkman-config;
    }
    (lib.mkIf pkgs.stdenv.isDarwin {
      ".phoenix.js".text = ''
        const pathToShellInNixStore = "${pkgs.fish}/bin/fish"
        ${builtins.readFile dotfiles/macOS/phoenix.js}
      '';
    })
  ];

  xdg.configFile =
    with lib;
    mkMerge [
      {
        "starship.toml".text =
          ''format = "$directory$git_branch$line_break$cmd_duration$character"''
          + builtins.readFile dotfiles/starship.toml;
        # export STARSHIP_CONFIG=$HOME/.config/starship-with-gcloud.toml
        "starship-with-gcloud.toml".text =
          ''format = "$directory$git_branch$line_break$cmd_duration$gcloud$character"''
          + builtins.readFile dotfiles/starship.toml;
        "kitty/kitty.conf".text =
          ''
            shell ${pkgs.fish}/bin/fish
          ''
          + builtins.readFile dotfiles/kitty.conf;
        "doom".source = mkMutableSymlink dotfiles/doom;
        "wezterm".source = mkMutableSymlink dotfiles/wezterm;
        "git".source = dotfiles/git;
        "espanso".source = dotfiles/espanso;
        # "nvim".source = dotfiles/nvim;
        "nvim/init.lua".text = ''
          vim.cmd 'colorscheme eink'
        '';
        "ripgrep.conf".source = dotfiles/ripgrep.conf;
        "helix".source = dotfiles/helix;
        "yazi".source = dotfiles/yazi;
        "zellij".source = dotfiles/zellij;
        "gitu/config.toml".text = ''
          [bindings]
          root.discard = ["x"]
          root.quit = ["q"]
          rebase_menu.rebase_continue = ["r"]
        '';
        "zed/settings.json".source = dotfiles/zed/settings.json;
        "zed/keymap.json".source = dotfiles/zed/keymap.json;
        "zed/tasks.json".source = dotfiles/zed/tasks.json;
        "zed/themes/eink.json".source = dotfiles/zed/themes/eink.json;
        "ideavim/ideavimrc".source = dotfiles/intellij/ideavimrc;
      }
      (mkIf pkgs.stdenv.isDarwin { "karabiner.edn".source = dotfiles/macOS/karabiner.edn; })
    ];

  home.activation.installAsdfVm = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    [ ! -d $HOME/.asdf ] && \
      $DRY_RUN_CMD ${pkgs.git}/bin/git clone --branch v0.14.0 $VERBOSE_ARG \
        https://github.com/asdf-vm/asdf.git ~/.asdf;
  '';

  home.activation.generateZellijCompletions = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
    $DRY_RUN_CMD ${pkgs.zellij}/bin/zellij setup --generate-completion fish > ~/.zellij.fish
  '';
}
