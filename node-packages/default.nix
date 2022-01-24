{ pkgs ? import <nixpkgs> { } }:

let
  inherit (pkgs) lib;
  nodejs = pkgs.nodejs-16_x;
  super = import ./composition.nix { inherit pkgs; };

  self = super // {
    mercury-parser = super."@postlight/mercury-parser".override {
      buildInputs = [ nodejs ];
      postInstall = ''
        exe=$out/bin/mercury-parser
        mkdir -p $out/bin
        cat >$exe <<EOF
        #!${pkgs.runtimeShell}
        exec -a mercury-parser ${nodejs}/bin/node $out/lib/node_modules/@postlight/mercury-parser/cli.js "\$@"
        EOF
        chmod a+x $exe
      '';
    };

    single-file =
      super."single-file-git+https://github.com/gildas-lormeau/SingleFile.git".override {
        postInstall = ''
          exe=$out/bin/single-file
          mkdir -p $out/bin
          cat >$exe <<EOF
          #!${pkgs.runtimeShell}
          exec -a single-file $out/lib/node_modules/single-file/cli/single-file "\$@"
          EOF
          chmod a+x $exe
        '';
        # ^ TODO use pkgs.chromium on linux
      };
  };
in self
