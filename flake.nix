{
  description = "dotfiles for my M1 mac";

  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/release-22.11";
    };
    nixpkgs-unstable = {
      url = "github:nixos/nixpkgs?rev=bbf77421ac51a7c93f5f0f760da99e4dbce614fa";
    };
    home-manager = {
      url = "github:nix-community/home-manager/release-22.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ {
    nixpkgs,
    nixpkgs-unstable,
    home-manager,
    darwin,
    ...
  }: let
    darwin-overlays = [
      # Allow configurations to use pkgs.unstable.<package-name>.
      (final: prev: {
        unstable = import nixpkgs-unstable {
          system = prev.system;
          config.allowUnfree = true;
        };
      })
    ];
  in {
    darwinConfigurations = {
      mbp = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
          {nixpkgs.overlays = darwin-overlays;}
          ./darwin/configuration.nix
          home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.david = import ./home.nix;
          }
        ];
      };
    };
  };
}
