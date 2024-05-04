{
  description = "Sammy's NixOS system flake";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    # Home manager
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # flake building utilities
    flake-utils.url = "github:numtide/flake-utils";

    # convenience modules for hardware-specific quirks
    hardware.url = "github:nixos/nixos-hardware";

    # Nix-managed neovim
    #nixneovim = {
    #  url = "github:nixneovim/nixneovim";
    #  inputs.nixpkgs.follows = "nixpkgs";
    #};
    nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    neovim-nightly = {
      url = "github:nix-community/neovim-nightly-overlay";
    };
  };

  outputs = {
    self,
    nixpkgs,
    hardware,
    home-manager,
    flake-utils,
    #nixneovim,
    nixvim,
    ...
  } @ inputs: let
    inherit (self) outputs;
    lib = nixpkgs.lib // home-manager.lib;
    systems = ["x86_64-linux"];
    forEachSystem = f: lib.genAttrs systems (system: f pkgsFor.${system});
    pkgsFor = lib.genAttrs systems (system:
      import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          #nixneovim.overlays.default
          inputs.neovim-nightly.overlay
          #nixvim.nixosModules.nixvim
        ];
      });
  in {
    inherit lib;

    nixosConfigurations = {
      tau-19 = lib.nixosSystem {
        specialArgs = {
          inherit inputs outputs;
        };
        modules = [
          ./hosts/configuration.nix
          ./hosts/users/tau-19
        ];
      };
    };

    homeConfigurations = {
      "sammy@tau-19" = lib.homeManagerConfiguration {
        pkgs = pkgsFor.x86_64-linux;
        extraSpecialArgs = {
          inherit inputs outputs;
        };
        modules = [
          #nixneovim.nixosModules.default
          nixvim.homeManagerModules.nixvim
          ./home/home.nix
        ];
      };
    };
  };
}
