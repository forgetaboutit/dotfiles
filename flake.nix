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

    # secret management
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # flake building utilities
    flake-utils.url = "github:numtide/flake-utils";

    # convenience modules for hardware-specific quirks
    hardware.url = "github:nixos/nixos-hardware";

    # Nix-managed neovim
    nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    neovim-nightly = {
      url = "github:nix-community/neovim-nightly-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # hyprwm
    hyprland = {
      url = "git+https://github.com/hyprwm/Hyprland?submodules=1";
    };

    # wezterm nightly
    wezterm = {
      url = "github:wez/wezterm?dir=nix";
    };

    # wsl
    wsl = {
      url = "github:nix-community/NixOS-WSL";
    };

    # `.luarc.json` is a configuration file for the Lua language server.
    # We use this flake to generate it from our for our Neovim configuration.
    gen-luarc = {
      url = "github:mrcjkb/nix-gen-luarc-json";
    };

    nixCats.url = "github:BirdeeHub/nixCats-nvim";
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    nixvim,
    disko,
    agenix,
    wezterm,
    wsl,
    nixCats,
    ...
  } @ inputs: let
    inherit (self) outputs;
    lib = nixpkgs.lib // home-manager.lib;
    systems = ["x86_64-linux"];
    # Build a custom Neovim package overlay
    neovim-overlay = import ./hosts/modules/neovim-overlay.nix {inherit inputs;};
    pkgsFor = lib.genAttrs systems (system:
      import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          #inputs.neovim-nightly.overlays.default
          neovim-overlay
        ];
      });
    pkgs = import nixpkgs {
      system = "x86_64-linux";
      overlays = [
        neovim-overlay
      ];
    };
  in {
    inherit lib;

    devShells.x86_64-linux.default = pkgs.mkShell {
      packages = with pkgs; [
        alejandra
        fd
        nh
      ];
    };

    nixosConfigurations = {
      tau-19 = lib.nixosSystem {
        specialArgs = {
          inherit inputs outputs;
        };
        modules = [
          agenix.nixosModules.default
          ./modules/neovim.nix
          ./hosts/configuration.nix
          ./hosts/users/tau-19
          ./hosts/modules/shell.nix
        ];
      };

      muhbaasu = lib.nixosSystem {
        specialArgs = {
          inherit inputs outputs;
        };
        modules = [
          agenix.nixosModules.default
          ./hosts/users/muhbaasu
        ];
      };

      muhbaasu-bootstrap = lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          disko.nixosModules.disko
          agenix.nixosModules.default
          ./hosts/bootstrap/muhbaasu/configuration.nix
        ];
      };

      wsl = lib.nixosSystem {
        specialArgs = {
          inherit inputs outputs;
        };
        system = "x86_64-linux";
        modules = [
          ./hosts/users/wsl
          ./hosts/modules/shell.nix
          wsl.nixosModules.wsl
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
          nixvim.homeManagerModules.nixvim
          ./home/home.nix
        ];
      };

      "wsl" = lib.homeManagerConfiguration {
        pkgs = pkgsFor.x86_64-linux;
        extraSpecialArgs = {
          inherit inputs outputs;
        };
        modules = [
          nixvim.homeManagerModules.nixvim
          ./home/wsl.nix
        ];
      };
    };
  };
}
