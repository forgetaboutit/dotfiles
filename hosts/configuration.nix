{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  riderWithPlugins = pkgs.jetbrains.plugins.addPlugins pkgs.jetbrains.rider [
    "github-copilot"
    "ideavim"
    "nixidea"
  ];
  riderExtraPath = with pkgs; [
    dotnetCorePackages.sdk_8_0_3xx
    dotnetPackages.Nuget
    mono
    msbuild
  ];
  riderExtraLib = [];
  # Shamelessly stolen from https://huantian.dev/blog/unity3d-rider-nixos/
  rider = riderWithPlugins.overrideAttrs (attrs: {
    postInstall =
      ''
        mv $out/bin/rider $out/bin/.rider-toolless
        makeWrapper $out/bin/.rider-toolless $out/bin/rider \
          --argv0 rider \
          --prefix PATH : "${lib.makeBinPath riderExtraPath}" \
          --prefix LD_LIBRARY_PATH : "${lib.makeLibraryPath riderExtraLib}"
      ''
      + attrs.postInstall or "";
  });
in {
  imports = [
    ./system
  ];

  nix = {
    settings = {
      experimental-features = "nix-command flakes";
      #auto-optimize-storage = true;
      trusted-users = ["root" "sammy" "@wheel"];
    };
  };

  # Bootloader.
  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    supportedFilesystems = ["ntfs zfs"];

    kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
  };

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  i18n = {
    # Select internationalisation properties.
    defaultLocale = "en_US.UTF-8";

    extraLocaleSettings = {
      LC_ADDRESS = "de_DE.UTF-8";
      LC_IDENTIFICATION = "de_DE.UTF-8";
      LC_MEASUREMENT = "de_DE.UTF-8";
      LC_MONETARY = "de_DE.UTF-8";
      LC_NAME = "de_DE.UTF-8";
      LC_NUMERIC = "de_DE.UTF-8";
      LC_PAPER = "de_DE.UTF-8";
      LC_TELEPHONE = "de_DE.UTF-8";
      LC_TIME = "de_DE.UTF-8";
    };
  };

  hardware.graphics = {
    enable = true;
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  virtualisation.podman = {
    enable = true;
    dockerCompat = true;
    dockerSocket = {
      enable = true;
    };
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages =
    [
      inputs.agenix.packages.x86_64-linux.default
      rider
    ]
    ++ (with pkgs; [
      protonvpn-gui
      brave
      flutter
      androidStudioPackages.beta
      dotnetCorePackages.sdk_8_0_3xx
      chromium
      wget
      git
      lazygit
      alacritty
      spotify
      htop
      btop
      keepass
      ntfs3g
      nvtopPackages.full

      ## Rusty system tools
      # better `cat`
      bat
      # better `cat`
      ripgrep
      # better `find`
      fd
      # better `ls`
      lsd
      # show disk usage
      dust
      diskonaut
      # show languages and LoC
      tokei
      # Terminal profiling tool
      hyperfine
      # Show network usage per process
      bandwhich
      # diff tool
      delta

      nmap

      sshfs

      steam
      discord

      ## Better terminal tooling
      # Nice shell
      zsh

      # Fetch-tools showing system info
      pfetch
      neofetch
      fastfetch
      cpufetch

      # Nice & fast terminal prompt
      starship

      # Fast file search
      fzf

      # Tool to show file types
      file

      # Better cd
      zoxide

      # Clipboard for Wayland
      wl-clipboard

      # Clipboard for xserver
      xclip

      # Screenshot utility
      shotman

      # JSON query tool
      jq

      # .zip extractor
      unzip

      ## Programming
      # NodeJS
      #nodejs_21
      inkscape-with-extensions

      # tree-sitter CLI tools
      tree-sitter

      # Rust tooling
      cargo

      # Neovim: necessary for treesitter grammars and lua-rocks
      #gcc_multi
      #gnumake
      #cmake
      #luajit
      #luajitPackages.luarocks-nix

      #lua-language-server
    ]);

  # globally default to zsh
  users.defaultUserShell = pkgs.zsh;

  # allow proprietary software
  nixpkgs.config.allowUnfree = true;

  system.stateVersion = "23.11";
}
