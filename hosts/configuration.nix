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

  golandWithPlugins = pkgs.jetbrains.plugins.addPlugins pkgs.jetbrains.goland [
    "github-copilot"
    "ideavim"
    "nixidea"
  ];

  golandExtraPath = with pkgs; [
    go
  ];
  golandExtraLib = [];
  goland = golandWithPlugins.overrideAttrs (attrs: {
    postInstall =
      ''
        mv $out/bin/goland $out/bin/.goland-toolless
        makeWrapper $out/bin/.goland-toolless $out/bin/goland \
          --argv0 goland \
          --prefix PATH : "${lib.makeBinPath golandExtraPath}" \
          --prefix LD_LIBRARY_PATH : "${lib.makeLibraryPath golandExtraLib}"
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
      goland
    ]
    ++ (with pkgs; [
      teamviewer
      protonvpn-gui
      brave
      flutter
      androidStudioPackages.beta
      dotnetCorePackages.sdk_8_0_3xx
      chromium
      alacritty
      spotify
      keepass
      ntfs3g
      nvtopPackages.full

      steam
      discord
    ]);

  # globally default to zsh
  users.defaultUserShell = pkgs.zsh;

  # allow proprietary software
  nixpkgs.config.allowUnfree = true;

  system.stateVersion = "23.11";
}
