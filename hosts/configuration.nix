{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
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

  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
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

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
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
  ];

  # globally default to zsh
  users.defaultUserShell = pkgs.zsh;

  # allow proprietary software
  nixpkgs.config.allowUnfree = true;

  system.stateVersion = "23.11";
}
