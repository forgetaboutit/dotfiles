{pkgs, ...}: {
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    zsh
    wget
    git
    lazygit
    htop
    btop

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

    # Neovim: necessary for treesitter grammars, LSP and lua-rocks
    gopls
    golangci-lint

    #gcc_multi
    #gnumake
    #cmake
    #luajit
    #luajitPackages.luarocks-nix

    #lua-language-server
    alejandra
  ];

  programs.zsh.enable = true;
  # globally default to zsh
  users.defaultUserShell = pkgs.zsh;

  # allow proprietary software
  nixpkgs.config.allowUnfree = true;
}
