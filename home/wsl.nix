{pkgs, ...}: let
  username = "nixos";
  homeDirectory = "/home/${username}";
  configHome = "${homeDirectory}/.config";
  defaultPkgs = with pkgs; [
    acpi
    bottom
    cacert
  ];
in {
  programs = {
    home-manager.enable = true;
  };

  imports = [
    ./programs/git.nix
    ./programs/neovim.nix
    ./programs/shell.nix
    ./programs/ssh-client.nix
    ./programs/tmux.nix
    ./programs/zsh.nix
  ];

  neovim-custom.username-undodir = "nixos";

  xdg = {
    inherit configHome;
    enable = true;
  };

  home = {
    inherit username homeDirectory;
    stateVersion = "24.05";
    packages = defaultPkgs;
  };

  # notifications about home-manager news
  news.display = "silent";
}
