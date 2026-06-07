{pkgs, ...}: let
  username = "sammy";
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

  neovim-custom.username-undodir = "sammy";

  imports = builtins.concatMap import [
    ./programs
  ];

  xdg = {
    inherit configHome;
    enable = true;
  };

  home = {
    inherit username homeDirectory;
    stateVersion = "23.11";
    packages = defaultPkgs;
  };

  # notifications about home-manager news
  news.display = "silent";
}
