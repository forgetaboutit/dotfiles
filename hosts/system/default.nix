{
  pkgs,
  lib,
  ...
}:
with lib; {
  environment.systemPackages = with pkgs; [
    wget
    curl
    git

    lua
    wezterm

    # nix formatting tool
    alejandra
    firefox
  ];

  programs.gamemode.enable = true;

  programs.steam = {
    enable = true;
    extest.enable = true;
    extraCompatPackages = with pkgs; [
      proton-ge-bin
    ];
  };

  imports = [
    ./fonts.nix
    ./gnome.nix
    ./shell.nix
  ];
}
