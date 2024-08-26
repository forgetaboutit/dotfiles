{
  pkgs,
  lib,
  ...
}: {
  environment.systemPackages = with pkgs; [
    wget
    curl
    git

    lua
    # Terminal emulator
    wezterm

    # nix formatting tool
    alejandra
    firefox
  ];

  services.logind = {
    hibernateKey = "ignore";
    hibernateKeyLongPress = "ignore";

    suspendKey = "ignore";
    suspendKeyLongPress = "ignore";
  };

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
    ./hyprland.nix
    ./shell.nix
  ];
}
