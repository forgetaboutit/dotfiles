{pkgs, ...}: let
in {
  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
  };

  environment.systemPackages = with pkgs; [
    rofi-wayland
    kitty
    xdg-desktop-portal
    wev
    mako
    hyprpaper
    wayland-logout
    wayland-protocols
    wayland-utils

  ];

  environment.sessionVariables.NIXOS_OZONE_WL = "1";
}
