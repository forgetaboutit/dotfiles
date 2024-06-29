{pkgs, ...}: let
in {
  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
  };

  environment.systemPackages = with pkgs; [
    rofi-wayland
    kitty
  ];

  environment.sessionVariables.NIXOS_OZONE_WL = "1";
}
