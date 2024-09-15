{
  pkgs,
  inputs,
  config,
  ...
}: {
  environment.systemPackages = with pkgs; [
    whitesur-cursors
    whitesur-gtk-theme
    whitesur-icon-theme
    gnome.gnome-tweaks
  ];

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;

    # Enable the GNOME Desktop Environment.
    displayManager.gdm.enable = true;
    desktopManager.gnome.enable = true;

    # Enable i3
    windowManager.i3 = {
      enable = true;
      extraPackages = with pkgs; [
        rofi
        i3status
        i3lock
      ];

      package = pkgs.i3-gaps;
    };

    # Configure keymap in X11
    xkb = {
      layout = "us";
      variant = "altgr-intl";
      options = "caps:escape";
    };
  };
}
