{
  pkgs,
  config,
  inputs,
  ...
}: let
  terminal = "wezterm";
  inherit (config.lib.formats.rasi) mkLiteral;
in {
  imports = [
    inputs.hyprland.homeManagerModules.default
  ];

  wayland = {
    windowManager = {
      hyprland = {
        enable = true;
        plugins = [];

        systemd = {
          enable = true;
        };

        xwayland = {
          enable = false;
        };

        settings = {
          "$mod" = "SUPER";

          general = {
            gaps_in = 7;
            gaps_out = 7;
            border_size = 1;

            allow_tearing = true;
            resize_on_border = true;
          };

          input = {
            kb_layout = "us";
            kb_variant = "alt-intl";
            kb_options = "caps:escape";
            numlock_by_default = true;
          };

          misc = {
            disable_hyprland_logo = true;
            force_default_wallpaper = 1;
          };

          decoration = {
            rounding = 10;
          };

          cursor = {
            no_hardware_cursors = true;
            default_monitor = "DP-2";
          };

          # Nvidia compatibility
          env = [
            "LIBVA_DRIVER_NAME,nvidia"
            "XDG_SESSION_TYPE,wayland"
            "GBM_BACKEND,nvidia-drm"
            "__GLX_VENDOR_LIBRARY_NAME,nvidia"
            "NVD_BACKEND,direct"
          ];

          monitor = [
            "DP-1,2560x1440@144,2560x0,1"
            "DP-2,2560x1440@144,0x0,1"
            "DP-3,2560x1440@144,-2560x0,1"
          ];

          bindd = [
            # Window management
            "$mod,Q,Close the active window,killactive,"
            "$mod,H,Focuses the workspace to the left,movefocus,l"
            "$mod,L,Focuses the workspace to the right,movefocus,r"
            "$mod&Shift_L,H,Moves the current window to the workspace to the left,movewindow,l"
            "$mod&Shift_L,L,Moves the current window to the workspace to the right,movewindow,r"
            "$mod,K,Toggles the focused window's fullscreen state,fullscreen,1"
            "$mod,1,Focuses monitor 1,focusmonitor,2"
            "$mod,2,Focuses monitor 2,focusmonitor,1"
            "$mod,3,Focuses monitor 3,focusmonitor,0"

            # Terminal
            "$mod,Return,Open a terminal,exec,${terminal}"

            # Rofi
            "SUPER,D,Open the rofi menu,exec,rofi -show drun"
          ];

          layerrule = [
            # No annoying resize animations while searching rofi
            "noanim,^(rofi)$"
          ];
        };
      };
    };
  };

  programs = {
    rofi = {
      enable = true;
      inherit terminal;

      package = pkgs.rofi-wayland;
      cycle = true;
      location = "center";

      # Theme adapted from
      # https://github.com/newmanls/rofi-themes-collection/blob/c8239a45edced3502894e1716a8b661fdea8f1c9/themes/rounded-common.rasi
      theme = {
        "*" = {
          # Colors
          bg0 = mkLiteral "#212121F2";
          bg1 = mkLiteral "#2A2A2A";
          bg2 = mkLiteral "#3D3D3D80";
          bg3 = mkLiteral "#1A73E8F2";
          fg0 = mkLiteral "#E6E6E6";
          fg1 = mkLiteral "#FFFFFF";
          fg2 = mkLiteral "#969696";
          fg3 = mkLiteral "#3D3D3D";

          font = "Montserrat 9";

          background-color = mkLiteral "transparent";
          text-color = mkLiteral "@fg0";

          margin = mkLiteral "0px";
          padding = mkLiteral "0px";
          spacing = mkLiteral "0px";
        };

        window = {
          location = mkLiteral "center";
          width = 480;
          border-radius = mkLiteral "24px";

          background-color = mkLiteral "@bg0";
        };

        mainbox = {
          padding = mkLiteral "12px";
        };

        inputbar = {
          background-color = mkLiteral "@bg1";
          border-color = mkLiteral "@bg1";

          border = mkLiteral "2px";
          border-radius = mkLiteral "16px";

          padding = mkLiteral "8px 16px";
          spacing = mkLiteral "8px";
          children = mkLiteral "[ prompt, entry ]";
        };

        prompt = {
          text-color = mkLiteral "@fg2";
        };

        entry = {
          placeholder = "Search";
          placeholder-color = mkLiteral "@fg3";
        };

        message = {
          margin = mkLiteral "12px 0 0";
          border-radius = mkLiteral "16px";
          border-color = mkLiteral "@bg2";
          background-color = mkLiteral "@bg2";
        };

        textbox = {
          padding = mkLiteral "8px 24px";
        };

        listview = {
          background-color = mkLiteral "transparent";

          margin = mkLiteral "12px 0 0";
          lines = 8;
          columns = 1;

          fixed-height = false;
        };

        element = {
          padding = mkLiteral "8px 16px";
          spacing = mkLiteral "8px";
          border-radius = mkLiteral "16px";
        };

        "element normal active" = {
          text-color = mkLiteral "@bg3";
        };

        "element alternatve active" = {
          text-color = mkLiteral "@bg3";
        };

        "element selected normal, element selected active" = {
          background-color = mkLiteral "@bg3";
        };

        element-icon = {
          size = mkLiteral "1em";
          vertical-align = mkLiteral "0.5";
        };

        element-text = {
          text-color = mkLiteral "inherit";
        };
      };
    };
  };
}
