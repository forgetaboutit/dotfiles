{pkgs, ...}: let
  fontSize = 11;
in {
  programs.alacritty = {
    enable = true;
    settings = {
      selection = {
        save_to_clipboard = true;
      };
    };
  };
}
