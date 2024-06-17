let
  more = {pkgs, ...}: {
    programs = {
      bat.enable = true;

      fzf = {
        enable = true;
        tmux.enableShellIntegration = true;
        defaultCommand = "fd --type file --follow";
      };

      lsd = {
        enable = true;
        enableAliases = true;
      };

      gpg.enable = true;
      jq.enable = true;

      direnv = {
        enable = true;
        enableZshIntegration = true;
        nix-direnv.enable = true;
      };
    };
  };
in [
  ./alacritty.nix
  ./git.nix
  ./neovim.nix
  ./tmux.nix
  ./wezterm.nix
  ./ssh-client.nix
  ./zsh.nix
  more
]
