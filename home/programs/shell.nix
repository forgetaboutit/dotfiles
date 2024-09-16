{
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

    zoxide = {
      enable = true;
      enableZshIntegration = true;
    };

    starship = {
      enable = true;
      enableZshIntegration = true;

      settings = {
        add_newline = false;
      };
    };
  };
}
