{pkgs, ...}: {
  programs.zsh = {
    enable = true;
  };

  programs.starship = {
    enable = true;
    settings = {
      add_newline = false;

      hostname = {
        ssh_only = false;
      };
    };
  };
}
