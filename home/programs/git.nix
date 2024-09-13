{pkgs, ...}: let
  gitConfig = {
    core = {
      editor = "nvim";
      pager = "diff-so-fancy | less --tabs=4 --quit-if-one-screen --RAW-CONTROL-CHARS --no-init";
    };

    init.defaultBranch = "main";

    color.ui = true;
    # Always run fetch with `--prune`
    fetch.prune = true;
    pull.rebase = true;

    push = {
      # Always push branch to remote branch with the same name
      default = "current";
      autoSetupRemote = true;
    };

    branch = {
      # Automatically track the starting branch
      autosetupmerge = "always";
      # Always perform a rebase on pull, not a merge
      autosetuprebase = "always";
    };
  };
in {
  home.packages = with pkgs.gitAndTools; [
    diff-so-fancy # diff with colors
    tig # diff and commit view
  ];

  programs.git = {
    enable = true;
    userName = "Samuel Schuhmacher";
    userEmail = "post@samuel-schuhmacher.de";

    aliases = {
      co = "checkout";
      cw = "commit -m WIP";
      ci = "commit";
      st = "status";
      sdiff = "diff --staged";
      po = "push origin";
      poff = "push origin --force-with-lease";
      undo = "reset --mixed HEAD^";
      f = "fetch --all --tags --prune --prune-tags --force";
    };

    signing = {
      key = "441CA6ABF67CF1F78B7D789FAF6DF102AF67F697";
    };

    extraConfig = gitConfig;
  };
}
