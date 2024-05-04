{...}: {
  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    enableCompletion = true;

    shellAliases = {
      l = "ls -l";
      sl = "ls";
    };

    initExtra = ''
      ## Reattach to tmux, if connected over SSH.
      #if [[ "$TMUX" == "" ]]; then
      #  # Attempt to discover a detached session and attach to it;
      #  # Otherwise, create a new session.
      #  WHOAMI=$(whoami)

      #  if tmux has-session -t $WHOAMI 2>/dev/null; then
      #    tmux -2 attach-session -t $WHOAMI
      #  else
      #    tmux -2 new-session -s $WHOAMI
      #  fi
      #fi
    '';
  };
}
