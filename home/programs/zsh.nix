{...}: {
  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    enableCompletion = true;

    shellAliases = {
      l = "ls -l";
      sl = "ls";
      # Use zoxide for directory navigation
      cd = "z";
    };

    history = {
      expireDuplicatesFirst = true;
      # Save timestamps
      extended = true;
      ignoreDups = true;
      ignoreAllDups = true;
      ignoreSpace = true;
      size = 5000;
      share = true;
    };

    # zsh uses scancodes for key bindings. Hint: `sudo showkey -a` shows the
    # currently pressed keys. Alternatively, the Arch wiki is a good source
    # on additional info: https://wiki.archlinux.org/title/Keyboard_input
    initExtra = ''
      # Ctrl + {Left,Right} navigates between words
      bindkey "^[[1;5D" emacs-backward-word
      bindkey "^[[1;5C" emacs-forward-word

      # Unfortunately, those don't work as of now yet.
      # {Alt,Shift} + {Left,Right} shouldn't do anything for now
      bindkey "^[[1;2D" undefined-key
      bindkey "^[[1;2C" undefined-key
      bindkey "^[[1;3D" undefined-key
      bindkey "^[[1;3C" undefined-key
    '';
  };
}
