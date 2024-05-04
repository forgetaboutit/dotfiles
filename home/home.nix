{pkgs, ...}: let
  username = "sammy";
  homeDirectory = "/home/${username}";
  configHome = "${homeDirectory}/.config";
  defaultPkgs = with pkgs; [
    acpi
    bottom
    cacert
  ];
in {
  programs = {
    home-manager.enable = true;
  };

  imports = builtins.concatMap import [
    ./programs
  ];

  xdg = {
    inherit configHome;
    enable = true;
  };

  home = {
    inherit username homeDirectory;
    stateVersion = "23.11";
    packages = defaultPkgs;
  };

  systemd.user = {
    # restart service on change
    startServices = "sd-switch";

    mounts = {
      storage-box = {
        Unit = {
          Description = "Mount storage-box";
          Wants = "network-online.target";
          After = "network-online.target";
        };

        Mount = {
          What = "u117386@u117386.your-storagebox.de:/";
          Where = "/home/sammy/StorageBox/";
          Type = "fuse.sshfs";
          Options = "x-systemd.automount,_netdev,reconnect,rw,idmap=user,allow_other";
        };
      };
    };
  };

  # notifications about home-manager news
  news.display = "silent";
}
