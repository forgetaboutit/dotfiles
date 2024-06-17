{
  pkgs,
  lib,
  ...
}: {
  programs.ssh = {
    enable = true;

    matchBlocks = {
      "muhbaasu" = {
        addressFamily = "inet";
        host = "muhbaasu";
        hostname = "muhbaasu.de";
        user = "root";
        port = 22;
      };
    };
  };
}
