{
  modulesPath,
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    ./disk-config.nix
    ../../modules/ssh-server.nix
  ];

  networking.hostId = "ec5ff910";

  boot.loader.grub = {
    # no need to set devices, disko will add all devices that have a EF02 partition to the list already
    # devices = [ ];
    zfsSupport = true;
    efiSupport = true;
    efiInstallAsRemovable = true;
  };

  boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
  services.openssh.enable = true;

  environment.systemPackages = map lib.lowPrio [
    pkgs.curl
    pkgs.gitMinimal
  ];

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILCGvFKQqGkSt023z2AwXUO0mg3QuPnlwhYa7TbMJ0yH sammy@tau-19"
  ];

  system.stateVersion = "23.11";
}
