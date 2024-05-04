{
  pkgs,
  lib,
  config,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
  ];

  networking.hostName = "tau-19";

  users.users = {
    sammy = {
      isNormalUser = true;
      description = "Sammy";
      extraGroups = ["wheel" "networkmanager"];
      shell = pkgs.zsh;
    };
  };

  services.xserver.videoDrivers = ["nvidia"];

  # Nvidia proprietary driver
  hardware.nvidia = {
    # required
    modesetting.enable = true;

    # disable Nvidia power management
    powerManagement = {
      enable = false;
      finegrained = false;
    };

    # use proprietary driver
    open = false;

    # enable `nvidia-settings` tool
    nvidiaSettings = true;

    # select driver
    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };
}
