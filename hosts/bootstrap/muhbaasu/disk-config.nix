{
  disko.devices = {
    disk = {
      disk0 = {
        type = "disk";
        device = "/dev/disk/by-id/nvme-SAMSUNG_MZQLB960HAJR-00007_S437NA0MA05037";
        content = {
          type = "gpt";
          partitions = {
            boot = {
              size = "1M";
              type = "EF02";
            };
            ESP = {
              size = "1G";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
              };
            };
            zfs = {
              size = "100%";
              content = {
                type = "zfs";
                pool = "rpool";
              };
            };
          };
        };
      };
      disk1 = {
        type = "disk";
        device = "/dev/disk/by-id/nvme-SAMSUNG_MZQLB960HAJR-00007_S437NA0MA05038";
        content = {
          type = "gpt";
          partitions = {
            boot = {
              size = "1M";
              type = "EF02";
            };
            ESP = {
              size = "1G";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = null;
              };
            };
            zfs = {
              size = "100%";
              content = {
                type = "zfs";
                pool = "rpool";
              };
            };
          };
        };
      };
    };
    zpool = {
      rpool = {
        type = "zpool";
        mode = "mirror";
        options = {
          autotrim = "on";
          ashift = "13";
        };
        rootFsOptions = {
          acltype = "posixacl";
          canmount = "off";
          compression = "lz4";
          relatime = "on";
          xattr = "sa";
        };

        datasets = {
          local = {
            type = "zfs_fs";
            options.mountpoint = "none";
          };
          safe = {
            type = "zfs_fs";
            options.mountpoint = "none";
          };
          "local/reserved" = {
            type = "zfs_fs";
            options = {
              mountpoint = "none";
              reservation = "10GiB";
            };
          };
          "local/root" = {
            type = "zfs_fs";
            mountpoint = "/";
            options.mountpoint = "legacy";
            postCreateHook = ''
              zfs snapshot rpool/local/root@blank
            '';
          };
          "local/nix" = {
            type = "zfs_fs";
            mountpoint = "/nix";
            options = {
              # No writes on reading
              atime = "off";
              canmount = "on";
              mountpoint = "legacy";
            };
          };
          "local/log" = {
            type = "zfs_fs";
            mountpoint = "/var/log";
            options = {
              mountpoint = "legacy";
            };
          };
          "safe/home" = {
            type = "zfs_fs";
            mountpoint = "/home";
            options = {
              mountpoint = "legacy";
            };
          };
          "safe/postgres" = {
            type = "zfs_fs";
            mountpoint = "/postgres";
            options = {
              # Recommendations from:
              #   https://vadosware.io/post/everything-ive-seen-on-optimizing-postgres-on-zfs-on-linux/
              #   https://wiki.archlinux.org/title/ZFS#Databases
              recordsize = "16k";
              primarycache = "metadata";
              logbias = "throughput";
              mountpoint = "legacy";
            };
          };
        };
      };
    };
  };
}
