{
  pkgs,
  lib,
  config,
  ...
}: let
  # We trust local users implicitly for now, but Keycloak wants a password file
  # anyway. Fortunately, an empty file works fine.
  keycloakPasswordFile = pkgs.writeText "keycloak-password-file" '''';
  exporterUrl = exporterName: "localhost:${toString (builtins.getAttr exporterName config.services.prometheus.exporters).port}";
in {
  imports = [
    ./configuration.nix
    ../../modules/ssh-server.nix
    {
      age.secrets.grafana-secrets.file = ../../../secrets/grafana-secrets.age;
    }
  ];

  programs.zsh.enable = true;
  networking.hostName = "muhbaasu";

  networking.hostId = "ec5ff910";
  boot.loader.grub.device = "/dev/disk/by-id/nvme-SAMSUNG_MZQLB960HAJR-00007_S437NA0MA05037";

  users.users = {
    sammy = {
      isNormalUser = true;
      description = "Sammy";
      extraGroups = ["wheel" "networkmanager"];
      shell = pkgs.zsh;
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILCGvFKQqGkSt023z2AwXUO0mg3QuPnlwhYa7TbMJ0yH sammy@tau-19"
      ];
    };
  };

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILCGvFKQqGkSt023z2AwXUO0mg3QuPnlwhYa7TbMJ0yH sammy@tau-19"
  ];

  environment.systemPackages = with pkgs; [
    bat
    bottom
  ];

  networking = {
    nftables.enable = true;
    enableIPv6 = true;
    firewall = {
      enable = true;
      allowPing = true;

      allowedTCPPorts = [
        22
        80
        443
      ];
    };
  };

  security.acme = {
    acceptTerms = true;

    defaults = {
      email = "letsencrypt@muhbaasu.de";
    };
  };

  services.nginx = {
    enable = true;
    recommendedZstdSettings = true;
    recommendedTlsSettings = true;
    recommendedProxySettings = true;
    recommendedOptimisation = true;
    recommendedGzipSettings = true;
    recommendedBrotliSettings = true;

    virtualHosts = {
      "accounts.muhbaasu.de" = {
        serverName = "accounts.muhbaasu.de";
        enableACME = true;
        forceSSL = true;

        locations = {
          "/" = {
            proxyPass = "http://${config.services.keycloak.settings.http-host}:${toString config.services.keycloak.settings.http-port}";
            recommendedProxySettings = true;
          };
        };
      };

      "dashboard.muhbaasu.de" = {
        serverName = "dashboard.muhbaasu.de";
        enableACME = true;
        forceSSL = true;

        locations = {
          "/" = {
            proxyPass = "http://${config.services.grafana.settings.server.http_addr}:${toString config.services.grafana.settings.server.http_port}";
            proxyWebsockets = true;
            recommendedProxySettings = true;
          };
        };
      };

      "pfennig.muhbaasu.de" = {
        serverName = "pfennig.muhbaasu.de";
        enableACME = true;
        forceSSL = true;
      };
    };
  };

  # Set secrets via systemd's `EnvironmentFile` to allow runtime access to our
  # mounted secrets. We don't want to expose the secret at build time or
  # hardcode it in our configuration.
  systemd.services.grafana.serviceConfig.EnvironmentFile = config.age.secrets.grafana-secrets.path;

  services.grafana = {
    enable = true;

    settings = {
      database = {
        type = "postgres";
        host = "/run/postgresql/";
        name = "grafana";
        user = "grafana";
        password = "";
      };

      server = {
        domain = "dashboard.muhbaasu.de";
        http_addr = "127.0.0.1";
        root_url = "https://dashboard.muhbaasu.de";
      };

      "auth.generic_oauth" = {
        enabled = true;
        name = "accounts.muhbaasu.de";
        allow_sign_up = true;
        client_id = "grafana-oauth";
        # Set via environment variable
        #client_secret = ...;
        scopes = "openid email profile offline_access roles";
        use_refresh_token = true;
        email_attribute_path = "email";
        login_attribute_path = "username";
        name_attribute_path = "full_name";
        auth_url = "https://accounts.muhbaasu.de/realms/muhbaasu/protocol/openid-connect/auth";
        token_url = "https://accounts.muhbaasu.de/realms/muhbaasu/protocol/openid-connect/token";
        api_url = "https://accounts.muhbaasu.de/realms/muhbaasu/protocol/openid-connect/userinfo";
        role_attribute_path = "contains(roles[*], 'grafanaadmin') && 'GrafanaAdmin' || contains(roles[*], 'admin') && 'Admin' || contains(roles[*], 'editor') && 'Editor' || 'Viewer'";
        allow_assign_grafana_admin = true;
      };

      security = {
        cookie_secure = true;
        strict_transport_security = true;
      };
    };
  };

  services.postgresql = {
    enable = true;
    enableTCPIP = true;
    package = pkgs.postgresql_16_jit;
    extraPlugins = ps:
      with ps; [
        pg_repack
      ];

    authentication = ''
      # trust everyone connecting over a Unix domain socket
      local all all trust
      # trust everyone connecting over a Unix domain socket
      host all all samehost trust
    '';

    settings = {
      # Listen on local connections only
      listen_addresses = lib.mkForce "127.0.0.1,::1";
      port = 5432;

      # Settings recommended by https://pgtune.leopard.in.ua/
      # using these parameters:
      # DB Version: 16
      # OS Type: linux
      # DB Type: mixed
      # Total Memory (RAM): 16 GB
      # CPUs num: 16
      # Data Storage: ssd
      max_connections = 100;
      shared_buffers = "4GB";
      effective_cache_size = "12GB";
      maintenance_work_mem = "1GB";
      checkpoint_completion_target = 0.9;
      wal_buffers = "16MB";
      default_statistics_target = 100;
      random_page_cost = 1.1;
      effective_io_concurrency = 200;
      work_mem = "5242kB";
      huge_pages = false;
      min_wal_size = "1GB";
      max_wal_size = "4GB";
      max_worker_processes = 16;
      max_parallel_workers_per_gather = 4;
      max_parallel_workers = 16;
      max_parallel_maintenance_workers = 4;
    };

    ensureUsers = [
      {
        name = "postgres";
        ensureClauses = {
          superuser = true;
          createrole = true;
          createdb = true;
          bypassrls = true;
          login = true;
        };
      }
      {
        name = "keycloak";
        ensureDBOwnership = true;
        ensureClauses = {
          login = true;
        };
      }
      {
        name = "grafana";
        ensureDBOwnership = true;
        ensureClauses = {
          login = true;
        };
      }
      {
        name = "pfennig";
        ensureDBOwnership = true;
        ensureClauses = {
          login = true;
        };
      }
    ];

    ensureDatabases = [
      "keycloak"
      "grafana"
      "pfennig"
    ];
  };

  services.keycloak = {
    enable = true;

    settings = {
      hostname = "accounts.muhbaasu.de";
      # Listen on HTTP only because all connections happen locally only
      proxy = "edge";
      http-host = "[::1]";
      http-port = 8080;
    };

    database = {
      type = "postgresql";
      host = "localhost";
      name = "keycloak";
      username = "keycloak";
      port = 5432;
      passwordFile = keycloakPasswordFile.outPath;
    };
  };

  services.prometheus = {
    enable = true;
    port = 9090;
    listenAddress = "127.0.0.1";

    scrapeConfigs = [
      {
        job_name = "muhbaasu.de";
        static_configs = [
          {
            targets = [
              (exporterUrl "zfs")
              (exporterUrl "postgres")
              (exporterUrl "node")
              (exporterUrl "nginxlog")
              (exporterUrl "nginx")
            ];
          }
        ];
      }
    ];

    exporters = {
      zfs = {
        enable = true;
      };
      postgres = {
        enable = true;
      };
      node = {
        enable = true;
      };
      nginxlog = {
        enable = true;
      };
      nginx = {
        enable = true;
      };
    };
  };
}
