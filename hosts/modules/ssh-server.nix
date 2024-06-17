{
  services.openssh = {
    enable = true;
    ports = [22];
    openFirewall = true;

    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "prohibit-password";
    };
  };
}
