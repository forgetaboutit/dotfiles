{pkgs, ...}: {
  fonts.packages = with pkgs; [
    nerdfonts
    font-awesome
    vollkorn
  ];
}
