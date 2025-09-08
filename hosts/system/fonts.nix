{pkgs, ...}: {
  fonts.packages = with pkgs; [
    font-awesome
    fira-sans
    nerd-fonts.monaspace
    nerd-fonts.fira-code
    montserrat
    vollkorn
  ];
}
