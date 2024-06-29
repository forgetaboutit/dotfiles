{pkgs, ...}: {
  fonts.packages = with pkgs; [
    font-awesome
    (nerdfonts.override {
      fonts = [
        "FiraCode"
        "Monaspace"
      ];
    })
    montserrat
    vollkorn
  ];
}
