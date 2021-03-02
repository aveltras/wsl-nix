{ config, pkgs, ... }:

{
  programs.home-manager.enable = true;
  targets.genericLinux.enable = true;

  home.username = "wsl";
  home.homeDirectory = "/home/wsl";

  home.stateVersion = "21.05";

  programs.emacs.enable = true;

  home.packages = [
    pkgs.yadm
  ];

  programs.git = {
    enable = true;
    userEmail = "romain.viallard@outlook.fr";
    userName = "aveltras";
    extraConfig = {
      init.defaultBranch = "main";
    };
  };
 
  programs.bash = {
    enable = true;
    sessionVariables = {
      DISPLAY = "$(cat /etc/resolv.conf | grep nameserver | awk '{print $2; exit;}'):0.0";
    };
  };
}
