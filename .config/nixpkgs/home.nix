{ config, pkgs, ... }:

{
  home = {
    username = "romain";
    homeDirectory = "/home/romain";
    
    stateVersion = "21.05";

    packages = [
      pkgs.iosevka-bin
      pkgs.multimarkdown
      pkgs.nixFlakes
      pkgs.yadm
    ];

    sessionVariables = {
      DISPLAY = "$(cat /etc/resolv.conf | grep nameserver | awk '{print $2; exit;}'):0.0";
      PS1 = " \\[\\033[1;36m\\]\\u\\[\\033[1;31m\\]@\\[\\033[1;32m\\]\\h:\\[\\033[1;35m\\]\\w\\[\\033[1;31m\\]\\$\\[\\033[0m\\] ";
    };
    
  };

  programs = {

    bash = {
      enable = true;
      shellAliases = {
        ls = "ls --color=auto";
        grep = "grep --color=auto";
        fgrep = "fgrep --color=auto";
        egrep = "egrep --color=auto";
        ll = "ls -l";
      };
    };

    dircolors = {
      enable = true;
      enableBashIntegration = true;
    };
    
    direnv = {
      enable = true;
      enableBashIntegration = true;
      enableNixDirenvIntegration = true;
    };

    emacs.enable = true;

    git = {
      enable = true;
      userEmail = "romain.viallard@outlook.fr";
      userName = "aveltras";
      extraConfig = {
        init.defaultBranch = "main";
      };
    };

    home-manager.enable = true;
  };
  
  fonts.fontconfig.enable = true;

  targets.genericLinux.enable = true;
}
