{ config, pkgs, ... }:

let
  githubTarball = owner: repo: rev:
    builtins.fetchTarball { url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz"; };
  nixpkgs2009 = import (githubTarball "NixOS" "nixpkgs" "20.09") {};
in
{
  nixpkgs.config.allowUnfree = true;
  
  home = {
    username = "romain";
    homeDirectory = "/home/romain";
    
    stateVersion = "21.05";

    packages = [
      pkgs.awscli
      # nixpkgs2009.bazel
      pkgs.bazel
      pkgs.ghcid
      pkgs.haskellPackages.hspec-discover
      pkgs.haskellPackages.implicit-hie
      pkgs.httplz
      pkgs.ibm-plex
      pkgs.iosevka-bin
      pkgs.libvterm
      pkgs.kibana7
      pkgs.mongodb
      pkgs.multimarkdown
      pkgs.ncat
      pkgs.ngrok
      pkgs.nodejs
      pkgs.openjdk11
      pkgs.ormolu
      pkgs.patchelf
      # pkgs.pcre
      # pkgs.pcre.dev
      # pkgs.pkg-config
      pkgs.ripgrep
      pkgs.robo3t
      pkgs.yadm
    ];

    sessionVariables = {
      DISPLAY = "$(cat /etc/resolv.conf | grep nameserver | awk '{print $2; exit;}'):0.0";
      PULSE_SERVER = "tcp:$(cat /etc/resolv.conf | grep nameserver | awk '{print $2; exit;}'):0.0";
      PS1 = " \\[\\033[1;36m\\]\\u\\[\\033[1;31m\\]@\\[\\033[1;32m\\]\\h:\\[\\033[1;35m\\]\\w\\[\\033[1;31m\\]\\$\\[\\033[0m\\] ";
    };
    
  };

  programs = {

    bash = {
      enable = true;
      bashrcExtra = ''
        vterm_printf(){
            if [ -n "$TMUX" ] && ([ "''${TERM%%-*}" = "tmux" ] || [ "''${TERM%%-*}" = "screen" ] ); then
                # Tell tmux to pass the escape sequences through
                printf "\ePtmux;\e\e]%s\007\e\\" "$1"
            elif [ "''${TERM%%-*}" = "screen" ]; then
                # GNU screen (screen, screen-256color, screen-256color-bce)
                printf "\eP\e]%s\007\e\\" "$1"
            else
                printf "\e]%s\e\\" "$1"
            fi
        }
      '';
      shellAliases = {
        ls = "ls --color=auto";
        grep = "grep --color=auto";
        fgrep = "fgrep --color=auto";
        egrep = "egrep --color=auto";
        ll = "ls -lAXGhv --group-directories-first";
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
