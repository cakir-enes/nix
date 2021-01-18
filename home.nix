{ config, pkgs, ... }:

let unstable = import <unstable> { config = config.nixpkgs.config; };
in {
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "ec";
  home.homeDirectory = "/home/ec";
  nixpkgs.config.allowUnfree = true;

  home.packages =
    [ pkgs.htop pkgs.fortune pkgs.starship pkgs.nixfmt pkgs.broot pkgs.bat pkgs.rustup pkgs.leiningen ];

  programs.git = {
    enable = true;
    userName = "cakir-enes";
    userEmail = "cakir-enes@outlook.com";
  };

  programs.rofi = { enable = true; };

  programs.vscode = { enable = true; };

programs.neovim = {
  enable = true;
};

  services.picom = {
    enable = true;
    inactiveOpacity = "0";
    activeOpacity = "1"; # "0.90";
    opacityRule = [
      "100:class_g = 'Google-chrome'"
      "100:class_g = 'Alacritty'"
      "60:class_g = 'rofi'"
    ];
    fade = true;
    vSync = true;
    shadow = true;
    fadeDelta = 4;
    fadeSteps = [ "0.02" "0.02" ];
    blur = true;
    inactiveDim = "0.2";
    extraOptions = ''
      frame-opacity = 1;
      blur-background = true;
      blur-kern = "7x7box";
      blur-background-exclude = [];
    '';
  };

  xsession.enable = true;
  xsession.windowManager = {
    xmonad = {
      enable = true;
      config = ./xmonadcfg.hs;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: [
        haskellPackages.xmonad-contrib
        haskellPackages.xmonad-extras
        haskellPackages.xmonad
      ];
    };
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.03";
}
