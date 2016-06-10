# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

# Mostly shamelessly stolen from DamienCassou's GitHub.

{ config, pkgs, ... }:

let
  hostname = "nixos";
  user = "dvl";
in {
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
    ];

  # put a copy of configuration.nix in /run/current-system -- broken with
  # symlinks :(
  # system.copySystemConfiguration = true;

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda";

  boot.kernel.sysctl = {
    "vm.swappiness" = 5;
    "vm.min_free_kbytes" = 131072;
  };

  swapDevices = [
    { label = "nixwap"; }
  ];

  fileSystems = {
    "/" = { label = "nixos-root"; };
    "/home" = { label = "nixos-home"; };
  };

  networking.hostName = hostname; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Select internationalization properties.
  i18n = {
    # remember that you might still need to use `setfont` if graphics initialization
    # resets the font
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Prague";

  # Prefer custom nixpkgs if available.
  nix.nixPath = [
    "/home/${user}/src/"
    "/nix/var/nix/profiles/per-user/root/channels/nixos"
    "nixos-config=/etc/nixos/configuration.nix"
    "/nix/var/nix/profiles/per-user/root/channels"
  ];

  # enable zsh
  programs.zsh.enable = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    # docker
    # gnome3.vte
    # gnome3.vte-select-text
    arandr
    aspell
    aspellDicts.cs
    aspellDicts.en
    aspellDicts.es
    aspellDicts.fr
    audacity
    bibtex2html
    cmake
    chromium
    coreutils
    dmenu
    elfutils
    editorconfig
    emacs # see below for modifications
    fasd
    fbterm
    feh
    file
    firefoxWrapper
    gcc
    ghostscript
    gimp
    gitAndTools.gitFull
    glib # for gsettings
    gnome3.dconf
    gnumake
    gnupg
    go_1_6
    gparted
    # gutenprint
    haskellPackages.pandoc
    htop
    i3
    i3status
    imagemagick
    inkscape
    # kde4.krusader needs kde4.konsole for embedded terminal
    kde4.konsole
    kde4.krusader
    kde4.ksnapshot
    kde4.yakuake
    kde5.frameworkintegration
    kde5.okular
    kde5.plasma-framework
    kde5.plasma-workspace
    kde5.plasma-workspace-wallpapers
    kde5.print-manager
    libreoffice
    libxml2
    libxslt
    lsof
    lynx
    mosh
    nix-generate-from-cpan
    nix-repl
    nmap
    openjdk
    owncloudclient
    pciutils
    pcre
    perlPackages.Appcpanminus
    perlPackages.DataPrinter
    perlPackages.GetoptArgParse
    perlPackages.LogFast
    perlPackages.ModuleBuild
    perlPackages.Moose
    perlPackages.MooseXGetopt
    perlPackages.MooseXGetoptUsage
    psmisc
    python
    python35
    python35Packages.click
    python35Packages.cycler
    python35Packages.dateutil
    python35Packages.flake8
    python35Packages.flask
    python35Packages.ipython
    python35Packages.itsdangerous
    python35Packages.jinja2
    python35Packages.lxml
    python35Packages.markupsafe
    python35Packages.matplotlib
    python35Packages.notebook
    python35Packages.numpy
    python35Packages.pandas
    python35Packages.pew
    python35Packages.pip
    python35Packages.pyparsing
    python35Packages.pytz
    python35Packages.scipy
    python35Packages.six
    python35Packages.virtualenv
    # python35Packages.virtualenvwrapper
    python35Packages.werkzeug
    R
    rlwrap
    rstudio
    ruby
    silver-searcher
    sox
    sshfsFuse
    subversion
    (texlive.combine {
      inherit (texlive) scheme-medium type1cm wallpaper tcolorbox environ
      trimspaces;
      # more packages to be found at
      # https://github.com/NixOS/nixpkgs/blob/master/pkgs/tools/typesetting/tex/texlive-new/pkgs.nix if needed
    })
    thunderbird
    tmux
    tree
    unetbootin
    unrar
    utillinux
    vim
    vlc
    wget
    which
    zip unzip
  ];

  # List services that you want to enable:
  services = {
    openssh.enable = true;

    # Locate will update its database everyday at lunch time
    locate.enable = true;
    locate.interval = "12:00";

    # Cups
    printing = {
      enable = true;
      # drivers = [ pkgs.gutenprint ];
      # cupsdConf = ''
      #   BrowsePoll kopirka-barevna.ucnk
      #   BrowsePoll kopirka.ucnk
      # '';
    };

    # gnome3 = {
    #   tracker.enable = false; # I don't use tracker
    #   gnome-keyring.enable = true;
    # };

    xserver = {
      enable = true;
      layout = "cz,us,fr";
      # xkbOptions = "grp:alt_shift_toggle";
      xkbVariant = "qwerty";

      # displayManager.gdm.enable = true;
      # displayManager.sddm.enable = true;
      # displayManager.sddm.theme = "circles";
      # displayManager.kdm.enable = true;
      displayManager.slim.enable = true;

      # desktopManager.gnome3.enable = true;
      desktopManager.kde5.enable = true;
      # desktopManager.kde4.enable = true;
      # windowManager.i3.enable = true;

      # https://github.com/NixOS/nixpkgs/issues/4416
      # displayManager.desktopManagerHandlesLidAndPower = false;

      # This is the way to activate some Gnome 3 modules
      # desktopManager.gnome3.sessionPath = with pkgs.gnome3_12; [ gpaste pomodoro ];

      # openvpn.enable = true;

      # Launch backintime regularly
      # cron.systemCronJobs = [
      #   "*/15 * * * * cassou ${pkgs.coreutils}/bin/nice -n 19 ${pkgs.utillinux}/bin/ionice -c2 -n7 ${pkgs.backintime-common}/bin/backintime --profile-id 2 --backup-job"
      #   "0 * * * *    cassou ${pkgs.coreutils}/bin/nice -n 19 ${pkgs.utillinux}/bin/ionice -c2 -n7 ${pkgs.backintime-common}/bin/backintime --backup-job"
      # ];

    };
  };

  # Use Gnome 3.12
  # environment.gnome3.packageSet = pkgs.gnome3_12;

  nixpkgs.config = {
    allowUnfree = true;

    firefox = {
      enableGoogleTalkPlugin = true;
      enableAdobeFlash = true;
    };

    chromium = {
      enablePepperFlash = true;
      enablePepperPDF = true;
    };

    # pidgin = {
    #   openssl = true;
    #   gnutls = true;
    # };

    # Enable USB in guest OS
    virtualbox.enableExtensionPack = true;
  };

  # The wifi broadcom driver
  # networking.enableB43Firmware = true;
  networking.firewall.allowedTCPPorts = [ 22 80 ];
  networking.firewall.allowPing = true;

  # Make sure the only way to add users/groups is to change this file
  users.mutableUsers = false;
  # users.defaultUserShell = "/run/current-system/sw/bin/zsh";

  # Add myself as a super user
  users.extraUsers."${user}" = {
    isNormalUser = true;
    shell = "/run/current-system/sw/bin/zsh";
    # createHome = true;
    # home = "/home/${user}";
    description = "David Lukeš";
    extraGroups = [ "wheel" "networkmanager" "vboxusers" "docker" "video" "vboxusers" ];
    # uid = 1000;
    hashedPassword = "$6$88zhfvgkxNg/F$Ns/V98sNZ8gorl9uNaFqOk/Zcp3c1HGT2eP0nVTGsgG.Mz1cHXeZ5gY/Jc89sUS7uXUuVK2WntaKcvFMVxPCG0";
    # ... and other stuff can also be set here, like SSH keys
  };

  users.extraUsers."test" = {
    isNormalUser = true;
    createHome = true;
    description = "Testy McTestface";
    extraGroups = [ "wheel" "networkmanager" "vboxusers" "docker" "video" "vboxusers" ];
    hashedPassword = "$6$88zhfvgkxNg/F$Ns/V98sNZ8gorl9uNaFqOk/Zcp3c1HGT2eP0nVTGsgG.Mz1cHXeZ5gY/Jc89sUS7uXUuVK2WntaKcvFMVxPCG0";
  };

  # better do this manually in case of any clashes
  # powerManagement.powerDownCommands = ''
  #   cd /home/${user}/Google\ Drive
  #   /home/dvl/src/go/bin/drive push
  # '';

  # Add fonts
  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      corefonts # Microsoft free fonts
      dejavu_fonts
      powerline-fonts
      ubuntu_font_family
    ];
  };

  # Fix problem with Emacs tramp (https://github.com/NixOS/nixpkgs/issues/3368)
  programs.bash = {
    # promptInit = "PS1=\"# \"";
    enableCompletion = true;
  };

  virtualisation.virtualbox.host.enable = true;

  # stuff for which sudo shouldn't ask me passwords; POWER commands should
  # by default according to the NixOS manual, but they don't -- perhaps
  # because I'm using zsh instead of bash? (just a wild thought)
  # security.sudo.extraConfig =
  # ''
  #   Cmnd_Alias POWER = /run/current-system/sw/bin/systemctl poweroff,\
  #     /run/current-system/sw/bin/systemctl reboot,\
  #     /run/current-system/sw/bin/systemctl suspend,\
  #     /run/current-system/sw/bin/systemctl hibernate,\
  #     /run/current-system/sw/bin/shutdown,\
  #     /run/current-system/sw/bin/reboot
  #   ${user} ${hostname} = (root) NOPASSWD: POWER
  # '';

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.03";

}

# Local Variables:
# mode: nix
# End:
