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

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda";

  fileSystems = {
    "/" = { label = "nixos-root"; };
    "/home" = { label = "nixos-home"; };
  };

  swapDevices = [
    { label = "nixwap"; }
  ];

  networking.hostName = hostname; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Select internationalization properties.
  i18n = {
    # consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Prague";

  # enable zsh
  programs.zsh.enable = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    # docker
    # gnome3.vte
    # gnome3.vte-select-text
    aspell
    aspellDicts.cs
    aspellDicts.en
    aspellDicts.es
    aspellDicts.fr
    cmake
    chromium
    coreutils
    dmenu
    elfutils
    editorconfig
    emacs # see below for modifications
    fasd
    firefoxWrapper
    gcc
    ghostscript
    gimp
    gitAndTools.gitFull
    glib # for gsettings
    gnome3.dconf
    gnumake
    gnupg
    go
    gparted
    haskellPackages.pandoc
    htop
    i3
    i3status
    inkscape
    # kde4.krusader needs kde4.konsole for embedded terminal
    kde4.konsole
    kde4.krusader
    kde4.yakuake
    kde5.okular
    libreoffice
    lsof
    nmap
    pcre
    perlPackages.Appcpanminus
    perlPackages.DataPrinter
    perlPackages.Moose
    # perlPackages.LogFast
    psmisc
    python3
    python34Packages.flask
    python34Packages.pew
    python34Packages.pip
    python34Packages.virtualenv
    python34Packages.virtualenvwrapper
    rlwrap
    ruby
    silver-searcher
    sox
    sshfsFuse
    subversion
    # texLiveFull
    texlive.combined.scheme-medium
    # texlive.combine {
    #   inherit (texlive) scheme-medium
    #   # more packages to be found at
    #   # https://github.com/NixOS/nixpkgs/blob/master/pkgs/tools/typesetting/tex/texlive-new/pkgs.nix if needed
    # }
    thunderbird
    tree
    unetbootin
    unrar
    utillinux
    vim
    vlc
    wget
    which
    zip
    zsh
  ];

  # List services that you want to enable:
  services = {
    # openssh.enable = true;

    # Locate will update its database everyday at lunch time
    locate.enable = true;
    locate.period = "00 12 * * *";

    # Cups
    printing = {
      enable = true;
      # cupsdConf = ''
      #   BrowsePoll cups.lille.inria.fr:631
      #   BrowsePoll prserv.lifl.rf
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
      displayManager.sddm.enable = true;
      # displayManager.kdm.enable = true;

      # desktopManager.gnome3.enable = true;
      desktopManager.kde5.enable = true;
      # desktopManager.kde4.enable = true;
      windowManager.i3.enable = true;

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

    # pidgin = {
    #   openssl = true;
    #   gnutls = true;
    # };

    # Enable USB in guest OS
    virtualbox.enableExtensionPack = true;

    packageOverrides = pkgs: {
        # Define my own Emacs
        emacs = pkgs.lib.overrideDerivation (pkgs.emacs.override {
            # Use gtk3 instead of the default gtk2
            # gtk = pkgs.gtk3;
            # Make sure imagemgick is a dependency because I regularly
            # look at pictures from Emacs
            imagemagick = pkgs.imagemagickBig;
          }) (attrs: {
            # I don't want emacs.desktop file because I only use
            # emacsclient.
            postInstall = attrs.postInstall + ''
              sed -ri 's/emacs %F/emacsclient -c %F/' $out/share/applications/emacs.desktop
            '';
        });

    };

  };

  # The wifi broadcom driver
  # networking.enableB43Firmware = true;

  # Make sure the only way to add users/groups is to change this file
  users.mutableUsers = false;
  users.defaultUserShell = "/run/current-system/sw/bin/zsh";

  # Add myself as a super user
  users.extraUsers."${user}" = {
    isNormalUser = true;
    # createHome = true;
    # home = "/home/${user}";
    description = "David Lukeš";
    extraGroups = [ "wheel" "networkmanager" "vboxusers" "docker" ];
    # uid = 1000;
    hashedPassword = "$6$88zhfvgkxNg/F$Ns/V98sNZ8gorl9uNaFqOk/Zcp3c1HGT2eP0nVTGsgG.Mz1cHXeZ5gY/Jc89sUS7uXUuVK2WntaKcvFMVxPCG0";
    # ... and other stuff can also be set here, like SSH keys
  };

  # Add fonts
  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      corefonts # Microsoft free fonts
      inconsolata # monospaced
      ubuntu_font_family
      dejavu_fonts
    ];
  };

  # Fix problem with Emacs tramp (https://github.com/NixOS/nixpkgs/issues/3368)
  # programs.bash = {
  #   promptInit = "PS1=\"# \"";
  #   enableCompletion = true;
  # };

  # Make sure I can use "openvpn" without entering my password
  # security.sudo.configFile =
  # ''
  #   cassou ${hostname} = (root) NOPASSWD: /run/current-system/sw/sbin/openvpn
  # '';

  # Not sure what this is about anymore (nor if it's useful)
  # environment.variables.GIO_EXTRA_MODULES = [ "${pkgs.gnome3.dconf}/lib/gio/modules" ];

  # Create a systemd user service for emacs daemon. This is useful because
  # systemd will take care of launching emacs in the background and I
  # will just have to connect to it through emacs-client. This is a
  # user service. This means I have to pass the "--user" option to
  # systemd when I want to control the service.
  systemd.user.services.emacs = {
    description = "Emacs: the extensible, self-documenting text editor";


    serviceConfig = {
      Type = "forking";
      ExecStart = "${pkgs.emacs}/bin/emacs --daemon";
      ExecStop = "${pkgs.emacs}/bin/emacsclient --eval (kill-emacs)";
      Restart = "always";
    };

    # I want the emacs service to be started with the rest of the user services
    wantedBy = [ "default.target" ];

    # Annoyingly, systemd doesn't pass any environment variable to its
    # services. Below, I set some variables that I missed.
    environment = {
      # Give Emacs a chance to use gnome keyring for the ssh-agent
      SSH_AUTH_SOCK = "%t/keyring/ssh";

      # Some variables for GTK applications I will launch from Emacs
      # (typically evince and the gnome-terminal)
      GTK_DATA_PREFIX = config.system.path;
      # GTK_PATH = "${config.system.path}/lib/gtk-3.0:${config.system.path}/lib/gtk-2.0";
      GTK_RC_FILES = "/etc/gtk/gtkrc:/home/dvl/.gtkrc:/home/dvl/.config/gtkrc";
      GTK2_RC_FILES = "/etc/gtk-2.0/gtkrc:/home/dvl/.gtkrc-2.0:/home/dvl/.config/gtkrc-2.0";
      GTK_PATH = "/home/dvl/.nix-profile/lib/gtk-2.0:/home/dvl/.nix-profile/lib/gtk-3.0:/nix/var/nix/profiles/default/lib/gtk-2.0:/nix/var/nix/profiles/default/lib/gtk-3.0:/run/current-system/sw/lib/gtk-2.0:/run/current-system/sw/lib/gtk-3.0";

      # Make sure aspell will find its dictionaries
      ASPELL_CONF = "dict-dir /run/current-system/sw/lib/aspell";

      # Make sure locate will find its database
      LOCATE_PATH = "/var/cache/locatedb";
    };
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "15.09";

}
