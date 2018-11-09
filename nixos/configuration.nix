# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let

  inherit (pkgs) lib callPackage stdenv fetchFromGitHub;

  libinput-gestures = callPackage ./libinput-gestures.nix {};

  simplicity = stdenv.mkDerivation rec {
    name = "simplicity";
    src = fetchFromGitHub {
      owner = "gabretana";
      repo = "simplicity-sddm-theme";
      rev = "7437e662c50e30ea557d08bc05b0ae48282c3c77";
      sha256 = "1hxxwvmasjq5klfkglh16yvlz9283lchkc6milil4xvkbyaz3wab";
    };
    # dontBuild = true;
    installPhase = ''
      mkdir -p $out/share/sddm/themes/
      echo $out
      cp -r simplicity $out/share/sddm/themes
      ls -la $out/share/sddm/themes
    '';
  };

in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./keys.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.grub.device = "/dev/nvme0n1";
  boot.loader.timeout = 0;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.plymouth.enable = true;

  hardware.cpu.intel.updateMicrocode = true;

  swapDevices = [ { label = "swap"; } ];

  networking.hostName = "delilah"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.wireless.networks = {
    "OnePlus 5" = {
      psk = "404ce879f965";
    };
    SKYC0FA5 = {
      psk = "XDSBVBXV";
    };
    "WholeFoodsMarket" = {};
    SHAW-D2AA40-5G = {
      psk = "251151000198";
    };
    Spartacus = {
      psk = "spartacus";
    };
    SG-User = {
      psk = "suitegenius";
    };
    ShawOpen = {};
    "GokiteCabarete.com" = {
      psk = "MesaExtreme2511";
    };
    "Fresh Fresh cafe2" = {
      psk = "eatfresh2";
    };
    "Co-cab wifi" = {
      psk = "31498968";
    };
    "LAGUNA PARK 2" = {};
    "LAGUNA PARK 3" = {};
    "Vagamundo Coffee & Waffles" = {
      psk = "espressobean";
    };
    Claro_1271 = {
      psk = "KjjUvGx3py";
    };
  };

  networking.extraHosts = ''
    192.168.56.101 local.moneyandco.com
  '';

  # Select internationalisation properties.
  i18n = {
    consoleFont = "latarcyrheb-sun32";
    consoleKeyMap = "uk";
    defaultLocale = "en_GB.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "America/Santo_Domingo";

  nixpkgs.config.allowUnfree = true;

  # nixpkgs.config.packageOverrides = super: {
  #   sddm = lib.overrideDerivation super.sddm
  #     (a: { patches = a.patches ++ ["/etc/nixos/sddm1.patch"]; });
  # };

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    arc-theme
    aspell
    aspellDicts.en
    at_spi2_core
    breeze-qt5
    cabal-install
    compton-git
    darktable
    direnv
    emacs
    feh
    firefox
    git
    git-crypt
    git-lfs
    gnumake
    gnupg
    haskellPackages.brittany
    haskellPackages.ghcid
    haskellPackages.hasktags
    haskellPackages.hindent
    haskellPackages.hlint
    haskellPackages.stylish-haskell
    haskellPackages.xmobar
    hsetroot
    htop
    keepassx2
    keychain
    minecraft
    openssl
    openvpn
    pavucontrol
    ripgrep
    rofi
    rxvt_unicode
    simplicity
    slock
    stack
    taskwarrior
    texlive.combined.scheme-full
    vit
    xdotool
    xorg.xbacklight
    zathura
  ];

  programs.zsh = {
    enable = true;
    # ohMyZsh = {
    #   enable = true;
    #   theme = "spaceship";
    # };
  };

  users.defaultUserShell = pkgs.zsh;

  programs.vim.defaultEditor = true;

  security.wrappers.slock.source = "${pkgs.slock.out}/bin/slock";

  environment.shells = [ "/run/current-system/sw/bin/zsh" ];

  # Link paths for sddm - should upstream this to nixpkgs
  environment.pathsToLink = [ "/share/sddm" "/share/sddm/themes" ];

  fonts.fonts = with pkgs; [ fira-code fira-code-symbols font-awesome-ttf ];

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  networking.firewall.allowedTCPPortRanges = [ { from = 8080; to = 8180; } ];
  # networking.firewall.allowedUDPPorts = [ 8081 8082 8083 ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;
  networking.firewall.extraCommands = ''
    iptables -A nixos-fw -d 224.0.0.0/4 -j nixos-fw-accept
  '';

  hardware.pulseaudio.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.gutenprint ];

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    displayManager = {
      xserverArgs = [ "-dpi 192" ];
      sddm = {
        enable = true;
        theme = "simplicity";
        extraConfig = ''
          [X11]
          EnableHiDPI=true
        '';
      };
    };
    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;
    layout = "gb";
    xkbOptions = "caps:super";
    libinput.enable = true;
    libinput.naturalScrolling = true;
    # libinput.tapping = false;
  };

  # Themes
  environment.extraInit = ''
    # GTK3: add /etc/xdg/gtk-3.0 to search path for settings.ini
    # We use /etc/xdg/gtk-3.0/settings.ini to set the icon and theme name for GTK 3
    export XDG_CONFIG_DIRS="/etc/xdg:$XDG_CONFIG_DIRS"
    # GTK2 theme + icon theme
    export GTK2_RC_FILES=${pkgs.arc-theme}/share/themes/Arc-Darker/gtk-2.0/gtkrc:$GTK2_RC_FILES

    # these are the defaults, but some applications are buggy so we set them
    # here anyway
    export XDG_CONFIG_HOME=$HOME/.config
    export XDG_DATA_HOME=$HOME/.local/share
    export XDG_CACHE_HOME=$HOME/.cache
  '';

  # QT4/5 global theme
  environment.etc."xdg/Trolltech.conf" = {
    text = ''
      [Qt]
      style=Arc-Darker
    '';
    mode = "444";
  };

  environment.etc."xdg/gtk-3.0/settings.ini" = {
    text = ''
      [Settings]
      gtk-icon-theme-name=breeze
      gtk-theme-name=Arc-Darker
    '';
    mode = "444";
  };

  environment.etc."libinput-gestures.conf" = {
    text = ''
      device DLL06E4:01 06CB:7A13 Touchpad
      gesture swipe left xdotool key Super_L+shift+Tab
      gesture swipe right xdotool key Super_L+shift+alt+Tab
    '';
    mode = "444";
  };

  environment.variables.QT_AUTO_SCREEN_SCALE_FACTOR = "1";

  # emacs
  # services.emacs.enable = true;
  # services.emacs.defaultEditor = true;

  # Hibernate-after-suspend
  systemd.services.hibernate-after-suspend = {
    description = "Hibernate after suspend";
    before = [ "suspend.target" ];
    conflicts = [ "hibernate.target" "hybrid-suspend.target" ];
    unitConfig = {
     StopWhenUnneeded = "true";
    };
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = "yes";
      Environment = [ "\"ALARM_SEC=1800\"" "\"WAKEALARM=/sys/class/rtc/rtc0/wakealarm\"" ];
      ExecStart = "-/run/current-system/sw/bin/rtcwake --seconds $ALARM_SEC --auto --mode no";
      ExecStop = ''
        -/bin/sh -c '\
        ALARM=$(cat $WAKEALARM); \
        NOW=$(date +%%s); \
        if [ -z "$ALARM" ] || [ "$NOW" -ge "$ALARM" ]; then \
            echo "hibernate-after-suspend: Woke up - no alarm set. Hibernating..."; \
            systemctl hibernate; \
        else \
            echo "hibernate-after-suspend: Woke up before alarm - normal wakeup."; \
            /run/current-system/sw/bin/rtcwake --auto --mode disable; \
        fi \
        '
      '';
    };
    wantedBy = [ "sleep.target" ];
    requiredBy = [ "suspend.target" ];
  };

  systemd.user.services."libinput-gestures" = {
    description = "Add multitouch gestures using libinput-gestures";
    wantedBy = [ "default.target" ];
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${libinput-gestures}/bin/libinput-gestures";
    environment = { DISPLAY = ":0"; };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.rupert = {
    isNormalUser = true;
    extraGroups = [ "wheel" "input" ];
    useDefaultShell = true;
  };

  programs.oblogout = {
    enable = true;
    buttons = "cancel, logout, restart, shutdown, lock, hibernate";
    clogout = "xdotool key Super_L+shift+Q";
    clock = "slock";
  };

  virtualisation.virtualbox.host.enable = true;

  # nix.useSandbox = true;

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.09";

  system.autoUpgrade.enable = true;

  # IOHK
  nix.binaryCaches = [ "http://cache.nixos.org" "https://hydra.iohk.io" ];
  nix.binaryCachePublicKeys = [
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
  ];

  programs.gnupg.agent.enable = true;

  # Money&Co.
  services.postgresql.enable = true;

  environment.etc.git-ssh-config = {
    text = ''
      Host github.com
      IdentityFile /etc/ssh/mandco_rsa_key
      StrictHostKeyChecking=no
    '';
    user = "nixbld1";
    group = "nixbld";
    mode = "0400";
  };

  nix.nixPath = [
    # (let
    #   sshConfigFile =
    #     pkgs.writeText "ssh_config" ''
    #       Host github.com
    #       IdentityFile /etc/ssh/mandco_rsa_key
    #       StrictHostKeyChecking=no
    #     '';
    # in
    #   "ssh-config-file=${sshConfigFile}"
    # )
    "ssh-config-file=/etc/git-ssh-config"
    "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos/nixpkgs"
    "nixos-config=/etc/nixos/configuration.nix"
    "/nix/var/nix/profiles/per-user/root/channels"
  ];

  # Fixing network failure on resume bug
  powerManagement.resumeCommands = ''
    systemctl restart dhcpcd.service
  '';

  nix.package = pkgs.nixUnstable;

}
