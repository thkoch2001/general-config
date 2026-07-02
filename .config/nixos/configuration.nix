{ config, lib, pkgs, ... }:
{
  imports =
    [
      <home-manager/nixos>
      # github.com/NixOS/nixos-hardware
      <nixos-hardware/lenovo/thinkpad/x260>
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./autologin.nix
      ./printing
    ];

  boot = {
    kernel.sysctl = {
      # https://knowledge.informatica.com/s/article/524270?language=en_US
      "kernel.yama.ptrace_scope" = 0;
    };
    # Use the systemd-boot EFI boot loader.
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;

    # see https://nixos.wiki/wiki/Full_Disk_Encryption
    initrd.kernelModules = [ "dm-snapshot" "cryptd" ];
    initrd.luks.devices."cryptroot".device = "/dev/disk/by-label/NixOS-Encrypted";
  };

  console = {
    font = "ter-128n";
    keyMap = "de-latin1-nodeadkeys";
    # useXkbConfig = true; # use xkb.options in tty.
    packages = with pkgs; [
      tamsyn # bitmap font
      terminus_font # bitmap font
      termsyn # bitmap font
    ];
  };

  documentation = {
    dev.enable = false; # false is default TODO
    nixos.includeAllModules = true;
    man = {
      mandoc.settings.output.toc = true;
      mandoc.enable = false; # man-db and mandoc can't be used as the default man page viewer at the same time!
      # generateCaches = true; # conflict with home-manager?
    };
  };

  environment.systemPackages = with pkgs; [
    pinentry-qt # provided here, see ~/.gnupg/gpg-agent.conf
    git
    htop
    qt5.qtwayland
    tmux
    tree
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
  ];

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NixOS-Root";
      fsType = "btrfs";
    };

    "/boot" = {
      device = "/dev/disk/by-label/NixOS-Boot";
      fsType = "vfat";
    };
  };

  fonts = {
    enableDefaultPackages = true;
    fontDir.enable = true; # needed for ladybird browser
    packages = with pkgs; [
      # corefonts # has unfree license
      dejavu_fonts
      font-awesome
      jetbrains-mono
      openmoji-color
      noto-fonts
    ];

    fontconfig = {
      hinting.enable = true;
      subpixel.lcdfilter = "light";

      defaultFonts = {
        monospace = [ "JetBrains Mono" ];
        emoji = [ "OpenMoji Color" ];
      };
    };
  };

  hardware = {
    bluetooth.enable = true;
    graphics = {
      enable = true;
      extraPackages = with pkgs; [
        vpl-gpu-rt
        #intel-media-sdk
      ];
    };
  };

  home-manager.users.thk = /home/thk/.config/home-manager/home.nix;

  i18n.defaultLocale = "de_DE.UTF-8";

  networking = {
    hostName = "thk-nixos";
    networkmanager = {
      enable = true;
      insertNameservers = [ "8.8.8.8" ];
    };
  };

  nix = {
    gc = {
      automatic = false;
      dates = "daily";
    };
    optimise.automatic = false;
    settings = {
      trusted-users = [ "@wheel" ];
      use-xdg-base-directories = true;
    };
  };

#  nixpkgs.config.allowAliases = false; # needed to use pkgs.river

  programs = {
    dconf.enable = true; # needed to set dconf stuff in home-manager
    fish.enable = true; # make sure it's there even if it's also configured in home-manager
    gnupg.agent = {
      enable = true;
      # enableSSHSupport = true; # TODO
    };
    # using ladybird from unstable
    # manually doing what ladybird.enable does
    # ladybird.enable = true; # https://github.com/LadybirdBrowser/ladybird/issues/572#issuecomment-2242268172
    mosh.enable = true;
    # mtr.enable = true; # TODO network diagnostic https://www.bitwizard.nl/mtr/
    ssh.startAgent = true;
  };

  # from tazjin
  security = {
    pam.services.swaylock = { };
    sudo.extraConfig = "Defaults insults,pwfeedback";
  };

  services = {
    autologin = {
      enable = lib.mkDefault true;
      leader = lib.mkDefault "${pkgs.niri}/bin/niri-session";
      user = "thk";
    };
    blueman.enable = true;
    libinput.enable = true;
    pcscd.enable = true;
    pipewire = {
      enable = true;
      pulse.enable = true;
    };
    openssh.enable = false;
    # Enable power-saving features.
    tlp.enable = true;
    udisks2.enable = true;
    # from tazjin, see https://yggdrasil-network.github.io/
    # Enable yggdrasil network.
    # yggdrasil = {
    #   enable = true;
    #   persistentKeys = true;
    #   settings.IfName = "ygg0";
    # };
    upower.enable = true;
  };

  specialisation = {
    reka.configuration =
    let
      reka = import (pkgs.fetchgit {
        url = "https://codeberg.org/tazjin/reka.git";
        rev = "refs/heads/canon";
        hash = "sha256-w25BZpbKMAACFweRqNEtGDvw/sNNJUfsVJQCXh41Y6w=";
      });
      reka-emacs = (pkgs.emacsPackagesFor pkgs.emacs-pgtk).emacsWithPackages (
        reka
      );
    in {
      imports = [ ./reka.nix ];
      programs.reka = {
        enable = true;
        debug = true;
        launchCommand = ''${reka-emacs}/bin/emacs --eval "(require 'reka)" --eval "(reka-enable)" --no-init-file'';
      };
      services.autologin.leader = "reka-session";
      services.autologin.enable = false;
    };
  };

  swapDevices = [{ device = "/dev/disk/by-label/NixOS-Swap"; }];

  system = {
    autoUpgrade = {
      dates = "daily";
      enable = false;
      operation = "boot";
    };
    # `man configuration.nix`
    # https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion
    stateVersion = "25.11"; # Did you read the comment?
  };

  time.timeZone = "Europe/Bucharest";

  users.users.thk = {
    isNormalUser = true;
    # my former Debian groups: cdrom floppy sudo audio dip video plugdev netdev bluetooth docker nix-users
    extraGroups = [ "video" "wheel" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.fish;
#    ignoreShellProgramCheck = true; # home-manager is responsible for ~/.config/fish
  };
}
