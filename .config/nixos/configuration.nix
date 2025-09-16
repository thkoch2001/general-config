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
      ./laptop.nix
      ./printing
    ];

  home-manager.users.thk = /home/thk/.config/home-manager/home.nix;
  hardware.bluetooth.enable = true;
  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
      vpl-gpu-rt
      #intel-media-sdk
    ];
  };

  boot = {
    # Use the systemd-boot EFI boot loader.
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;

    # see https://nixos.wiki/wiki/Full_Disk_Encryption
    initrd.kernelModules = [ "dm-snapshot" "cryptd" ];
    initrd.luks.devices."cryptroot".device = "/dev/disk/by-label/NixOS-Encrypted";
  };

  # documentation.man.generateCaches = true;
  fileSystems."/" =
    {
      device = "/dev/disk/by-label/NixOS-Root";
      #    { device = "/dev/disk/by-uuid/561c61cf-de56-49e0-bdd4-3252ab766d38";
      fsType = "btrfs";
    };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-label/NixOS-Boot";
      fsType = "vfat";
    };
  #    { device = "/dev/disk/by-uuid/82F8-842D";
  #      options = [ "fmask=0022" "dmask=0022" ];

  swapDevices = [{ device = "/dev/disk/by-label/NixOS-Swap"; }];
  #    [ { device = "/dev/disk/by-uuid/ae097c9a-a9eb-474d-8d22-95ceb511a096"; }

  networking = {
    hostName = "thk-nixos";
    networkmanager = {
      enable = true;
      insertNameservers = [ "8.8.8.8" ];
    };
  };

  time.timeZone = "Europe/Bucharest";

  i18n.defaultLocale = "de_DE.UTF-8";

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

  users.users.thk = {
    isNormalUser = true;
    # my former Debian groups: cdrom floppy sudo audio dip video plugdev netdev bluetooth docker nix-users
    extraGroups = [ "video" "wheel" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.fish;
#    ignoreShellProgramCheck = true; # home-manager is responsible for ~/.config/fish
  };

  nix.settings = {
    trusted-users = [ "@wheel" ];
    use-xdg-base-directories = true;
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

  # from tazjin
  security.pam.services.swaylock = { };

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

  services = {
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

  nix.optimise.automatic = true;
  nix.gc = {
    automatic = true;
    dates = "daily";
  };
  system.autoUpgrade = {
    dates = "daily";
    enable = true;
    operation = "boot";
  };

  # `man configuration.nix`
  # https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion
  system.stateVersion = "25.05"; # Did you read the comment?
}
