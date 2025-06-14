{ config, lib, pkgs, ... }:
{
  imports =
    [
      # github.com/NixOS/nixos-hardware
      <nixos-hardware/lenovo/thinkpad/x260>
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./autologin.nix
      ./fonts.nix
      ./laptop.nix
      ./printing
    ];

  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
      vpl-gpu-rt
      #intel-media-sdk
    ];
  };

  # see https://nixos.wiki/wiki/Full_Disk_Encryption
  boot.initrd.kernelModules = [ "dm-snapshot" "cryptd" ];
  boot.initrd.luks.devices."cryptroot".device = "/dev/disk/by-label/NixOS-Encrypted";

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

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "thk-nixos";
  networking.networkmanager.enable = true;

  time.timeZone = "Europe/Bucharest";

  i18n.defaultLocale = "de_DE.UTF-8";
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
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.fish;
    ignoreShellProgramCheck = true; # home-manager is responsible for ~/.config/fish
  };
  nix.settings.trusted-users = [ "@wheel" ];

  environment.systemPackages = with pkgs; [
    autologin
    pinentry-qt # provided here, see ~/.gnupg/gpg-agent.conf
    fish # make sure it's there even if it's also configured in home-manager
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
    gnupg.agent = {
      enable = true;
      # enableSSHSupport = true; # TODO
    };
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
    openssh.enable = true;
    # Enable power-saving features.
    tlp.enable = true;
    # from tazjin, see https://yggdrasil-network.github.io/
    # Enable yggdrasil network.
    # yggdrasil = {
    #   enable = true;
    #   persistentKeys = true;
    #   settings.IfName = "ygg0";
    # };
  };

  nix.gc = {
    automatic = true;
    dates = "daily";
  };
  # TODO https://nixos.org/manual/nixos/stable/#sec-upgrading
  # `man configuration.nix`
  # https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion
  system.stateVersion = "25.05"; # Did you read the comment?
}
