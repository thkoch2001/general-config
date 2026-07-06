{ config, lib, pkgs, ... }:
let
  upkgs = (import <nixpkgs-unstable> { }).pkgs;
  thk-nur = import
    (pkgs.fetchFromGitHub {
      owner = "thkoch2001";
      repo = "nur-packages";
      rev = "e3afcb25c7539d47b52023057b7f6f99cd025af3";
      hash = "sha256-pi9pakM7MujZZktPp7P5EJhEINPT3sFs2/H0mnJhdBQ=";
    })
    { inherit pkgs; };
in
{
  imports =
    [
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
      cache.enable = true;
      mandoc.settings.output.toc = true;
      # TODO      mandoc.enable = false; # man-db and mandoc can't be used as the default man page viewer at the same time!
    };
  };

  environment = {
    homeBinInPath = true;
    localBinInPath = true;
    sessionVariables = {
      NIXOS_OZONE_WL = "1";
    };
    # TODO:
    # get rid of chromium
    # https://github.com/crossbowerbt/dillo-plus/
    # https://sr.ht/~bptato/chawan/
    systemPackages = with pkgs;
      let
        # https://nixos.wiki/wiki/TexLive
        # https://gitlab.com/Kaligule/org-to-pdf/-/blob/master/shell.nix
        tex_for_orgmode = (texlive.combine {
          inherit (pkgs.texlive) scheme-basic
            dvisvgm dvipng# for preview and export as html
            wrapfig amsmath ulem hyperref capt-of
            latexmk collection-fontsrecommended
            ;
          #(setq org-latex-compiler "lualatex")
          #(setq org-preview-latex-default-process 'dvisvgm)
        });
      in
      [
        # # It is sometimes useful to fine-tune packages, for example, by applying
        # # overrides. You can do that directly here, just don't forget the
        # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
        # # fonts?
        # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

        # # You can also create simple shell scripts directly inside your
        # # configuration. For example, this adds a command 'my-hello' to your
        # # environment:
        # (pkgs.writeShellScriptBin "my-hello" ''
        #   echo "Hello, ${config.home.username}!"
        # '')

        alacritty
        (aspellWithDicts (d: [ d.de d.en d.ro d.ru ]))
        bat # syntax highlighting cat https://github.com/sharkdp/bat
        bc # cli calculator
        # blueman
        (callPackage ./borg2.nix { })
        # cargo # rust package manager
        chromium # needed although programs.enabled.chromius = true; ungoogled-chromium didn't work
        # clippy # rust lint tool, see https://github.com/rust-lang/rustlings/issues/2111
        copyq # clipboard manager
        # darcs
        # davfs2
        # detox
        # devenv
        dnsutils
        # dovecot_pigeonhole
        # duff
        eom
        evince
        (import ./emacs.nix { pkgs = upkgs; })
        ffmpeg
        # freecad
        fuzzel # application launcher
        gcc # needed for rustc
        # ghc
        gimp
        gitFull # To also get gitk
        git-annex
        # git-big-picture
        # git-imerge
        # glibcLocalesUtf8
        # gnome.cheese
        gnumake
        go-mtpfs # mount android
        # gparted
        # gramps # gen-trees
        graphviz
        htop
        kanshi # wayland equivalent to autorandr
        kdePackages.akregator
        kdePackages.gwenview
        # kdePackages.kgeography
        kdePackages.okular
        # kdePackages.pim-sieve-editor
        # (pkgs.haskell-language-server.override { supportedGhcVersions = [ "96" ]; })
        # hledger # bookkeeping
        # hledger-ui
        # html-tidy
        # jdupes
        josh # monorepos with git
        jq
        # kdePackages.falkon # webbrowser
        # ladybird # webbrowser C++, needs to be installed via NixOS
        ladybird
        # klavaro
        libnotify # provides notify-send
        libreoffice-fresh
        libwebp # for dwebp for telegram emacs client telega, to display stickers
        # libsecret # for msmtp
        # lingot
        lsof # git-annex assistant checks if files are open for writing
        # lxde.lxsession
        # lynx
        # mdbook
        # mediathekview
        mpv
        # msmtp
        # muvm # Run programs from your system in a microVM
        # ncdu
        # netsurf.browser
        networkmanagerapplet
        # newsboat
        nettools
        niri # tliing wayland composer
        nixpkgs-fmt
        # nix-prefetch-github
        # nmap
        openrsync
        # palemoon-bin # firefox fork https://www.palemoon.org/
        pandoc # e.g. markdown preview in emacs
        # paperkey
        # parcellite
        pass
        # pasystray
        pavucontrol
        pinentry-qt # provided here, see ~/.gnupg/gpg-agent.conf
        # powertop
        # psmisc # fuser, killall and pstree
        pspg # postgres pager
        # pwgen # maybe replace with spwgen from pwgen-secure?
        # qemu_kvm # qickemu should bring in qemu_full
        #qgnomeplatform-qt6 # dark-mode for e.g. akregator, see https://people.debian.org/~werdahias/enforcing-darkmode-for-qt-programs-under-a-non-qt-based-environment.html     #adwaita-qt6
        qt5.qtwayland
        quasselClient
        # from tazjin
        # pkgs.quassel.override {
        #   client = true;
        #   enableDaemon = false;
        #   monolithic = false;
        # };
        quickemu # Quickly create and run optimised Windows, macOS and Linux virtual machines
        # redshift
        # reuse
        # rust-analyzer
        # rustc # rust compiler
        # rustfmt
        # rustic # backup, see GitHub issue on --cacert option
        # rustup
        samba # for file sharing to VMs, see quickemu
        # sassc
        # screen-message
        # sequoia-sq
        servo
        # sshfs
        # stack
        strace
        # sqlite
        swayidle
        swaylock
        # sweethome3d.application
        # sweethome3d.furniture-editor
        # sweethome3d.textures-editor
        telegram-desktop
        # termonad
        tex_for_orgmode
        # thunderbird
        # tig # text mode interface to git (to browse sparse checkouts)
        tmux
        traceroute
        translate-shell # Command-line translator using Google Translate, Bing Translator, Yandex.Translate, and Apertium
        # https://github.com/soimort/translate-shell/wiki/Configuration
        tree
        # unrar-free
        unzip
        # usbutils # provides lsusb
        vcsh
        vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
        # virtiofsd
        vlc
        watchexec
        wavemon
        wdisplays
        wget
        wl-clipboard
        wlr-randr
        wlr-which-key
        xdg-utils # xdg-open
        xwayland-satellite
        xlsclients # find out which running apps use xwayland
        xwininfo # get X11 info by clicking on window
        yt-dlp
        thk-nur.yt2feed
        zip
      ];
  };

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
      extra-experimental-features = [ "flakes" "nix-command" ];
      trusted-users = [ "@wheel" ];
      use-xdg-base-directories = true;
    };
  };

  #  nixpkgs.config.allowAliases = false; # needed to use pkgs.river

  programs = {
    chromium = {
      enable = true;
    };
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
    dconf = {
      enable = true;
      # https://github.com/Electrostasy/dots/blob/c62895040a8474bba8c4d48828665cfc1791c711/profiles/system/gnome/default.nix#L123-L287
      profiles = {
        user.databases = [{
          settings = with lib.gvariant; {
            "org/blueman/general" = {
              # dunst doesn't seem to support OSD to enter pin codes
              # https://ubuntu-mate.community/t/bluetooth-doesnt-display-pin-code-for-pairing/8496
              notification-daemon = false;
            };
            "org/blueman/transfer" = {
              opp-accept = true;
              shared-path = "/home/thk/Downloads";
            };
          };
        }];
      };
    };
    firefox = {
      enable = false;
      package = pkgs.librewolf;
    };
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
    waybar.enable = true;
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
    dunst = {
      enable = true;
      settings = {
        global.font = "DejaVu Sans 12";
      };
    };

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

  /*specialisation = {
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
  };*/

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

  systemd.user.services = {
    chromium = {
      description = "Chromium Web Browser";
      serviceConfig.ExecStart = "${pkgs.chromium}/bin/chromium";
      wantedBy = [ "applications.target" ];
    };
    poweralertd = {
      after = ["graphical-session.target"];
      description = "UPower-powered power alerter";
      documentation = ["man:poweralertd(1)"];
      partOf = ["graphical-session.target"];
      serviceConfig = {
        ExecStart = "${pkgs.poweralertd}/bin/poweralertd";
      };
      wantedBy = ["graphical-session.target"];
#Restart=always
    };
    swayidle =
      let
        lockcmd = "${pkgs.swaylock}/bin/swaylock";
        niricmd = "${pkgs.niri}/bin/niri msg action";
      in {
        after = ["niri.service"];
        description = "Idle manager for Wayland";
        documentation = ["man:swayidle(1)"];
        partOf = ["graphical-session.target"];
        serviceConfig = {
          ExecStart = "${pkgs.swayidle}/bin/swayidle -w \
            timeout 6000 '${niricmd} power-off-monitors' \
            after-resume '${niricmd} power-on-monitors' \
            before-sleep '${lockcmd} lock' \
            ${lockcmd}
          ";
          #Restart = "always";
        };
        unitConfig = {
          ConditionEnvironment = "WAYLAND_DISPLAY";
        };
        wantedBy = ["graphical-session.target"];
      };
    udiskie = {
      after = ["graphical-session.target" "tray.target"];
      description = "udiskie mount daemon";
      partOf = ["graphical-session.target"];
      serviceConfig = {
        ExecStart = "${pkgs.udiskie}/bin/udiskie";
      };
      requires = ["tray.target"];
      wantedBy = ["graphical-session.target"];
    };
    wpaperd = {
      after = ["graphical-session.target"];
      description = "wpaperd background image";
      partOf = ["graphical-session.target"];
      serviceConfig = {
        ExecStart= "${upkgs.wpaperd}/bin/wpaperd";
#Restart=always
        RestartSec = 10;
      };
      unitConfig = {
        ConditionEnvironment = "WAYLAND_DISPLAY";
#X-Restart-Triggers=/nix/store/zaf2ssvamkd4af6h5dib8diwlv5m31fr-wpaperd-wallpaper
      };
      wantedBy = ["graphical-session.target"];
    };
  };

  time.timeZone = "Europe/Bucharest";

  users.users.thk = {
    isNormalUser = true;
    # my former Debian groups: cdrom floppy sudo audio dip video plugdev netdev bluetooth docker nix-users
    extraGroups = [ "video" "wheel" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.fish;
    #    ignoreShellProgramCheck = true; # home-manager is responsible for ~/.config/fish
  };

  xdg.mime = {
    enable = true;
    defaultApplications = {
      "x-scheme-handler/org-protocol" = [ "emacsclient.desktop" ];
      "x-scheme-handler/mailto" = [ "chromium-browser.desktop" ];
      "text/html" = [ "chromium-incognito.desktop" ];
      "x-scheme-handler/http" = [ "chromium-incognito.desktop" ];
      "x-scheme-handler/https" = [ "chromium-incognito.desktop" ];
      "x-scheme-handler/tg" = [ "org.telegram.desktop.desktop" ];
      "x-scheme-handler/tonsite" = [ "org.telegram.desktop.desktop" ];
      "inode/directory" = ["emacsclient.desktop"];
    };
  };
}
