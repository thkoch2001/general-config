{ config, lib, pkgs, ... }:
let
  upkgs = (import <nixpkgs-unstable> { }).pkgs;
  thk-emacs = (import ./emacs.nix { inherit pkgs upkgs; });
  thk-nur = import
    (pkgs.fetchFromGitHub {
      owner = "thkoch2001";
      repo = "nur-packages";
      rev = "e3afcb25c7539d47b52023057b7f6f99cd025af3";
      hash = "sha256-pi9pakM7MujZZktPp7P5EJhEINPT3sFs2/H0mnJhdBQ=";
    })
    { inherit pkgs; };
  river-keyboard-layout-listener = pkgs.callPackage
    (pkgs.fetchFromGitHub {
      owner = "thkoch2001";
      repo = "river-keyboard-layout-listener";
      rev = "master";
      hash = "sha256-qepkKHc051GN6Kx4hLOg6FepUuu29IGxQH7YJNW0sN8=";
    }){};
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
      ./rau.nix
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
      EDITOR = "emacsclient -a vi";
    };
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
        audacity
        (aspellWithDicts (d: [ d.de d.en d.ro d.ru ]))
        bat # syntax highlighting cat https://github.com/sharkdp/bat
        bc # cli calculator
        (callPackage ./borg2.nix { })
        brightnessctl
        # cargo # rust package manager
        chawan # TUI browser https://sr.ht/~bptato/chawan/
        chromium # needed although programs.enabled.chromius = true; ungoogled-chromium didn't work
        # clippy # rust lint tool, see https://github.com/rust-lang/rustlings/issues/2111
        copyq # clipboard manager
        # darcs
        # davfs2
        # detox
        # devenv
        dillo
        dillo-plus
        dnsutils
        # dovecot_pigeonhole
        # duff
        e2fsprogs # e4crypt
        eom
        evince
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
        inhibridge
        kanshi # wayland equivalent to autorandr
        kdePackages.akregator
        kdePackages.gwenview
        kdePackages.kdenlive
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
        # ladybird # webbrowser C++, needs to be installed via module?
        # klavaro
        libnotify # provides notify-send
        libreoffice-fresh
        libwebp # for dwebp for telegram emacs client telega, to display stickers
        # libsecret # for msmtp
        # lingot
        linuxPackages.v4l2loopback # video camera recording with obs-studio
        lsof # git-annex assistant checks if files are open for writing
        # lxde.lxsession
        # lynx
        man-pages
        man-pages-posix
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
        nixpkgs-fmt
        # nix-prefetch-github
        # nmap
        obs-studio
        ollama
        openrsync
        # palemoon-bin # firefox fork https://www.palemoon.org/
        pandoc # e.g. markdown preview in emacs
        # paperkey
        # parcellite
        pass
        pavucontrol
        pinentry-qt # provided here, see ~/.gnupg/gpg-agent.conf
        # convert PDF to png images with
        # pdftoppm -png -r 300 $PDF $PREFIX
        poppler-utils
        # powertop
        # psmisc # fuser, killall and pstree
        pspg # postgres pager
        # pwgen # maybe replace with spwgen from pwgen-secure?
        # qemu_kvm # qickemu should bring in qemu_full
        #qgnomeplatform-qt6 # dark-mode for e.g. akregator, see https://people.debian.org/~werdahias/enforcing-darkmode-for-qt-programs-under-a-non-qt-based-environentm.html     #adwaita-qt6
        qt5.qtwayland
        quasselClient
        # from tazjin
        # pkgs.quassel.override {
        #   client = true;
        #   enableDaemon = false;
        #   monolithic = false;
        # };
        quickemu # Quickly create and run optimised Windows, macOS and Linux virtual machines
        qutebrowser
        # redshift
        # reuse
        river
        river-keyboard-layout-listener
        # rust-analyzer
        # rustc # rust compiler
        # rustfmt
        # rustic # backup, see GitHub issue on --cacert option
        rustup
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
        thk-emacs
        # thunderbird
        # tig # text mode interface to git (to browse sparse checkouts)
        tmux
        traceroute
        translate-shell # Command-line translator using Google Translate, Bing Translator, Yandex.Translate, and Apertium
        # https://github.com/soimort/translate-shell/wiki/Configuration
        tree
        udiskie
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
        wl-screenrec
        wlopm # wayland output power management, used by swayidle, swaylock
        wlr-randr
        wlr-which-key
        wpaperd
        xdg-utils # xdg-open
        xwayland-satellite
        xlsclients # find out which running apps use xwayland
        xwininfo # get X11 info by clicking on window
        yt-dlp
        thk-nur.yt2feed
        zip
        upkgs.zmx
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

  nixpkgs = {
    config.permittedInsecurePackages = [
    ];

    overlays = [
      (final: prev: {
        ladybird = upkgs.ladybird;
        river = upkgs.river;
        wpaperd = upkgs.wpaperd;
        yt-dlp = upkgs.yt-dlp;
      })
      (import ./lib/autopatch.nix { inherit lib; patchesDir = ./patches; })
    ];
  };

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
            "org/gnome/desktop/interface" = {
              color-scheme = "'prefer-dark'";
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
    rau = {
      enable = lib.mkDefault true;
      #rau_loglevel = "debug"; # defaults to info
      #river_loglevel = "debug"; # defaults to info
    };
    # - `programs.ssh.startAgent' (defined in `/home/thk/.config/nixos/configuration.nix') and `services.gnome.gcr-ssh-agent.enable' (defined in `/nix/store/snl5y2ljnxfaz7a7hih2f67n1wvj69jk-nixos-26.05/nixos/nixos/modules/services/desktops/gnome/gcr-ssh-agent.nix') cannot both be enabled at the same time.
    # These options conflict because only one SSH agent can be installed at a time.
    ssh.startAgent = true;
    waybar.enable = true;
    xwayland.enable = true;
  };

  # from tazjin
  security = {
    pam.services.swaylock = { };
    polkit.enable = true; # TODO why? taken from reka module
    sudo.extraConfig = "Defaults insults,pwfeedback";
  };

  services = {
    autologin = {
      enable = true;
      user = "thk";
    };
    blueman.enable = true;
    #    displayManager.gdm.enable = true;
    dunst = {
      enable = true;
      settings = {
        global.font = "DejaVu Sans 12";
      };
    };
    gnome.gcr-ssh-agent.enable = lib.mkForce false; # seems this gets enabled by niri?
    graphical-desktop.enable = true;
    libinput.enable = true;
    ollama = {
      enable = true;
      environmentVariables = {
        OLLAMA_KEEP_ALIVE = "120m";
      };
      #loadModels
      #syncModels
      #alias ollama-flush='ollama run --keep-alive 0s' ?
    };
    openssh.enable = false;
    pcscd.enable = true;
    pipewire = {
      enable = true;
      pulse.enable = true;
    };
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
    niri.configuration = {
      programs = {
        niri.enable = true;
        rau.enable = false;
      };
      services = {
        autologin.leader = "${pkgs.niri}/bin/niri-session";
      };
      systemd.user.services = {
        swayidle =
          let
            lockcmd = "${pkgs.swaylock}/bin/swaylock";
            niricmd = "${pkgs.niri}/bin/niri msg action";
          in
            {
              after = [ "niri.service" ];
              description = "Idle manager for Wayland";
              documentation = [ "man:swayidle(1)" ];
              partOf = [ "graphical-session.target" ];
              serviceConfig = {
                ExecStart = ''${pkgs.swayidle}/bin/swayidle -w \
                            timeout 180 '${niricmd} power-off-monitors' \
                                        resume '${niricmd} power-on-monitors' \
                                                    '';
                #Restart = "always";
              };
              unitConfig = {
                ConditionEnvironment = "WAYLAND_DISPLAY";
              };
              wantedBy = [ "graphical-session.target" ];
            };
        waybar.serviceConfig = {
          ExecStart = [ "" "${pkgs.waybar}/bin/waybar -c %h/.config/waybar/config.niri.jsonc" ];
        };
      };
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

  systemd.user.services = {
    chromium = {
      description = "Chromium Web Browser";
      serviceConfig.ExecStart = "${pkgs.chromium}/bin/chromium";
      wantedBy = [ "applications.target" ];
    };
    copyq = {
      description = "CopyQ Clipboard manager";
      after = [ "niri.service" "rau.service" ];
      wantedBy = [ "graphical-session.target" ];
      serviceConfig.ExecStart = "${pkgs.copyq}/bin/copyq";
    };
    inhibridge = {
      description = "bridge freedesktop.org ScreenSaver inhibitions to systemd-inhibit";
      after = [ "niri.service" "rau.service" ];
      wantedBy = [ "graphical-session.target" ];
      serviceConfig.ExecStart = "${pkgs.inhibridge}/bin/inhibridge";
    };
    kanshi = {
      description = "automatically apply display configs";
      # After niri to ensure WAYLAND_DISPLAY is set
      after = [ "niri.service" "rau.service" ];
      wantedBy = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.kanshi}/bin/kanshi";
        ExecReload = "kill -HUP $MAINPID";
      };
    };
    pasystray = {
      description = "PulseAudio systray";
      after = [ "niri.service" "rau.service" ];
      wantedBy = [ "graphical-session.target" ];
      serviceConfig.ExecStart = "${pkgs.pasystray}/bin/pasystray";
    };
    poweralertd = {
      after = [ "graphical-session.target" ];
      description = "UPower-powered power alerter";
      documentation = [ "man:poweralertd(1)" ];
      partOf = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.poweralertd}/bin/poweralertd";
      };
      wantedBy = [ "graphical-session.target" ];
      #      restart = "always";
    };
    swayidle =
      let
        lockcmd = "${pkgs.swaylock}/bin/swaylock";
        wlopmcmd = "${pkgs.wlopm}/bin/wlopm";
      in
        lib.mkDefault
        {
          description = "Idle manager for Wayland";
          documentation = [ "man:swayidle(1)" ];
          partOf = [ "graphical-session.target" ];
          serviceConfig = {
            ExecStart = ''${pkgs.swayidle}/bin/swayidle -w \
                          timeout 180 '${wlopmcmd} --off "*"' \
                          resume '${wlopmcmd} --on "*"' \
                        '';
            #Restart = "always";
          };
          unitConfig = {
            ConditionEnvironment = "WAYLAND_DISPLAY";
          };
          wantedBy = [ "graphical-session.target" ];
        };
    udiskie = {
      after = [ "graphical-session.target" ];
      description = "udiskie automount daemon";
      partOf = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.udiskie}/bin/udiskie --automount --no-config --notify --tray --appindicator";
      };
      #      requires = [ "tray.target" ];
      wantedBy = [ "graphical-session.target" ];
    };
    wpaperd = {
      after = [ "graphical-session.target" ];
      description = "wpaperd background image";
      partOf = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.wpaperd}/bin/wpaperd -v";
        Restart= "always";
        RestartSec = 4;
        RestartSteps = 3;
        RestartMaxDelaySec = 60;
      };
      unitConfig = {
        #TODO ConditionEnvironment = "XDG_CURRENT_DESKTOP=niri";
        #X-Restart-Triggers=/nix/store/zaf2ssvamkd4af6h5dib8diwlv5m31fr-wpaperd-wallpaper
      };
      wantedBy = [ "graphical-session.target" ];
    };
  };

  time.timeZone = "Europe/Bucharest";

  users.users.thk = {
    isNormalUser = true;
    extraGroups = [ "input" "video" "wheel" ];
    shell = pkgs.fish;
  };

  users.users.test = {
    isNormalUser = true;
    extraGroups = [ "input" "video" "wheel" ];
    shell = pkgs.fish;
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
      "inode/directory" = [ "emacsclient.desktop" ];
    };
  };

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-gtk
      xdg-desktop-portal-gnome
      xdg-desktop-portal-wlr
    ];

    config = {
      # niri ships its own niri-portals.conf, nothing needed
      # TODO: configure this for rau session
      rau.default = [ "wlr" ];
      rau."org.freedesktop.impl.portal.ScreenCast" = [ "wlr" ];
    };
  };
}
