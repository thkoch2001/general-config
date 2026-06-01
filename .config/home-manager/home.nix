{ lib, config, pkgs, ... }:
let
  font = "DejaVu Sans 12";
  unstable = import <nixpkgs-unstable> {};
  upkgs = unstable.pkgs;
  thk-nur = import (pkgs.fetchFromGitHub {
    owner = "thkoch2001";
    repo = "nur-packages";
    rev = "e3afcb25c7539d47b52023057b7f6f99cd025af3";
    hash = "sha256-pi9pakM7MujZZktPp7P5EJhEINPT3sFs2/H0mnJhdBQ=";
  }) {inherit pkgs;};
in
{
  nix = {
    enable = true;
    settings = {
      extra-experimental-features = [ "flakes" "nix-command" ];
      use-xdg-base-directories = true;
    };
  };
  nixpkgs.overlays = [
    (final: prev:
      {
        wlr-which-key = upkgs.wlr-which-key.overrideAttrs (old: {
          patches = (old.patches or [ ]) ++ [
            ./patches/wlr-which-key_fix_26.patch
          ];
        });
        wpaperd = prev.wpaperd.overrideAttrs (old: {
          patches = (old.patches or [ ]) ++ [
            ./patches/wpaperd_fix_140.patch
          ];
        });
        # yt-dlp = (pkgs.callPackage (import ./packages/yt-dlp.nix) {});
      })
  ];
  dconf.settings = {
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

  home = {
    homeDirectory = "/home/thk";
    pointerCursor = {
      # https://github.com/YaLTeR/niri/issues/2339
      # https://github.com/YaLTeR/niri/wiki/Configuration:-Miscellaneous#cursor
      # https://nix-community.github.io/home-manager/options.xhtml#opt-home.pointerCursor
      enable = true;
      package = pkgs.vanilla-dmz;
      gtk.enable = true;
      name = "Vanilla-DMZ";
      size = 48;
      x11.enable = true;
    };
    preferXdgDirectories = true;
    stateVersion = "25.05";
    sessionPath = [
      "$HOME/.local/bin"
      "$HOME/bin"
    ];
    sessionVariables = {
      NIXOS_OZONE_WL = "1";
    };
    username = "thk";
  };

  # TODO:
  # get rid of chromium
  # https://github.com/crossbowerbt/dillo-plus/
  # https://sr.ht/~bptato/chawan/
  home.packages = with pkgs;
    let
      # https://nixos.wiki/wiki/TexLive
      # https://gitlab.com/Kaligule/org-to-pdf/-/blob/master/shell.nix
      tex_for_orgmode = (texlive.combine {
        inherit (pkgs.texlive) scheme-basic
          dvisvgm dvipng # for preview and export as html
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
    # chromium # enabled; ungoogled-chromium didn't work
    # clippy # rust lint tool, see https://github.com/rust-lang/rustlings/issues/2111
    copyq # clipboard manager
    # darcs
    # davfs2
    # detox
    # devenv
    dnsutils
    # dovecot_pigeonhole
    # duff
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
    mate.eom
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
    # palemoon-bin # firefox fork https://www.palemoon.org/
    pandoc # e.g. markdown preview in emacs
    # paperkey
    # parcellite
    pass
    # pasystray
    pavucontrol
    # powertop
    # psmisc # fuser, killall and pstree
    pspg # postgres pager
    # pwgen # maybe replace with spwgen from pwgen-secure?
    # qemu_kvm # qickemu should bring in qemu_full
    #qgnomeplatform-qt6 # dark-mode for e.g. akregator, see https://people.debian.org/~werdahias/enforcing-darkmode-for-qt-programs-under-a-non-qt-based-environment.html     #adwaita-qt6
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
    # virtiofsd
    vlc
    watchexec
    wavemon
    waybar # desktop bar
    wdisplays
    wget
    wl-clipboard
    wlr-randr
    wlr-which-key
    xdg-utils # xdg-open
    xwayland-satellite
    xorg.xlsclients # find out which running apps use xwayland
    xorg.xwininfo # get X11 info by clicking on window
    yt-dlp
    thk-nur.yt2feed
    zip
  ];

  home.activation.gitignore-for-vcsh = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    (cd $newGenPath/home-files && run find . \( -type f -or -type l \) -printf "%h/%f\n" | cut -c 2- > "$HOME/.gitignore" )
    run echo "/.gitignore" >> "$HOME/.gitignore"
  '';

  programs = {
    chromium = {
      enable = true;
      package = upkgs.chromium;
    };
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
    firefox = {
      enable = true;
      package = pkgs.librewolf;
    };
    fish.enable = true;
    home-manager.enable = true;
    man = {
      enable = true;
      generateCaches = true; # Needed for fish manpage completion
    };
    waybar.systemd.enable = true;
  };

  services = {
    dunst = {
      enable = true;
      settings = {
        global.font = font;
      };
    };
    poweralertd.enable = true;
    udiskie.enable = true;
  };

  services.swayidle =
    let
      lockcmd = "${pkgs.swaylock}/bin/swaylock";
      niricmd = "${pkgs.niri}/bin/niri msg action";
    in
    {
      enable = true;
      events = [
        { event = "before-sleep"; command = lockcmd; }
        { event = "lock"; command = lockcmd; }
        { event = "after-resume"; command = "${niricmd} power-on-monitors"; }
      ];
      timeouts = [
        { timeout = 6000; command = "${niricmd} power-off-monitors"; }
      ];
      # extraArgs = ["-d"];
    };

  services.wpaperd = {
    enable = true;
    settings = {
      default = {
        duration = "1d";
        mode = "fit";
        sorting = "random";
      };
      any.path = "~/.local/share/wallpapers";
    };
  };

  xdg.mimeApps = {
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

  systemd.user.services.swayidle.Unit.After = lib.mkForce [ "niri.service" ];
  systemd.user.startServices = false; # This sd-switch thing has no documentation.
  systemd.user.services.chromium = {
    Unit.Description = "Chromium Web Browser";
    Service.ExecStart = "${pkgs.chromium}/bin/chromium";
    Install.WantedBy = [ "applications.target" ];
  };
}
