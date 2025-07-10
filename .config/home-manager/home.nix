{ lib, config, pkgs, ... }:
let
  font = "DejaVu Sans 12";
in
{
  nix = {
    enable = true;
    settings = {
      extra-experimental-features = ["flakes" "nix-command"];
      use-xdg-base-directories = true;
    };
  };
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
    preferXdgDirectories = true;
    stateVersion = "25.05";
    sessionPath = [
      "$HOME/.local/bin"
      "$HOME/bin"
    ];
    username = "thk";
  };

  # TODO:
  # get rid of chromium
  # https://github.com/crossbowerbt/dillo-plus/
  # https://sr.ht/~bptato/chawan/
  home.packages = with pkgs; [
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
    bc # cli calculator
    # blueman
    (callPackage ./borg2.nix { })
    # cargo # rust package manager
    chromium # ungoogled-chromium didn't work
    # clippy # rust lint tool, see https://github.com/rust-lang/rustlings/issues/2111
    copyq # clipboard manager
    # darcs
    # davfs2
    # detox
    # devenv
    dnsutils
    # dovecot_pigeonhole
    # duff
    # evince
    (import ./emacs.nix { inherit pkgs; })
    ffmpeg
    # freecad
    fuzzel # application launcher
    gcc # needed for rustc
    # ghc
    gimp
    gitFull # To also get gitk
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
    # josh # monorepos with git
    # jq
    kanshi # TODO like autorandr but for wayland
    # kdePackages.falkon # webbrowser
    # ladybird # webbrowser C++, needs to be installed via NixOS
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
    # pandoc
    # paperkey
    # parcellite
    pass
    # pasystray
    pavucontrol
    # powertop
    # psmisc # fuser, killall and pstree
    # pspg # postgres pager
    # pwgen # maybe replace with spwgen from pwgen-secure?
    qemu_kvm
    quasselClient
    # from tazjin
    # pkgs.quassel.override {
    #   client = true;
    #   enableDaemon = false;
    #   monolithic = false;
    # };
    # redshift
    # reuse
    # rust-analyzer
    # rustc # rust compiler
    # rustfmt
    # rustic # backup, see GitHub issue on --cacert option
    rustup
    # sassc
    # screen-message
    # sequoia-sq
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
    # thunderbird
    # tig # text mode interface to git (to browse sparse checkouts)
    tmux
    tree
    # unrar-free
    unzip
    # usbutils # provides lsusb
    vcsh
    # virtiofsd
    vlc
    waybar # desktop bar
    wdisplays
    wget
    wl-clipboard
    wlr-randr
    xdg-utils # xdg-open
    xwayland-satellite
    xorg.xlsclients # find out which running apps use xwayland
    xorg.xwininfo # get X11 info by clicking on window
    yt-dlp
  ];

  home.activation.gitignore-for-vcsh = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    (cd $newGenPath/home-files && run find . \( -type f -or -type l \) -printf "%h/%f\n" | cut -c 2- > "$HOME/.gitignore" )
    run echo "/.gitignore" >> "$HOME/.gitignore"
  '';

  programs = {
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
        { timeout = 120; command = "${niricmd} power-off-monitors"; }
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
    };
  };

  systemd.user.services.swayidle.Unit.After = lib.mkForce [ "niri.service" ];
  systemd.user.startServices = false; # This sd-switch thing has no documentation.
  systemd.user.services.chromium = {
    Unit.Description = "Chromium Web Browser";
    Service.ExecStart = "${pkgs.chromium}/bin/chromium --gtk-version=4 --enable-features=UseOzonePlatform --ozone-platform=wayland --use-gl=desktop --enable-features=VaapiVideoDecoder --enable-accelerated-video-decode";
    Install.WantedBy = ["applications.target"];
  };
}
