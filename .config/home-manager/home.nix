{ config, pkgs, ... }:

{
  home.username = "thk";
  home.homeDirectory = "/home/thk";
  home.stateVersion = "25.05";

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
    # blueman # does not show up
    (callPackage ./borg2.nix { })
    # brightnessctl # TODO: check whether udev rules are necessary?
    #cargo # rust package manager
    chromium
    #clippy # rust lint tool, see https://github.com/rust-lang/rustlings/issues/2111
    #darcs
    # davfs2
    # detox
    #devenv
    dnsutils
    # dovecot_pigeonhole
    # duff
    # evince
    (import ./emacs.nix { inherit pkgs; })
    ffmpeg
    #        freecad
    fuzzel # application launcher
    #gcc # needed for rustc
    #ghc
    git
    # git-big-picture
    # git-imerge
    #        glibcLocalesUtf8
    #        gnome.cheese
    #gnumake
    #go-mtpfs
    # gparted
    #gramps
    graphviz
    kanshi # wayland equivalent to autorandr
    kdePackages.akregator
    #kdePackages.gwenview
    #kdePackages.kgeography
    kdePackages.okular
    #kdePackages.pim-sieve-editor
    #        (pkgs.haskell-language-server.override { supportedGhcVersions = [ "96" ]; })
    #hledger
    #hledger-ui
    # html-tidy
    # jdupes
    #josh
    #jq
    #kdePackages.falkon
    #        ladybird
    #klavaro
    libnotify # provides notify-send
    libreoffice-fresh
    # libsecret # for msmtp
    #lingot
    lsof # git-annex assistant checks if files are open for writing
    #lxde.lxsession
    # lynx
    #mate.eom
    #mdbook
    #mediathekview
    mpv
    # msmtp
    # netsurf.browser
    #newsboat
    #ncdu
    nettools
    niri # tliing wayland composer
    nixpkgs-fmt
    #nmap
    #palemoon-bin # firefox fork https://www.palemoon.org/
    #pandoc
    # paperkey
    #parcellite
    pass
    #pasystray
    pavucontrol
    # powertop
    # psmisc # fuser, killall and pstree
    #pspg # postgres pager
    #pwgen # maybe replace with spwgen from pwgen-secure?
    qemu_kvm
    quasselClient
    # from tazjin
    #pkgs.quassel.override {
    #  client = true;
    #  enableDaemon = false;
    #  monolithic = false;
    #};
    # redshift
    #reuse
    #rust-analyzer
    #rustc # rust compiler
    #rustfmt
    # rustic # backup, see GitHub issue on --cacert option
    #rustup
    # sassc
    #screen-message
    #sequoia-sq
    # sshfs
    #       stack
    strace
    #sqlite
    swayidle
    swaylock
    #sweethome3d.application
    #sweethome3d.furniture-editor
    #sweethome3d.textures-editor
    telegram-desktop
    #termonad
    # thunderbird
    #tig # text mode interface to git (to browse sparse checkouts)
    tmux
    tree
    #udiskie
    #unrar-free
    #unzip
    # usbutils # provides lsusb
    vcsh
    #virtiofsd
    #vlc
    waybar # desktop bar
    wdisplays
    wget
    wl-clipboard
    wlr-randr
    #wmctrl # used in ~/bin/switch-to-emacs
    xdg-utils
    xwayland-satellite
    #xclip # used in tmux.conf
    xorg.xlsclients # find out which running apps use xwayland
    xorg.xwininfo # get X11 info by clicking on window
    yt-dlp
    #zsh
  ];

  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  programs.home-manager.enable = true;
  services.dunst.enable = true;
  programs.waybar.systemd.enable = true;
}
