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
    # blueman # does not show up
    # brightnessctl # TODO: check whether udev rules are necessary?
    #cargo # rust package manager
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
    #kdePackages.akregator
    #kdePackages.gwenview
    #kdePackages.kgeography
    #kdePackages.okular
    #kdePackages.pim-sieve-editor
    #        (pkgs.haskell-language-server.override { supportedGhcVersions = [ "96" ]; })
    #hledger
    #hledger-ui
    # html-tidy
    htop
    # jdupes
    #josh
    #jq
    #kdePackages.falkon
    #        ladybird
    #klavaro
    #libreoffice-fresh
    # libsecret # for msmtp
    #lingot
    lsof # git-annex assistant checks if files are open for writing
    #lxde.lxsession
    # lynx
    #mate.eom
    #mdbook
    #mediathekview
    #mpv
    # msmtp
    # netsurf.browser
    #newsboat
    #ncdu
    nettools
    nixpkgs-fmt
    #nmap
    #palemoon-bin # firefox fork https://www.palemoon.org/
    #pandoc
    # paperkey
    #parcellite
    #pass
    #pasystray
    #pavucontrol
    # powertop
    # psmisc # fuser, killall and pstree
    #pspg # postgres pager
    #pwgen # maybe replace with spwgen from pwgen-secure?
    #quasselClient
    # redshift
    #reuse
    #rust-analyzer
    #rustc # rust compiler
    #rustfmt
    #rustup
    # sassc
    #screen-message
    #sequoia-sq
    # sshfs
    #       stack
    #strace
    #sqlite
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
    #vcsh
    #virtiofsd
    #vlc
    # wget
    #wmctrl # used in ~/bin/switch-to-emacs
    #xclip # used in tmux.conf
    #xmonad-with-packages
    #        xpra
    #xsecurelock
    yt-dlp
    #zsh
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. These will be explicitly sourced when using a
  # shell provided by Home Manager. If you don't want to manage your shell
  # through Home Manager then you have to manually source 'hm-session-vars.sh'
  # located at either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/thk/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  programs.home-manager.enable = true;
}
