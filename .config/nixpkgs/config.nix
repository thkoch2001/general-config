with (import <nixpkgs> {});
with (import <nixgl> {}); /* https://github.com/nix-community/nixGL */
let
  thk-emacsWithPackages = (pkgs.emacsPackagesFor emacs-gtk).emacsWithPackages (
    epkgs:
      (with epkgs.elpaPackages; [
        ace-window
        company
        org
        use-package
      ]) ++ (with epkgs.melpaPackages; [
        apache-mode
        atomic-chrome
        git-auto-commit-mode
        graphviz-dot-mode
        haskell-mode
        helpful
        magit
        markdown-mode
        nix-mode
        nov
        paredit
        rainbow-delimiters
        rust-mode
        solarized-theme
        systemd
        treemacs
        visual-fill-column
        ws-butler
        yaml-mode
        yasnippet-snippets
      ]) ++ [    # From main packages set
    ]
/*
pandoc-mode?

Debian emacs packages not yet installed by nix:
elpa-avy
elpa-bar-cursor
elpa-bind-key
elpa-bm
elpa-dap-mode
elpa-dash
elpa-debian-el
elpa-diminish
elpa-dpkg-dev-el
elpa-elisp-refs
elpa-epl
elpa-esxml
elpa-git-modes
elpa-gitattributes-mode
elpa-gitconfig-mode
elpa-gitignore-mode
elpa-ht
elpa-htmlize
elpa-hydra
elpa-jinja2-mode
elpa-let-alist
elpa-lsp-haskell
elpa-lsp-java
elpa-lsp-mode
elpa-lsp-treemacs
elpa-lsp-ui
elpa-lv
elpa-magit-annex
elpa-org-drill
elpa-php-mode
elpa-pkg-info
elpa-posframe
elpa-projectile
elpa-rainbow-mode
elpa-request
elpa-s
elpa-seq
elpa-spinner
elpa-system-packages
elpa-world-time-mode
elpa-yasnippet-snippets
elpa-git-commit
elpa-magit
elpa-magit-section
auctex
*/
  );
in
{
  packageOverrides = pkgs: with pkgs; {
    userPackages = buildEnv {
#      inherit ((import <nixpkgs/nixos> {}).config.system.path)
#      pathsToLink ignoreCollisions postBuild;
      extraOutputsToInstall = [ "doc" "info" "man" ];
      name = "user-packages";
      paths = [
        arandr
        # blueman # does not show up
        brightnessctl # TODO: check whether udev rules are necessary?
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
        feh
        ffmpeg
        flameshot
#        freecad
        #gcc # needed for rustc
        #ghc
        git
        # git-big-picture
        # git-imerge
        glibcLocalesUtf8
        gmrun
        #        gnome.cheese
        gnumake
        go-mtpfs
        # gparted
        gramps
        graphviz
        kdePackages.akregator
        kdePackages.gwenview
        #kdePackages.kgeography
        kdePackages.okular
        #kdePackages.pim-sieve-editor
        gxkb
        haskellPackages.gtk-sni-tray
        haskellPackages.status-notifier-item
        #        (pkgs.haskell-language-server.override { supportedGhcVersions = [ "96" ]; })
        hledger
        hledger-ui
        # html-tidy
        htop
        # jdupes
        josh
        jq
        #kdePackages.falkon
        kdePackages.plasma-workspace # for xembedsniproxy
        #        ladybird
        #klavaro
        libreoffice-fresh
        # libsecret # for msmtp
        lingot
        lsof # git-annex assistant checks if files are open for writing
        lxde.lxsession
        # lynx
        mate.eom
        #mdbook
        #mediathekview
        mpv
        # msmtp
        # netsurf.browser
        #newsboat
        ncdu
        nettools
        nix
        # nix-channel --add https://github.com/guibou/nixGL/archive/main.tar.gz nixgl
        # see https://github.com/nix-community/nixGL
        # necessary e.g. for FreeCAD
        nixGLIntel
        nixpkgs-fmt
        nmap
        #palemoon-bin # firefox fork https://www.palemoon.org/
        pandoc
        # paperkey
        parcellite
        pass
        pasystray
        pavucontrol
        # powertop
        # psmisc # fuser, killall and pstree
        pspg # postgres pager
        pwgen # maybe replace with spwgen from pwgen-secure?
        quasselClient
        # redshift
        #reuse
        #rust-analyzer
        #rustc # rust compiler
        #rustfmt
        rustup
        # sassc
        screen-message
        #sequoia-sq
        # sshfs
 #       stack
        strace
        sqlite
        #sweethome3d.application
        #sweethome3d.furniture-editor
        #sweethome3d.textures-editor
        taffybar
        telegram-desktop
        #termonad
        thk-emacsWithPackages
        # thunderbird
        tig # text mode interface to git (to browse sparse checkouts)
        tmux
        tree
        udiskie
        unrar-free
        unzip
        # usbutils # provides lsusb
        vcsh
        virtiofsd
        vlc
        # wget
        wmctrl # used in ~/bin/switch-to-emacs
        xclip # used in tmux.conf
        xmonad-with-packages
#        xpra
        xsecurelock
        yt-dlp
        zsh
      ];
    };
  };
}
