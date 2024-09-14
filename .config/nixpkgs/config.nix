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
        editorconfig
        flycheck
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
#        blueman
        brightnessctl # TODO: check whether udev rules are necessary?
        dnsutils
        feh
        ffmpeg
        flameshot
#        freecad
        ghc
        git
        glibcLocalesUtf8
        gmrun
#        gnome.cheese
        go-mtpfs
#        (pkgs.haskell-language-server.override { supportedGhcVersions = [ "96" ]; })
        htop
        #        ladybird
        klavaro
        lingot
        lxde.lxsession
        mate.eom
        mediathekview
        mpv
        ncdu
        nix
        nixGLIntel
        nmap
        # nix-channel --add https://github.com/guibou/nixGL/archive/main.tar.gz nixgl
        # see https://github.com/nix-community/nixGL
        # necessary e.g. for FreeCAD
        #nixgl.nixGLIntel
 #       pandoc
        parcellite
        pass
        pasystray
        pavucontrol
        screen-message
 #       stack
        strace
        sqlite
        #sweethome3d.application
        #sweethome3d.furniture-editor
        #sweethome3d.textures-editor
        telegram-desktop
        thk-emacsWithPackages
        tmux
        tree
        udiskie
        vcsh
        virtiofsd
        vlc
        wmctrl # used in ~/bin/switch-to-emacs
        xclip # used in tmux.conf
        xsecurelock
        yt-dlp
        zsh
      ];
    };
  };
}
