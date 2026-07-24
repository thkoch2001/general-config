{ pkgs, ... }:
let
  reka = import (pkgs.fetchgit {
    url = "https://codeberg.org/tazjin/reka.git";
    rev = "refs/heads/canon";     # 90e89c2f51240e3d0a10d98e0cc61732bd86334e
    hash = "sha256-w25BZpbKMAACFweRqNEtGDvw/sNNJUfsVJQCXh41Y6w=";
  });
in
with pkgs; (emacsPackagesFor emacs-pgtk).emacsWithPackages (
  epkgs:
  (with epkgs.elpaPackages; [
    ace-window
    company
    org
    # org-modern # https://github.com/minad/org-modern
    use-package
  ]) ++ (with epkgs.melpaPackages; [
    apache-mode
    atomic-chrome
    casual-suite
    ednc # TODO https://github.com/sinic/ednc
    envrc
    git-auto-commit-mode
    graphviz-dot-mode
    haskell-mode
    helpful
    keycast # TODO https://github.com/tarsius/keycast
    magit
    markdown-mode
    nix-mode
    nov
    paredit
    rainbow-delimiters
    rust-mode
    solarized-theme
    systemd
    telega
    treemacs
    visual-fill-column
    yaml-mode
    yasnippet-snippets
  ]) ++ (with epkgs.nongnuPackages; [
    eat # emulate a terminal https://codeberg.org/akib/emacs-eat
    ws-butler
  ]) ++ [
    (callPackage reka {
      inherit pkgs;
    })
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
)
