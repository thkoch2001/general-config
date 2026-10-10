{ pkgs, upkgs, ... }:
let
  reka = import (pkgs.fetchgit {
    url = "https://codeberg.org/tazjin/reka.git";
    rev = "refs/heads/canon";
    hash = "sha256-jdggakCHHRNpRQUjpHHHWyBsU1SL61QIWZMV6A0IWfU=";
  });
  rau = import (pkgs.fetchFromGitHub {
    owner = "thkoch2001";
    repo = "rau";
    rev = "refs/heads/master";
    hash = "sha256-mEzor0AqARGeLUUcjGlMAIYmhkOOOV3YkYAEQKKodS8=";
  });

in
with pkgs; (emacsPackagesFor upkgs.emacs31-pgtk).emacsWithPackages (
  epkgs:
  (with epkgs.elpaPackages; [
    ace-window
    company
    inspector
    # org # included in emacs
    # org-modern # https://github.com/minad/org-modern
    tree-inspector
  ]) ++ (with epkgs.melpaPackages; [
    apache-mode
    atomic-chrome
    casual-suite
    ednc # TODO https://github.com/sinic/ednc
    elfeed
    elisp-def # TODO https://github.com/Wilfred/elisp-def
    envrc
    git-auto-commit-mode
    graphviz-dot-mode
    haskell-mode
    helpful
    keycast # TODO https://github.com/tarsius/keycast
    magit
    markdown-mode
    mixed-pitch
    nerd-icons
    nix-mode
    nov
    ollama-buddy
    package-lint
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
    zig-mode
  ]) ++ (with epkgs.nongnuPackages; [
    eat # emulate a terminal https://codeberg.org/akib/emacs-eat
    # TODO
    # https://www.grothe.us/presentations/emacsconf-202512-emacs-private-ai.pdf
    gptel
    ws-butler
  ]) ++ [
    (callPackage reka {
      inherit pkgs;
    })
    (callPackage rau {
      inherit pkgs;
    })
    # TODO
    # https://github.com/dakra/ghostel
    # https://dakra.github.io/ghostel/
    upkgs.emacsPackages.ghostel
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
