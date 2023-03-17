with (import <nixpkgs> {});
{
  packageOverrides = pkgs: with pkgs; {
    thk-emacsWithPackages = (pkgs.emacsPackagesFor emacs-gtk).emacsWithPackages (
      epkgs:
      (with epkgs.elpaPackages; [
        ace-window
        company
        org
        use-package
      ]) ++ (with epkgs.melpaPackages; [
        editorconfig
        flycheck
        git-auto-commit-mode
        graphviz-dot-mode
        haskell-mode
        helpful
        magit
        markdown-mode
        nov
        paredit
        rainbow-delimiters
        solarized-theme
        systemd
        treemacs
        visual-fill-column
        ws-butler
        yasnippet-snippets
      ]) ++ [    # From main packages set
      ]
    );
    userPackages = buildEnv {
#      inherit ((import <nixpkgs/nixos> {}).config.system.path)
#      pathsToLink ignoreCollisions postBuild;
      extraOutputsToInstall = [ "doc" "info" "man" ];
      name = "user-packages";
      paths = [
              thk-emacsWithPackages
              vcsh
      ];
    };
  };
}