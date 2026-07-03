{ lib, config, pkgs, ... }:
let
  upkgs = (import <nixpkgs-unstable> {}).pkgs;
in
{
  nix = {
    enable = true;
    settings = {
      extra-experimental-features = [ "flakes" "nix-command" ];
      use-xdg-base-directories = true;
    };
  };
  /*nixpkgs.overlays = [
    (final: prev:
      {
        # yt-dlp = (pkgs.callPackage (import ./packages/yt-dlp.nix) {});
      })
  ];*/
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


  home.activation.gitignore-for-vcsh = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    (cd $newGenPath/home-files && run find . \( -type f -or -type l \) -printf "%h/%f\n" | cut -c 2- > "$HOME/.gitignore" )
    run echo "/.gitignore" >> "$HOME/.gitignore"
  '';

  programs = {
    home-manager.enable = false;
  };

  services = {
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
      events = {
        "before-sleep" = lockcmd;
        "lock" = lockcmd;
        "after-resume" = "${niricmd} power-on-monitors";
      };
      timeouts = [
        { timeout = 6000; command = "${niricmd} power-off-monitors"; }
      ];
      # extraArgs = ["-d"];
    };

  services.wpaperd = {
    enable = true;
    package = upkgs.wpaperd;
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
}
