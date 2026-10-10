# add a session for the rau WM on river
{ config, depot, lib, pkgs, ... }:

let
  cfg = config.programs.rau;
  channel = (pkgs.callPackage ./packages/river-channel { inherit pkgs; });

  # this session script is copy&pasted from various places and is probably
  # a worst practice type thing
  rauSession = pkgs.writeShellScript "rau-session" ''
    set -x

    # Re-exec through a login shell to get the full NixOS environment
    # (PATH, etc.) before importing it into systemd. Without this,
    # the display manager starts us with a minimal PATH.
    if [ -n "$SHELL" ] &&
       grep -q "$SHELL" /etc/shells &&
       ! (echo "$SHELL" | grep -q "false") &&
       ! (echo "$SHELL" | grep -q "nologin"); then
      if [ "$1" != '-l' ]; then
        exec bash -c "exec -l '$SHELL' -c '$0 -l $*'"
      else
        shift
      fi
    fi

    if systemctl --user -q is-active river.service; then
      echo 'rau is already running.'
      exit 1
    fi

    systemctl --user --wait start river.service

    # Force stop of graphical-session.target on exit
    #systemctl --user start --job-mode=replace-irreversibly rau-shutdown.target

    # Clean up environment
    systemctl --user unset-environment WAYLAND_DISPLAY DISPLAY XDG_SESSION_TYPE XDG_CURRENT_DESKTOP
  '';

  rauDesktop = pkgs.writeText "rau.desktop" ''
    [Desktop Entry]
    Name=rau
    Comment=rau, emacs for river
    Exec=rau-session
    Type=Application
    DesktopNames=rau
  '';

  rauSystemPackage = pkgs.runCommand "rau-system"
    {
      passthru.providedSessions = [ "rau" ];
    } ''
    install -Dm755 ${rauSession} $out/bin/rau-session
    install -Dm644 ${rauDesktop} $out/share/wayland-sessions/rau.desktop
    install -Dm644 ${rauShutdown} $out/lib/systemd/user/rau-shutdown.target
  '';

  # niri has this for some reason to ensure that things actually stop?
  # probably a classic workaround for complex and broken systemd behaviour
  rauShutdown = pkgs.writeText "rau-shutdown.target" ''
    [Unit]
    Description=Shutdown running rau session
    DefaultDependencies=no
    StopWhenUnneeded=true
    Conflicts=graphical-session.target graphical-session-pre.target
    After=graphical-session.target graphical-session-pre.target
  '';
in
{
  options.programs.rau = {
    enable = lib.mkEnableOption "rau, an Emacs window manager for river";
    rau_loglevel = lib.mkOption {
      type = lib.types.str;
      description = "log level for rau, will end up in RUST_LOG, see https://docs.rs/env_logger/latest/env_logger/";
      default = "info";
    };

    river_loglevel = lib.mkOption {
      type = lib.types.str;
      description = "log level for river, [*error*|*warning*|*info*|*debug*]";
      default = "info";
    };
  };

  config = lib.mkIf cfg.enable {
    security.polkit.enable = true;
    services.graphical-desktop.enable = true;

    systemd.packages = [ rauSystemPackage ];
    systemd.user.services.channel = {
      after = [ "graphical-session.target" ];
      description = "Channel: input config for river";
      #      documentation = ["man:poweralertd(1)"];
      partOf = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = "${channel}/bin/channel";
      };
      wantedBy = [ "rau-emacs.service" ];
    };

    services.displayManager.sessionPackages = [ rauSystemPackage ];

    environment.systemPackages = with pkgs; [
      channel
      rauSystemPackage
      river # for development and manpage
    ];
    services.autologin.leader = "${rauSession}"; # TODO this should not be here
  };
}
