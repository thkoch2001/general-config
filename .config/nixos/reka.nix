# add a session for the reka WM on river
{ config, depot, lib, pkgs, ... }:

let
  cfg = config.programs.reka;
  channel = (pkgs.callPackage ./packages/river-channel { inherit pkgs; });

  # this session script is copy&pasted from various places and is probably
  # a worst practice type thing
  rekaSession = pkgs.writeShellScript "reka-session" ''
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

    if systemctl --user -q is-active reka.service; then
      echo 'reka is already running.'
      exit 1
    fi

    systemctl --user --wait start reka.service

    # Force stop of graphical-session.target on exit
    systemctl --user start --job-mode=replace-irreversibly reka-shutdown.target

    # Clean up environment
    systemctl --user unset-environment WAYLAND_DISPLAY XDG_SESSION_TYPE XDG_CURRENT_DESKTOP
  '';

  rekaDesktop = pkgs.writeText "reka.desktop" ''
    [Desktop Entry]
    Name=reka
    Comment=reka, emacs for river
    Exec=reka-session
    Type=Application
    DesktopNames=reka
  '';

  # this is NOT a nixos unit because those have a weird environment
  rekaUnit = pkgs.writeText "reka.service" ''
    [Unit]
    Description=reka, emacs for river
    Wants=pipewire.service graphical-session-pre.target
    After=pipewire.service graphical-session-pre.target

    [Service]
    Slice=session.slice
    Type=simple
    WorkingDirectory=%h
    Environment=REKA_LOGLEVEL=${cfg.reka_loglevel}
    ExecStart=${pkgs.river}/bin/river -log-level ${cfg.river_loglevel}
  '';

  rekaSystemPackage = pkgs.runCommand "reka-system"
    {
      passthru.providedSessions = [ "reka" ];
    } ''
    install -Dm755 ${rekaSession} $out/bin/reka-session
    install -Dm644 ${rekaDesktop} $out/share/wayland-sessions/reka.desktop
    install -Dm644 ${rekaUnit} $out/lib/systemd/user/reka.service
    install -Dm644 ${rekaShutdown} $out/lib/systemd/user/reka-shutdown.target
  '';

  # niri has this for some reason to ensure that things actually stop?
  # probably a classic workaround for complex and broken systemd behaviour
  rekaShutdown = pkgs.writeText "reka-shutdown.target" ''
    [Unit]
    Description=Shutdown running reka session
    DefaultDependencies=no
    StopWhenUnneeded=true
    Conflicts=graphical-session.target graphical-session-pre.target
    After=graphical-session.target graphical-session-pre.target
  '';
in
{
  options.programs.reka = {
    enable = lib.mkEnableOption "reka, an Emacs window manager for river";
    reka_loglevel = lib.mkOption {
      type = lib.types.str;
      description = "log level for reka, will end up in RUST_LOG, see https://docs.rs/env_logger/latest/env_logger/";
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

    systemd.packages = [ rekaSystemPackage ];
    systemd.user.services.channel = {
      after = [ "graphical-session.target" ];
      description = "Channel: input config for river";
      #      documentation = ["man:poweralertd(1)"];
      partOf = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = "${channel}/bin/channel";
      };
      wantedBy = [ "reka-emacs.service" ];
    };

    services.displayManager.sessionPackages = [ rekaSystemPackage ];

    environment.systemPackages = [
      channel
      rekaSystemPackage
    ];
    services.autologin.leader = "${rekaSession}"; # TODO this should not be here
  };
}
