# add a session for the reka WM on river
{ config, depot, lib, pkgs, ... }:

let
  cfg = config.programs.reka;

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

    systemctl --user import-environment XDG_SESSION_TYPE XDG_CURRENT_DESKTOP
    if command -v dbus-update-activation-environment >/dev/null 2>&1; then
      dbus-update-activation-environment --all
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

  # launch command extended with stuff needed to fix systemd wonkiness
  wonkyLaunchCommand = pkgs.writeShellScript "reka-launch-wonky" ''
    systemctl --user import-environment WAYLAND_DISPLAY
    exec ${cfg.launchCommand}
  '';

  # this is NOT a nixos unit because those have a weird environment
  rekaUnit = pkgs.writeText "reka.service" ''
    [Unit]
    Description=reka, emacs for river
    BindsTo=graphical-session.target
    Before=graphical-session.target
    Wants=pipewire.service graphical-session-pre.target
    After=pipewire.service graphical-session-pre.target

    Wants=xdg-desktop-autostart.target
    Before=xdg-desktop-autostart.target

    [Service]
    Slice=session.slice
    Type=simple
    WorkingDirectory=%h
    Environment="RUST_LOG=${if cfg.debug then "DEBUG" else "INFO"}"
    ExecStart=${pkgs.river}/bin/river -c ${wonkyLaunchCommand}
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
    debug = lib.mkEnableOption "enable debug logging for reka";

    # TODO(tazjin): this is too generic, I guess, but it'll work ...
    launchCommand = lib.mkOption {
      type = lib.types.str;
      description = "Fully configured launch command script (passed to river)";
    };
  };

  config = lib.mkIf cfg.enable {
    security.polkit.enable = true;
    services.graphical-desktop.enable = true;

    systemd.packages = [ rekaSystemPackage ];
    services.displayManager.sessionPackages = [ rekaSystemPackage ];

    environment.systemPackages = [
      rekaSystemPackage
#      depot.third_party.river-channel
    ];
  };
}
