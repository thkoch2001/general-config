{ config, lib, pkgs, ... }:
with lib;
let
  # autologin on tty7y. Otherwise autologin and getty alternate in grabbing tty1 on nixos-rebuild switch
  autologin_on_7 = pkgs.autologin.overrideAttrs (_: {
    postPatch = ''
      substituteInPlace "main.c" \
        --replace-fail "setup_vt(1);" "setup_vt(7);" \
        --replace-fail "XDG_VTNR=1" "XDG_VTNR=7"
    '';
  }
  );
  cfg = config.services.autologin;
in
{
  options.services.autologin = {
    enable = mkEnableOption "autologin";
    leader = mkOption {
      description = "which binary to start";
      type = types.str;
    };
    user = mkOption {
      description = "user to autologin";
      type = types.str;
    };
  };

  config = mkIf cfg.enable {
    # see https://git.sr.ht/~kennylevinsen/autologin
    # TODO https://superuser.com/questions/1904422/how-to-auto-login-without-display-manager-for-nixos-wayland-niri/1904473#1904473
    environment.systemPackages = [ autologin_on_7 ];

    systemd.services.autologin = {
      enable = true;
      restartIfChanged = lib.mkForce false;
      description = "Autologin";
      after = [
        "systemd-user-sessions.service"
        "plymouth-quit-wait.service"
      ];

      serviceConfig = {
        ExecStart = "${autologin_on_7}/bin/autologin ${cfg.user} ${cfg.leader}";
        Type = "simple";
        IgnoreSIGPIPE = "no";
        SendSIGHUP = "yes";
        TimeoutStopSec = "30s";
        KeyringMode = "shared";
        Restart = "always";
        RestartSec = "10";
      };
      startLimitBurst = 5;
      startLimitIntervalSec = 30;
      aliases = [ "display-manager.service" ];
      wantedBy = [ "multi-user.target" ];
    };

    security.pam.services.autologin = {
      enable = true;
      name = "autologin";
      startSession = true;
      setLoginUid = true;
      updateWtmp = true;
    };
  };
}
