{ config, lib, pkgs, ... }:
let
  ppdFile = "Lexmark-E120n-lj4dith.ppd";
in
{
  # https://wiki.nixos.org/wiki/Printing
  services.printing = {
    enable = true;
    # logLevel = "debug";
    cups-pdf.enable = true;
  };
  services.avahi = {
    enable = true; # enable for autodiscovery of printers via dnssd://
    nssmdns4 = true;
    openFirewall = true;
  };
  hardware.printers = {
    ensurePrinters = [
      {
        name = "Lexmark-E120n";
        location = "Home";
        deviceUri = "dnssd://Lexmark%20E120n._printer._tcp.local/";
        model = ppdFile;
        ppdOptions = {
          PageSize = "A4";
        };
      }
    ];
    ensureDefaultPrinter = "Lexmark-E120n";
  };
  services.printing.drivers = [
    # pcl5e does not work since hplip in NixOS is apparently configured without hpijs
    # ("hpijs-install=no" in output of hpijs -r -t)
    # In any case, ghostscript says: 'Can't start ijs server "hpijs"'
    # (pkgs.writeTextDir "share/cups/model/Lexmark-E120n-hpijs-pcl5e.ppd" (builtins.readFile ./Lexmark-E120n-hpijs-pcl5e.ppd))
    (pkgs.writeTextDir "share/cups/model/${ppdFile}" (builtins.readFile (./. + "/${ppdFile}")))
  ];
  environment.systemPackages = with pkgs; [
    #    hplipWithPlugin
  ];
}
