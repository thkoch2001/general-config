{ config, lib, pkgs, ... }:
{
  programs.light = {
    enable = true;
    brightnessKeys.enable = true;
    brightnessKeys.minBrightness = 1;
  };
}
