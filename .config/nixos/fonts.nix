# Attempt at configuring reasonable font-rendering.
# TODO: rm depot from tazjin's config

{ pkgs, ... }:

{
  fonts = {
    packages = with pkgs; [
      #      corefonts # has unfree license
      dejavu_fonts
      font-awesome
      jetbrains-mono
      noto-fonts-cjk-sans
      noto-fonts-color-emoji
      noto-fonts-monochrome-emoji
    ];

    fontconfig = {
      hinting.enable = true;
      subpixel.lcdfilter = "light";

      defaultFonts = {
        monospace = [ "JetBrains Mono" ];
      };
    };
  };
}
