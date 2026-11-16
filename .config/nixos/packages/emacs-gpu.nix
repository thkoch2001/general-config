{ pkgs, ... }:

(pkgs.emacs31.override {
  withX = false;
  withCairo = false;
  withGTK3 = false;
  withPgtk = false;
  withXwidgets = false;
  withToolkitScrollBars = false;
}).overrideAttrs (old: {
  pname = "emacs31-gpu";
  version = "31.devel-gpu";

  src = pkgs.fetchFromCodeberg {
    owner = "ezemtsov";
    repo = "emacs";
    #    tag = "v${finalAttrs.version}";
    rev = "d5dcd3a167f30e7eea0e29c7aef735e729a3fa5c";
    hash = "sha256-QQbD5gK64+H6LOnHAar0d8lrjqpwUWa9jEkwyqQ4iwI=";
  };

  patches = pkgs.lib.drop 2 old.patches;

  # Drop the image opt-outs the bare (no-GTK) build inherits, then turn the
  # backends and image support we want back on.
  configureFlags =
    pkgs.lib.filter
      (flag: !(pkgs.lib.elem flag [ "--without-gif" "--without-jpeg" "--without-png" "--without-tiff" ]))
      (old.configureFlags or [ ])
    ++ [
      "--with-rsvg"
      "--with-pwayl"
      "--with-skia"
      "--with-gif"
      "--with-jpeg"
      "--with-png"
      "--with-tiff"
    ];

  buildInputs = old.buildInputs ++ (with pkgs; [
    fontconfig
    freetype
    giflib
    libjpeg
    libpng
    libtiff
    librsvg
    skia
    libepoxy
    wayland
    wayland-protocols
    libxkbcommon
  ]);

  nativeBuildInputs = old.nativeBuildInputs ++ (with pkgs; [ autoconf automake wayland-scanner ]);

  preConfigure = (old.preConfigure or "") + ''
    ./autogen.sh
  '';
})
