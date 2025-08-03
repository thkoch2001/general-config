{
  lib,
  python3Packages,
  fetchFromGitHub
}:

python3Packages.buildPythonApplication rec {
  pname = "ydl-podcast";
  version = "1.9.11";
  format = "pyproject";

  src = fetchFromGitHub {
    owner = "nbr23";
    repo = "${pname}";
    rev = "v${version}";
    hash = "sha256-bFDl/aSs5wHt8Mw97IsFjUffePOn6lulBQDZ6njBlfY=";
  };

  buildInputs = with python3Packages; [
    hatchling
  ];

  propagatedBuildInputs = with python3Packages; [
    jinja2
    markupsafe
    pillow
    pyyaml
    yt-dlp
  ];

  postPatch = ''
    substituteInPlace pyproject.toml \
        --replace "Pillow==10.4.0" "Pillow>=10.4.0" \
    '';
}
