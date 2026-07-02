{ lib
, stdenv
, acl
, e2fsprogs
, fetchFromGitHub
, libb2
, lz4
, openssh
, openssl
, python3
, xxhash
, zstd
, installShellFiles
, nixosTests
,
}:

let
  python = python3;
in
python.pkgs.buildPythonApplication rec {
  pname = "borgbackup";
  version = "2.0.0b5";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "borgbackup";
    repo = "borg";
    tag = version;
    hash = "sha256-pnCnw9kwiaFf3Lkr+3N/sDdZ6ngw5s6fzzYYWxA+BNY=";
  };

  /*postPatch = ''
    # sandbox does not support setuid/setgid/sticky bits
    substituteInPlace src/borg/testsuite/archiver.py \
      --replace-fail "0o4755" "0o0755"
  '';*/
  postPatch = ''
    substituteInPlace setup.cfg \
      --replace-warn "msgpack >=1.0.3, <=1.0.4" "msgpack >=1.0.3, <=1.1.2" \
      --replace-warn "platformdirs >=3.0.0, <4.0.0" "platformdirs >=3.0.0, <=4.5.1"
    substituteInPlace src/borg/helpers/msgpack.py \
      --replace-warn "# DO NOT CHANGE OR REMOVE! See also requirements and comments in setup.py." \
      "return True"
    substituteInPlace src/borg/item.pyx \
      --replace-warn "    cpdef __set_name__(self, name):" \
      "    cpdef __set_name__(self, owner, name):"
    substituteInPlace src/borg/item.pyx \
      --replace-warn "                value.__set_name__(name)" \
      "                value.__set_name__(cls, name)"
  '';

  build-system = with python.pkgs; [
    cython
    setuptools-scm
    pkgconfig
  ];

  nativeBuildInputs = with python.pkgs; [
    # docs
    sphinxHook
    sphinxcontrib-jquery
    guzzle-sphinx-theme

    # shell completions
    installShellFiles
  ];

  sphinxBuilders = [
    "singlehtml"
    "man"
  ];

  buildInputs =
    [
      libb2
      lz4
      xxhash
      zstd
      openssl
    ]
    ++ lib.optionals stdenv.hostPlatform.isLinux [
      acl
    ];

  dependencies = with python.pkgs; [
    argon2-cffi
    msgpack
    packaging
    platformdirs
    (if stdenv.hostPlatform.isLinux then pyfuse3 else llfuse)
  ];

  makeWrapperArgs = [
    ''--prefix PATH ':' "${openssh}/bin"''
  ];

  /*preInstallSphinx = ''
    # remove invalid outputs for manpages
    rm .sphinx/man/man/_static/jquery.js
    rm .sphinx/man/man/_static/_sphinx_javascript_frameworks_compat.js
    rmdir .sphinx/man/man/_static/
  '';*/

  postInstall = ''
    installShellCompletion --cmd borg \
      --bash scripts/shell_completions/bash/borg \
      --fish scripts/shell_completions/fish/borg.fish \
      --zsh scripts/shell_completions/zsh/_borg
  '';

  doCheck = false;

  outputs = [
    "out"
    "doc"
    "man"
  ];

  disabled = python.pythonOlder "3.9";

  meta = with lib; {
    changelog = "https://github.com/borgbackup/borg/blob/${src.rev}/docs/changes.rst";
    description = "Deduplicating archiver with compression and encryption";
    homepage = "https://www.borgbackup.org";
    license = licenses.bsd3;
    platforms = platforms.unix; # Darwin and FreeBSD mentioned on homepage
    mainProgram = "borg";
    maintainers = with maintainers; [
      dotlambda
      globin
    ];
  };
}
