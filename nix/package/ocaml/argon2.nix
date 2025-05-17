{
  buildDunePackage,
  fetchFromGitHub,
  libargon2,

  # ocaml packages
  ctypes,
  ctypes-foreign,
  dune-configurator,
  result,
}:

buildDunePackage {
  pname = "argon2";
  version = "dev";
  src = fetchFromGitHub {
    owner = "khady";
    repo = "ocaml-argon2";
    rev = "1.0.2";
    sha256 = "sha256-m5yOMT33Z9LfjQg6QRBW6mjHNyIySq6somTFuGmL9xI=";
  };

  propagatedBuildInputs = [
    ctypes-foreign
    libargon2
  ];

  buildInputs = [
    ctypes
    dune-configurator
    result
  ];
}
