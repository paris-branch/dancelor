{
  buildDunePackage,
  fetchFromGitHub,
  qcheck,
  ppxlib,
  ppx_deriving,
}:

buildDunePackage {
  pname = "ppx_deriving_qcheck";
  version = "0.7";

  src = fetchFromGitHub {
    owner = "c-cube";
    repo = "qcheck";
    tag = "v0.25";
    hash = "sha256-Z89jJ21zm89wb9m5HthnbHdnE9iXLyaH9k8S+FAWkKQ=";
  };

  propagatedBuildInputs = [
    qcheck
    ppxlib
    ppx_deriving
  ];

  meta = qcheck.meta // {
    description = "PPX Deriver for QCheck";
  };
}
