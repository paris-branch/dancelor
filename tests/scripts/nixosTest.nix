{ ... }:

{
  name = "integration";

  nodes.machine =
    { pkgs, ... }:
    {
      virtualisation = {
        cores = 4;
        memorySize = 8192;
      };

      services.dancelor = {
        enable = true;
        databaseRepositoryFile = "FIXME";
        listeningPort = 8080;
        githubTokenFile = "${pkgs.writeText "github-token" "dummy github token"}";
        githubRepository = "github.com/paris-branch/dancelor";
        githubDatabaseRepository = "github.com/paris-branch/dancelor-database";
        testMode = true;
      };
    };

  testScript = ''
    machine.start()
    machine.wait_for_unit("dancelor.service")
    machine.succeed("cd ${../..} && pytest --numprocesses auto --verbose >&2")
  '';
}
