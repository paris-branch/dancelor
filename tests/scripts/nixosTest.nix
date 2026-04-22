{ ... }:

{
  nodes.machine =
    { pkgs, ... }:
    {
      virtualisation = {
        cores = 4;
        memorySize = 8192;
      };

      services.dancelor = {
        enable = true;
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

    machine.succeed("mariadb dancelor < ${../database.sql}")

    ## FIXME: Because of Dancelor's in-memory cache, we need to restart it after
    ## data import. This is very silly, but will naturally get resolved once we
    ## get rid of the cache and rely on calling MariaDB directly.
    ##
    machine.succeed("systemctl restart dancelor.service")

    machine.succeed("cd ${../..} && pytest --numprocesses auto --verbose >&2")
  '';
}
