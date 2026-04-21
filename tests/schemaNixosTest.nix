{ ... }:

{
  nodes.machine =
    { pkgs, ... }:
    {
      virtualisation = {
        cores = 2;
        memorySize = 2048;
      };

      services.dancelor = {
        enable = true;
        listeningPort = 8080;
        githubTokenFile = "${pkgs.writeText "github-token" "dummy github token"}";
        githubRepository = "github.com/paris-branch/dancelor";
        githubDatabaseRepository = "github.com/paris-branch/dancelor-database";
        testMode = true;
      };

      environment.systemPackages = with pkgs; [
        mariadb
      ];
    };

  testScript = ''
    machine.start()
    machine.wait_for_unit("dancelor.service")

    machine.succeed("${../scripts/mariadb-dump-schema} > /tmp/actual-schema.sql")
    machine.succeed("diff --unified --ignore-space-change ${../src/server/database/schema.sql} /tmp/actual-schema.sql >&2")
  '';
}
