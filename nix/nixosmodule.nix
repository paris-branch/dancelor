{ self, ... }:

{
  flake.nixosModules.dancelor =
    {
      lib,
      config,
      pkgs,
      ...
    }:
    let
      cfg = config.services.dancelor;
    in
    {
      options.services.dancelor = {
        enable = lib.mkEnableOption "Enable the Dancelor service";

        databaseRepositoryFile = lib.mkOption {
          type = lib.types.str;
          description = "Path to a file that contains the link to the database repository.";
        };

        listeningPort = lib.mkOption {
          type = lib.types.int;
          default = 6872;
          description = "Port on which Dancelor will listen.";
        };

        githubTokenFile = lib.mkOption {
          type = lib.types.str;
          description = ''
            Path to a file that contains the GitHub API token.

            This is used by the issue report mechanism and must therefore be allowed to open issues on the repositories.
          '';
        };

        githubRepository = lib.mkOption {
          type = lib.types.str;
          description = ''
            Dancelor's GitHub repository

            This is used by the issue report mechanism. It must contain the host, owner, and repository.
          '';
          example = "github.com/paris-branch/dancelor";
        };

        githubDatabaseRepository = lib.mkOption {
          type = lib.types.str;
          description = ''
            Dancelor's database's GitHub repository - used by the error reporting mechanism.

            This is used by the issue report mechanism. It must contain the host, owner, and repository.
          '';
          example = "github.com/paris-branch/dancelor-database";
        };

        routineThreads = lib.mkOption {
          type = lib.types.int;
          description = ''
            Number of threads used by routines.
          '';
          default = 2;
          example = 8;
        };

        testMode = lib.mkOption {
          type = lib.types.bool;
          description = ''
            Whether to set up Dancelor for testing. Do not use in production.
          '';
          default = false;
        };
      };

      config = lib.mkIf cfg.enable (
        let
          stateDir = "/var/lib/dancelor";
          databaseDir = "${stateDir}/database";

          ## Part of the `init-dancelor` process that runs as the Dancelor user.
          init-dancelor-as-dancelor = pkgs.writeShellApplication {
            name = "init-dancelor";
            runtimeInputs = with pkgs; [ git ];
            excludeShellChecks = [ "SC2016" ];
            text =
              if cfg.testMode then
                ''
                  ln -sf ${../tests/database} ${databaseDir}
                ''
              else
                ''
                  ## If the repository does not exist, then we clone it.
                  if ! [ -e ${databaseDir} ]; then
                    echo "Cloning the repository to ${databaseDir}..."
                    git clone "$(cat ${cfg.databaseRepositoryFile})" ${databaseDir}
                    echo 'done.'
                  fi
                  cd ${databaseDir} 
                  ## Once the repository exists and is a Git repository, we
                  ## configure it. Most of these will not change, but they might
                  ## sometimes (in particular the repository) and it will not hurt
                  ## to do that again.
                  if [ "$(git rev-parse --is-inside-work-tree 2>/dev/null)" = true ]; then
                    (
                      echo 'Setting up the git repository...'
                      echo '  - username...'
                      git config user.name Dancelor
                      echo '  - email...'
                      git config user.email dancelor@dancelor.org
                      echo '  - remote...'
                      git remote set-url origin "$(cat ${cfg.databaseRepositoryFile})"
                      echo 'done.'
                    )
                  else
                    echo "The directory '${databaseDir}' exists but is not a Git repository." >&2
                    exit 1
                  fi
                '';
          };

          ## The actual `init-dancelor` process, just a small wrapper around the
          ## previous one.
          init-dancelor = pkgs.writeShellApplication {
            name = "init-dancelor";
            text = ''
              mkdir -p ${stateDir}
              chown -R dancelor:dancelor ${stateDir}
              ${pkgs.su}/bin/su -s /bin/sh dancelor -c ${init-dancelor-as-dancelor}/bin/init-dancelor
            '';
          };

          run-dancelor = pkgs.writeShellApplication {
            name = "run-dancelor";
            text = ''
              ${self.apps.${pkgs.system}.dancelor.program} \
                --database ${databaseDir} \
                ${lib.strings.optionalString cfg.testMode ''
                  --no-sync-storage \
                  --no-write-storage \
                ''} \
                --loglevel info \
                --port ${toString cfg.listeningPort} \
                --github-token-file ${cfg.githubTokenFile} \
                --github-repository ${cfg.githubRepository} \
                --github-database-repository ${cfg.githubDatabaseRepository} \
                --routine-threads ${toString cfg.routineThreads}
            '';
          };

        in
        {
          ## Create a user and a group `dancelor:dancelor`.
          users.users.dancelor = {
            isSystemUser = true;
            group = "dancelor";
          };
          users.groups.dancelor = { };

          ## Initialisation service. Runs once as root so as to create the right
          ## directories for the actual service which will run as `dancelor`.
          systemd.services.dancelor-init = {
            wantedBy = [ "multi-user.target" ];
            serviceConfig = {
              ExecStart = "${init-dancelor}/bin/init-dancelor";
              Type = "oneshot";
            };
          };

          ## Actual service that runs `dancelor`. Requires the service above. Runs as
          ## `dancelor:dancelor`.
          systemd.services.dancelor = {
            after = [
              "network.target"
              "dancelor-init.service"
            ];
            requires = [ "dancelor-init.service" ];
            wantedBy = [ "multi-user.target" ];
            serviceConfig = {
              ExecStart = "${run-dancelor}/bin/run-dancelor";
              Restart = "always";
              User = "dancelor";
              Group = "dancelor";
            };
          };
        }
      );
    };
}
