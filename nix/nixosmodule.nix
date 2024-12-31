{ self, ... }:
{
  flake.nixosModules.default =
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

        databaseRepositoryFile = lib.mkOption rec {
          type = lib.types.str;
          description = "Path to a file that contains the link to the database repository.";
        };

        listeningPort = lib.mkOption rec {
          type = lib.types.int;
          default = 6872;
          description = "Port on which Dancelor will listen.";
        };
      };

      config =
        let
          init-dancelor = pkgs.writeShellApplication {
            name = "init-dancelor";
            runtimeInputs = with pkgs; [ git ];
            excludeShellChecks = [ "SC2016" ];
            text = ''
              mkdir -p /var/cache/dancelor/{version,set,book}
              mkdir -p /var/lib/dancelor

              ## Test whether the given path is a Git repository owned by 'dancelor'.
              is_dancelor_git_repository () (
                cd "$1" && ${pkgs.su}/bin/su -s /bin/sh dancelor -c \
                  'test "$(git rev-parse --is-inside-work-tree 2>/dev/null)" = true'
              )

              ## If the repository does not exist, then we clone it.
              if ! [ -e /var/lib/dancelor/database ]; then
                echo 'Cloning the repository to /var/lib/dancelor/database...'
                git clone "$(cat ${cfg.databaseRepositoryFile})" /var/lib/dancelor/database
                echo 'done.'
              fi

              ## Once the repository exists and is a Git repository, we
              ## configure it. Most of these will not change, but they might
              ## sometimes (in particular the repository) and it will not hurt
              ## to do that again.
              if is_dancelor_git_repository /var/lib/dancelor/database; then
                (
                  echo 'Setting up the git repository...'
                  cd /var/lib/dancelor/database
                  echo '  - username...'
                  git -c safe.directory=/var/lib/dancelor/database config user.name Auto
                  echo '  - email...'
                  git -c safe.directory=/var/lib/dancelor/database config user.email noreply@dancelor.org
                  echo '  - remote...'
                  git -c safe.directory=/var/lib/dancelor/database remote set-url origin "$(cat ${cfg.databaseRepositoryFile})"
                  echo 'done.'
                )
              else
                echo "The directory '/var/lib/dancelor/database' exists but is not a Git repository." >&2
                exit 1
              fi

              chown -R dancelor:dancelor /var/cache/dancelor
              chown -R dancelor:dancelor /var/lib/dancelor
            '';
          };

          run-dancelor = pkgs.writeShellApplication {
            name = "run-dancelor";
            text = ''
              ${self.apps.${pkgs.system}.default.program} \
                --cache /var/cache/dancelor \
                --database /var/lib/dancelor/database \
                --loglevel info \
                --port ${toString cfg.listeningPort}
            '';
          };

        in
        {
          ## Create a user and a group `dancelor:dancelor`.
          users.users.dancelor = {
            isSystemUser = true;
            ## LilyPond needs a home to cache fonts.
            home = "/var/lib/dancelor";
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
        };
    };
}
