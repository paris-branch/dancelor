#!/bin/sh
set -euC

readonly PUBLIC=public
readonly TMPDIR=branch-documentation

## CI_COMMIT_TAG is set only if we are in a tag.
if [ "${CI_COMMIT_TAG:+x}" ]; then
    CI_COMMIT_REF_TYPE=tag
else
    CI_COMMIT_REF_TYPE=branch
fi
readonly CI_COMMIT_REF_TYPE
readonly TARGET=$PUBLIC/$CI_COMMIT_REF_TYPE/$CI_COMMIT_REF_SLUG

## Prepare SSH configuration by setting up the key and the known hosts. Erase
## previous keys and known hosts with >|.
##
printf 'Preparing SSH configuration... '
mkdir -p ~/.ssh
echo "$CI_SSH_KEY" >| ~/.ssh/id_"$CI_SSH_KEY_TYPE"
chmod 0600 ~/.ssh/id_"$CI_SSH_KEY_TYPE"
echo "$CI_KNOWN_HOST" >| ~/.ssh/known_hosts
printf 'done.\n'

## Clone and setup repository.
##
printf 'Cloning and configuring repository...\n'
git clone --branch documentation "$CI_SSH_URL" "$TMPDIR"
cd "$TMPDIR"
git config user.name 'GitLab CI'
git config user.email 'ci@gitlab.vlanvin.fr'
printf 'done.\n'

################################################################################
## Update the right directories.
##
printf 'Gathering documentation and metadata in the right sub-dicretory... '
rm -Rf "$TARGET"
mkdir -p "$TARGET"
cp -R ../documentation/* "$TARGET"
echo "$CI_COMMIT_REF_NAME" > "$TARGET"/name
echo "$CI_COMMIT_SHA" > "$TARGET"/hash
date +%s > "$TARGET"/date
printf 'done.\n'

## Replace the index of the newly-added documentation by a redirection to the
## global index.
printf 'Writing local index file... '
rm -f "$TARGET"/index.html
cat <<EOF > "$TARGET"/index.html
<!DOCTYPE html>
<html>
  <head>
    <meta http-equiv="refresh" content="0; url='../..'" />
  </head>
  <body>
    <p>Please follow <a href="../..">this link</a>.</p>
  </body>
</html>
EOF
printf 'done.\n'

## Regenerate the global index page to take into account the newly-added
## documentation.
printf 'Writing global index file... '
rm -f "$PUBLIC"/index.html
{
    print_table () {
        if [ -d "$PUBLIC"/"$2" ]; then
            printf '<tr class="head"><td class="name">%s</td><td>Commit</td><td>Date</td><td>Libraries</td></tr>\n' "$1"
            ls -1 "$PUBLIC"/"$2" | {
                while read -r ref; do
                    printf '%s\t%s\n' "$(cat "$PUBLIC"/"$2"/"$ref"/date)" "$ref"
                done
            } | sort -rn | cut -f 2 | {
                while read -r ref; do
                    libs=$(
                        for lib in "$PUBLIC"/"$2"/"$ref"/*; do
                            if [ -d "$lib" ]; then
                                basename "$lib"
                            fi
                        done
                        )
                    if [ -n "$libs" ] ; then
                        nb_libs=$(echo "$libs" | wc -l)
                    else
                        nb_libs=0
                    fi
                    lib=$(echo "$libs" | head -n 1)
                    libs=$(echo "$libs" | tail -n +2)
                    name=$(cat "$PUBLIC"/"$2"/"$ref"/name)
                    hash=$(cat "$PUBLIC"/"$2"/"$ref"/hash)
                    hash=$(echo "$hash" | head -c 7)
                    [ "$2" = tag ] && tree_link=$ref || tree_link=$hash
                    tree_link=$(printf 'https://%s/%s/-/commit/%s' "$CI_SERVER_HOST" "$CI_PROJECT_PATH" "$tree_link")
                    doc_link=$2/$ref/$lib
                    date=$(cat "$PUBLIC"/"$2"/"$ref"/date)
                    date=$(date -d @"$date" +'%b %d, %Y')
                    printf '<tr>'
                    printf '<td rowspan="%d" class="name">%s</td>' "$nb_libs" "$name"
                    printf '<td rowspan="%d" class="hash"><a href="%s">%s</a></td>' "$nb_libs" "$tree_link" "$hash"
                    printf '<td rowspan="%d" class="date">%s</td>' "$nb_libs" "$date"
                    printf '<td class="link"><a href="%s">%s</a></td>' "$doc_link" "$lib"
                    printf '</tr>\n'
                    if [ -n "$libs" ]; then
                        echo "$libs" | while read -r lib; do
                            printf '<tr><td class="link"><a href="%s">%s</a></td></tr>\n' "$2"/"$ref"/"$lib" "$lib"
                        done
                    fi
                done
            }
        fi
    }
    cat <<EOF
<!DOCTYPE html>
<html lang="en" xmlns="http://www.w3.org/1999/xhtml">
    <head>
        <title>Documentation</title>
        <meta charset="utf-8"/>
        <meta name="viewport" content="width=device-width,initial-scale=1.0"/>

        <style>
         body {
             font-family: "Fira Sans", Helvetica, Arial, sans-serif;
             text-align: left;
             color: rgb(51, 51, 51);
             background: #FFFFFF;
         }

         .content {
             max-width: 90ex;
             margin: auto;
         }

         ul {
             list-style-type: none;
             padding-left: 10px;
         }

         a { color: #2C5CBD; }

         table {
             margin-top: -20px;
             border-collapse: collapse;
             width: 100%;
         }

         tr {
             border-bottom: 1px solid lightgray;
         }

         td {
             padding: 3px 10px;
         }

         .head td {
             font-weight: bolder;
         }

         .head td {
             padding-top: 30px;
         }
        </style>
    </head>
    <body>
        <main class="content">
            <div class="by-name">
                <h2>Documentation</h2>
                <table>
EOF
    print_table 'Tags' tag
    print_table 'Branches' branch
    cat <<EOF
                </table>
            </div>
        </main>
    </body>
</html>
EOF
} > "$PUBLIC"/index.html
printf 'done.\n'

################################################################################
## Commit if not empty.
##
printf 'Staging, committing and pushing... '
git add "$PUBLIC"
git commit -m "Bump to $CI_COMMIT_SHORT_SHA: $CI_COMMIT_TITLE"
git push origin documentation
printf 'done.\n'
