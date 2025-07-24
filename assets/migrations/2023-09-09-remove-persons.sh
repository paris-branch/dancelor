#!/bin/sh
set -euC

printf 'Cleaning up all credits...\n'
find credit/ -name meta.yaml -exec yq -iy '.' {} +
git add credit/
git commit -m 'Cleaning up all credits'
printf 'done.\n\n'

printf 'Removing all persons...\n'
rm -Rf person/
git add person/
git commit -m 'Removing all persons'
printf 'done.\n\n'

printf 'Removing the field `person` from all credits...\n'
find credit/ -name meta.yaml -exec yq -iy 'del(.persons)' {} +
git add credit/
git commit -m 'Removing all persons from credits'
printf 'done.\n\n'

printf 'Removing leftover empty directories... '
find credit/ -type d -exec rmdir {} + 2>/dev/null && true
rmdir credit/ 2>/dev/null && true
printf 'done.\n'
