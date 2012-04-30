#!/bin/bash

# $Id: coverage.sh 5539 2008-03-30 09:05:39Z wiemann $
# Author: Lea Wiemann <LeWiemann@gmail.com>
# Copyright: This script has been placed in the public domain.

# Usage: ./coverage.sh [project, [module]]

set -e
# Resolve all symlinks in current path.
cd -P .
proj="${PWD##*/}"
if test "$proj" == test; then
    cd ..
    proj="${PWD##*/}"
fi
if test "$1"; then
    proj="$1"
fi
module="${2:-alltests.py}"
module="${module#test/}"
echo "Performing code coverage test for project \"$proj\", test module \"$module\"..."
echo
echo "Please be patient; coverage tracking slows test execution down by more"
echo "than factor 10."
echo
cd test
rm -rf cover
mkdir -p cover
python -u -m trace --count --coverdir=cover --missing "$module"
cd ..
echo
echo
echo Uncovered lines
echo ===============
echo
(
    find "$proj/" -name \*.py | while read i; do
        i="${i%.py}"
        test -f test/cover/"${i//\//.}".cover -o "${i##*/}" == Template || echo "${i//\//.}" "`cat "$i.py" | wc -l`"
    done
    cd test/cover
    find . \( -name . -o ! -name "$proj".\* -exec rm {} \; \)
    for i in *.cover; do
        sed 's/^>>>>>> \(.*"""\)/       \1/' < "$i" > "${i%.cover}"
        rm "$i"
    done
    for i in *; do echo -n "$i "; grep -c '^>>>>>> ' "$i" || true; done
) | grep -v ' 0$' | sort -nk 2
