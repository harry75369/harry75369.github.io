#!/usr/bin/env bash

TMPDIR=`mktemp -d -q /tmp/site.XXXXXXX`
if [ $? -ne 0 ]; then
  echo "Failed to create temp directory, exiting..."
  exit 1
fi

cp -r _site/* $TMPDIR
git checkout master
rsync -rvzPucl $TMPDIR/ ./ --exclude ".git*" --exclude "_*" --exclude ".cabal*" --exclude "cabal.sandbox.config" --exclude "dist" --exclude ".DS_Store"
git add -A
git commit -m "automaticly updated"
git push origin master
git checkout hakyll
