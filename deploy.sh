#!/usr/bin/env bash

function check_fail() {
  if [ $? -ne 0 ]; then
    echo $1
    echo "Exiting..."
    exit 1
  fi
}

command -v stack > /dev/null 2>&1 || check_fail "Command not found: stack"
command -v rsync > /dev/null 2>&1 || check_fail "Command not found: rsync"
command -v git > /dev/null 2>&1 || check_fail "Command not found: git"

echo "Creating temp directory..."
TMPDIR=`mktemp -d -q /tmp/site.XXXXXXX`
check_fail "Failed to create temp directory"

echo "Building hakyll binary..."
stack build > /dev/null
check_fail "Failed to build hakyll binary"

echo "Cleaning old files..."
stack exec site clean > /dev/null
check_fail "Failed to clean old files"

echo "Building new site..."
stack site build > /dev/null
check_fail "Failed to build new site"

echo "Checking site links..."
stack site check > /dev/null
check_fail "Failed to pass link check"

echo "Preparing temp files..."
cp -r _site/* $TMPDIR > /dev/null
check_fail "Failed to copy filesi to temp directory"

echo "Switching to master..."
git checkout master > /dev/null 2>&1
check_fail "Failed to switch to master branch"

echo "Synchronizing files..."
rsync -rzPucl $TMPDIR/ ./ --exclude ".git*" --exclude "_*" --exclude ".cabal*" --exclude "cabal.sandbox.config" --exclude "dist" --exclude ".DS_Store" --exclude ".stack-work" > /dev/null
check_fail "Failed to sync files"

echo "Updating repo files..."
git add -A > /dev/null
check_fail "Failed to update repo files"

echo "Commiting..."
git commit -m "automaticly updated" > /dev/null

if [ $? -ne 0 ]; then
  echo "No new commit, skipped pushing..."
else
  echo "Pushing..."
  git push origin master > /dev/null
  check_fail "Failed to push to remote server"
fi

echo "Switching back to hakyll..."
git checkout hakyll > /dev/null
check_fail "Failed to switch to hakyll branch"

echo "All done."

