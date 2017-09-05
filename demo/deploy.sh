#!/bin/bash

# From https://gist.github.com/domenic/ec8b0fc8ab45f39403dd

set -e # Exit with nonzero exit code if anything fails

SOURCE_BRANCH="master"
TARGET_BRANCH="gh-pages"
TEMP_FOLDER="${TRAVIS_BUILD_DIR}/temp"
ENCRYPTION_LABEL="2e639d5f7f73"
UGLIFYJS="node ${TRAVIS_BUILD_DIR}/node_modules/uglify-js/bin/uglifyjs"

# Save some useful information
REPO=`git config remote.origin.url`
SSH_REPO=${REPO/https:\/\/github.com\//git@github.com:}
SHA=`git rev-parse --verify HEAD`

# Clone the existing gh-pages for this repo into TEMP_FOLDER
# Create a new empty branch if gh-pages doesn't exist yet (should only happen on first deploy)
echo "Building and deploying demo"
echo "Target: ${TARGET_BRANCH} branch"
# git clone $REPO $TEMP_FOLDER
# cd $TEMP_FOLDER
# git checkout $TARGET_BRANCH || git checkout --orphan $TARGET_BRANCH

# Run our compile script
echo "Compiling into ${TEMP_FOLDER}/"
cd demo
$SYSCONFCPUS $ELM_MAKE Main.elm --output $TEMP_FOLDER/elm.js --yes
sed -i -e 's/\/_compile\/Main.elm/elm.js/g' index.html
cp index.html $TEMP_FOLDER/index.html
$UGLIFYJS $TEMP_FOLDER/elm.js --output $TEMP_FOLDER/elm.js

cd $TRAVIS_BUILD_DIR
git checkout -b $TARGET_BRANCH
cp $TEMP_FOLDER/* $TRAVIS_BUILD_DIR

# Git commands
git config user.name "Travis CI"
git config user.email "pablohirafuji@gmail.com"
git add index.html elm.js
git commit -m "Deploy to GitHub Pages: ${SHA}"

# Get the deploy key by using Travis's stored variables to decrypt deploy-key.enc
ENCRYPTED_KEY_VAR="encrypted_${ENCRYPTION_LABEL}_key"
ENCRYPTED_IV_VAR="encrypted_${ENCRYPTION_LABEL}_iv"
ENCRYPTED_KEY=${!ENCRYPTED_KEY_VAR}
ENCRYPTED_IV=${!ENCRYPTED_IV_VAR}
openssl aes-256-cbc -K $ENCRYPTED_KEY -iv $ENCRYPTED_IV -in demo/deploy-key.enc -out deploy-key -d

chmod 600 deploy-key
eval `ssh-agent -s`
ssh-add deploy-key

# Now that we're all set up, we can push.
git push -f $SSH_REPO $TARGET_BRANCH

echo "Deployed successfully."
exit 0