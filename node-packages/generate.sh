#!/usr/bin/env bash

# inspo: https://github.com/NixOS/nixpkgs/blob/a982e38c36cde1c405c8e2da8c6e676898e1a80a/pkgs/development/node-packages/generate.sh

set -eu -o pipefail

rm node-packages.nix composition.nix

node2nix \
    -i node-packages.json \
    -o node-packages.nix \
    -c composition.nix \
    --pkg-name nodejs-16_x

# https://github.blog/2021-09-01-improving-git-protocol-security-github
sd 'git://github.com/' 'https://github.com/' node-packages.nix
# https://github.com/svanderburg/node2nix/issues/134#issuecomment-475809875
sed -i -e 's/dontNpmInstall ? false/dontNpmInstall ? true/g' node-env.nix
