E:NIX_LINK = $E:HOME/.nix-profile
E:NIX_PATH = $E:HOME/.nix-defexpr/channels:darwin-config=$E:HOME/.nixpkgs/darwin-configuration.nix
E:NIX_PROFILES = "/nix/var/nix/profiles/default "$E:NIX_LINK
E:NIX_SSL_CERT_FILE = $E:NIX_LINK/etc/ssl/certs/ca-bundle.crt
paths = [
  $E:NIX_LINK/bin
  /run/current-system/sw/bin
  $@paths
]
