{ pkgs, self }:

let
  scriptsFile = file: pkgs.writeScriptBin "${file}" (builtins.readFile (self + "/${file}"));

  e = scriptsFile "e";
  ip = scriptsFile "ip";
  eb = scriptsFile "eb";
  ks = scriptsFile "ks";
  jwt = scriptsFile "jwt";
  podshell = scriptsFile "podshell";
  kubesummary = scriptsFile "kubesummary";
  changed-files = scriptsFile "changed-files";
  backup = scriptsFile "backup";
  po = scriptsFile "po";

  scripts = [
    backup
    e
    eb
    ks
    jwt
    podshell
    kubesummary
    changed-files
    ip
    po
    (pkgs.writeScriptBin "agentbox" (builtins.readFile (self + "/agentbox.ts")))
    (pkgs.writeScriptBin "safetools" (builtins.readFile (self + "/safetools.ts")))
  ];

in
scripts
