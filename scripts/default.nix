{ pkgs, isWorkLaptop }:

let
  scriptsFile = file:
    pkgs.writeScriptBin "${file}" (builtins.readFile (./. + "/${file}"));

  e = scriptsFile "e";
  eb = scriptsFile "eb";
  ks = scriptsFile "ks";
  jwt = scriptsFile "jwt";
  note = scriptsFile "note";
  podshell = scriptsFile "podshell";
  kubesummary = scriptsFile "kubesummary";
  changed-files = scriptsFile "changed-files";
  backup = scriptsFile "backup";

in [
  e
  eb
  ks
  jwt
  note
  podshell
  kubesummary
  changed-files
  (if isWorkLaptop then null else backup)
]
