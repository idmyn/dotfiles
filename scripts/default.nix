{ writeScriptBin, ... }:

let
  scriptsFile = file: writeScriptBin "${file}" (builtins.readFile (./. + "/${file}"));

  e = scriptsFile "e";
  jwt = scriptsFile "jwt";
  podshell = scriptsFile "podshell";
  kubesummary = scriptsFile "kubesummary";
  changed-files = scriptsFile "changed-files";
in

[
  e jwt podshell kubesummary changed-files
]
