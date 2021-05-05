{ writeScriptBin, ... }:

let
  scriptsFile = file: writeScriptBin "${file}" (builtins.readFile (./. + "/${file}"));

  e = scriptsFile "e";
  eb = scriptsFile "eb";
  jwt = scriptsFile "jwt";
  podshell = scriptsFile "podshell";
  kubesummary = scriptsFile "kubesummary";
  changed-files = scriptsFile "changed-files";
in

[
  e eb jwt podshell kubesummary changed-files
]
