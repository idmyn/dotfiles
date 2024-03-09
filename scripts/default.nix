{ pkgs }:

let
  scriptsFile = file:
    pkgs.writeScriptBin "${file}" (builtins.readFile (./. + "/${file}"));

  e = scriptsFile "e";
  ip = scriptsFile "ip";
  eb = scriptsFile "eb";
  ks = scriptsFile "ks";
  jwt = scriptsFile "jwt";
  podshell = scriptsFile "podshell";
  kubesummary = scriptsFile "kubesummary";
  changed-files = scriptsFile "changed-files";
  backup = scriptsFile "backup";

  scripts = [ backup e eb ks jwt podshell kubesummary changed-files ip ];

in  scripts
