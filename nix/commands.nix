{ nixpkgs, pkgs, basePackages }:
let
  mkForGuixPkg = guixPkg:
    let
      cmd-time-machine = pkgs.writeShellApplication {
        name = "time-machine";
        runtimeInputs = [ ];
        #checkPhase = "echo skip check phase";
        text = let
          text = ''
            GUIX_PACKAGE_PATH=${guixPkg}/guix-modules \
              guix time-machine \
              -C ${guixPkg}/channels.scm \
              -- "''${@:1}"
          '';
        in ''
          echo ${text}
          ${text}
        '';

      };
    in {
      packages = { inherit cmd-time-machine; };
      apps = {
        time-machine = {
          type = "app";
          program = "${cmd-time-machine}/bin/time-machine";
        };
      };
    };

  inherit (import ./lib.nix { inherit (nixpkgs) lib; }) concatAttrsSuffixed;

  variants = {
    "" = mkForGuixPkg basePackages.guix-with-apple;
    sans-apple = mkForGuixPkg basePackages.guix-sans-apple;
  };
in nixpkgs.lib.genAttrs [ "packages" "apps" ] (name:
  concatAttrsSuffixed (builtins.mapAttrs (_: builtins.getAttr name) variants))
