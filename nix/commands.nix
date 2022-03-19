{ nixpkgs, pkgs, basePackages }:
let
  mkForGuixPkg = guixPkg: rec {
    packages = let
      inferredArgs = default: ''
        if [ $# -eq 0 ]; then
           inferredArgs=(${default})
        else
           inferredArgs=( "$@:1" )
        fi
      '';
    in rec {
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
      cmd-guix-show = pkgs.writeShellApplication {
        name = "guix-show";
        runtimeInputs = [ ];
        text = ''
          ${inferredArgs "racket-libgit2-omnibus"}
          ${cmd-time-machine}/bin/time-machine \
            show "''${inferredArgs[@]}"
        '';
      };
      cmd-guix-build = pkgs.writeShellApplication {
        name = "guix-build";
        runtimeInputs = [ ];
        text = ''
          ${inferredArgs "--keep-failed racket-libgit2-omnibus"}
          ${cmd-time-machine}/bin/time-machine \
            build \
            ''${NIX_RUN_GUIX_BUILD_ROOT:+--root=$NIX_RUN_GUIX_BUILD_ROOT} \
            "''${inferredArgs[@]}"
        '';
      };
    };
    apps = with packages; {
      time-machine = {
        type = "app";
        program = "${cmd-time-machine}/bin/time-machine";
      };
      guix-show = {
        type = "app";
        program = "${cmd-guix-show}/bin/guix-show";
      };
      guix-build = {
        type = "app";
        program = "${cmd-guix-build}/bin/guix-build";
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
