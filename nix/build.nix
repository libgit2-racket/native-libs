{ self, nixpkgs, rkt }:
let

  flags = (import ./flags.nix rkt);

  mkDarwinHost = pkgs: rec {
    name = "${pkgs.hostPlatform.qemuArch}-macosx";
    built = pkgs.libgit2.overrideAttrs (oldAttrs: {
      version = rkt.libgit2.version;
      src = pkgs.fetchgit rkt.libgit2.src;
      patches = [ ];
      buildInputs = [ ];
      cmakeFlags = flags.common ++ flags.unix;
    });
    extracted = pkgs.runCommand "extracted-${name}" { } ''
      mkdir -p $out/${name}
      cd $out/${name}
      cp ${built}/lib/libgit2.${rkt.soVersion}.dylib .
      echo ${pkgs.buildPlatform.config} > built-on.txt
    '';
  };

  mkNixPkgName = name: "racket-libgit2-nix-${name}-${rkt.pkgVersion}";

  nixToSchemeExpr = value:
    with builtins;
    if isBool value then
      (if value then "#t" else "#f")
    else if isList value then
      "(list ${concatStringsSep "\n        " (map nixToSchemeExpr value)})"
    else
      assert isString value; ''"${value}"'';

  nixToSchemeDef = name: value: ''
    (define ${name}
      ${nixToSchemeExpr value})
  '';

  nixToSchemeDefMulti = attrs:
    builtins.concatStringsSep ""
    (nixpkgs.lib.attrsets.mapAttrsToList nixToSchemeDef attrs);

  mkAppleScmForPlatforms = names: maybeRelBase:
    with builtins;
    let
      nameToQq = name: ''("${name}" ${nameToGexp name})'';
      nameToGexp = name:
        if isString maybeRelBase then
          '',(local-file "${maybeRelBase}/#{name}")''
        else
          "#f";
    in ''
      (define-module (apple)
        #:use-module (guix gexp)
        #:export (apple-platforms))

      (define apple-platforms
        `(${concatStringsSep "\n    " (map nameToQq names)}))
    '';

  fromNixScm = let
    fromVersion = {
      pkg-version = rkt.pkgVersion;
      breaking-change-label = rkt.breakingChangeLabel;
      so-version = rkt.soVersion;
      deprecate-hard = rkt.deprecateHard;
      libgit2-version = rkt.libgit2.version;
      libgit2-sha256 = rkt.libgit2.src.sha256;
      # ^ Thankfully, Nix and Guix calculate it the same way!
      libgit2-commit = rkt.libgit2.src.rev;
      libgit2-url = rkt.libgit2.src.url;
    };
    fromFlags = {
      cfg-flags-common = flags.common;
      cfg-flags-unix = flags.unix;
    };
  in with builtins; ''
    (define-module (from-nix)
      #:export (${
        let sep = "\n            ";
        in concatStringsSep sep (concatMap attrNames [ fromVersion fromFlags ])
      }))

    ;; from version.nix
    ${nixToSchemeDefMulti fromVersion}
    ;; from nix/flags.nix
    ${nixToSchemeDefMulti fromFlags}
  '';

  mkPackagesForBuildPlatform = pkgs: darwinHosts:
    let
      mkAppleScm =
        mkAppleScmForPlatforms (nixpkgs.lib.catAttrs "name" darwinHosts);
    in rec {
      apple = pkgs.symlinkJoin {
        name = mkNixPkgName "apple-bundle";
        paths = nixpkgs.lib.attrsets.catAttrs "extracted" darwinHosts;
      };

      guixSansApple = pkgs.runCommand (mkNixPkgName "guix-sans-apple") {
        passAsFile = [ "fromNixScm" "appleScm" ];
        inherit fromNixScm;
        appleScm = mkAppleScm false;
      } ''
        mkdir -p $out/guix-modules
        cp ${self}/channels.scm $out
        cd $out/guix-modules
        cp -r ${self}/guix/* .
        cp ${self}/channels.scm ${self}/LICENSE* .
        cp $fromNixScmPath from-nix.scm
        cp $appleScmPath apple.scm
      '';

      guixWithApple = let appleExtractedDir = "apple-extracted";
      in pkgs.runCommand (mkNixPkgName "guix-plus-apple") {
        passAsFile = [ "appleScm" ];
        appleScm = mkAppleScm appleExtractedDir;
      } ''
        cp -as ${guixSansApple}/ $out
        chmod +w $out/guix-modules
        cd $out/guix-modules
        rm apple.scm
        cp $appleScmPath apple.scm
        ln -s ${apple} ${appleExtractedDir}
      '';
    } // (builtins.listToAttrs (builtins.concatMap
      ({ name, built, extracted }: [
        {
          name = "built-${name}";
          value = built;
        }
        {
          name = "extracted-${name}";
          value = extracted;
        }
      ]) darwinHosts));

in { inherit mkDarwinHost mkPackagesForBuildPlatform; }
