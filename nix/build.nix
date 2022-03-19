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
    let wrapList = strs: "(list ${concatStringsSep "\n        " strs})";
    in if isBool value then
      (if value then "#t" else "#f")
    else if isList value then
      wrapList (map nixToSchemeExpr value)
    else if isString value then
      ''"${value}"''
    else
      assert isAttrs value;
      wrapList (nixpkgs.lib.attrsets.mapAttrsToList
        (lhs: rhs: "(cons ${nixToSchemeExpr lhs} ${nixToSchemeExpr rhs})")
        value);

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
          '',(local-file "${maybeRelBase}/${name}" #:recursive? #t)''
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
    mkSourceInfoCommon = { lastModifiedDate, narHash, rev ? false, ... }: {
      inherit lastModifiedDate narHash rev;
    };
    lock-info = (nixpkgs.lib.importJSON ../flake.lock).nodes.nixpkgs;
    nixpkgsAllInfo =
      (lock-info.original // lock-info.locked // nixpkgs.sourceInfo);
    fromFlake = {
      "nixpkgs-source+lock-info" = (mkSourceInfoCommon nixpkgsAllInfo) // {
        inherit (nixpkgsAllInfo) owner repo ref;
      };
      self-source-info = mkSourceInfoCommon self.sourceInfo;
    };
    fromAll = fromVersion // fromFlags // fromFlake;
  in with builtins; ''
    (define-module (from-nix)
      #:export (${concatStringsSep "\n            " (attrNames fromAll)}
                all-from-nix-jsexpr))

    ;; from version.nix
    ${nixToSchemeDefMulti fromVersion}
    ;; from nix/flags.nix
    ${nixToSchemeDefMulti fromFlags}
    ;; from flake.lock and flake sourceInfo metadata
    ${nixToSchemeDefMulti fromFlake}
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; for json
    (define all-from-nix-jsexpr
      `(@ ${
        concatStringsSep "\n      " (nixpkgs.lib.attrsets.mapAttrsToList
          (lhs: rhs:
            if isAttrs rhs then
              "(${nixToSchemeExpr lhs} @ ,@${lhs})"
            else
              "(${nixToSchemeExpr lhs} . ,${lhs})") fromAll)
      }))
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

      guix-sans-apple = pkgs.runCommand (mkNixPkgName "guix-sans-apple") {
        passAsFile = [ "fromNixScm" "appleScm" ];
        inherit fromNixScm;
        appleScm = mkAppleScm false;
      } ''
        mkdir -p $out/guix-modules
        cp ${self}/channels.scm $out
        cd $out/guix-modules
        cp -r ${self}/guix/* .
        chmod +w aux-files
        cp ${self}/LICENSE* ${self}/flake.lock ./aux-files/
        cp $fromNixScmPath from-nix.scm
        cp $appleScmPath apple.scm
      '';

      guix-with-apple = let appleExtractedDir = "apple-extracted";
      in pkgs.runCommand (mkNixPkgName "guix-plus-apple") {
        passAsFile = [ "appleScm" ];
        appleScm = mkAppleScm appleExtractedDir;
      } ''
        cp -rL ${guix-sans-apple}/ $out
        chmod +w $out/guix-modules
        cd $out/guix-modules
        rm apple.scm
        cp $appleScmPath apple.scm
        cp -rL ${apple} ${appleExtractedDir}
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
