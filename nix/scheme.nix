{ self, nixpkgs, rkt }:
with builtins;
with nixpkgs.lib;
let

  wrapList = strs: "(list ${concatStringsSep "\n        " strs})";

  nixToSchemeExpr = value:
    if isBool value then
      (if value then "#t" else "#f")
    else if isList value then
      wrapList (map nixToSchemeExpr value)
    else if isString value then
      ''"${value}"''
    else
      assert isAttrs value;
      wrapList (attrsets.mapAttrsToList
        (lhs: rhs: "(cons ${nixToSchemeExpr lhs} ${nixToSchemeExpr rhs})")
        value);

  nixToSchemeDef = name: value: ''
    (define ${name}
      ${nixToSchemeExpr value})
  '';

  nixToSchemeDefMulti = attrs:
    concatStringsSep "" (attrsets.mapAttrsToList nixToSchemeDef attrs);

  #####

  from = rec {
    version = {
      pkg-version = rkt.pkgVersion;
      breaking-change-label = rkt.breakingChangeLabel;
      so-version = rkt.soVersion;
      deprecate-hard = rkt.deprecateHard;
      libgit2-version = rkt.libgit2.version;
      libgit2-sha256 = rkt.libgit2.src.sha256;
      # ^ Thankfully, Nix and Guix calculate it the same way!
      libgit2-rev = rkt.libgit2.src.rev;
      libgit2-url = rkt.libgit2.src.url;
    };
    flags = let flags = import ./flags.nix rkt;
    in {
      cfg-flags-common = flags.common;
      cfg-flags-unix = flags.unix;
    };
    flake = {
      self-source-info = {
        inherit (self.sourceInfo) lastModifiedDate narHash;
        rev = self.rev or false;
      };
      "nixpkgs-source+lock-info" =
        let lock-info = (nixpkgs.lib.importJSON ../flake.lock).nodes.nixpkgs;
        in {
          inherit (lock-info.original // lock-info.locked // nixpkgs.sourceInfo)
            lastModifiedDate narHash rev owner repo ref;
        };
    };
    all = version // flags // flake;
  };

in {
  fromNixScm = ''
    (define-module (from-nix)
      #:export (${concatStringsSep "\n            " (attrNames from.all)}
                all-from-nix-jsexpr))

    ;; from version.nix
    ${nixToSchemeDefMulti from.version}
    ;; from nix/flags.nix
    ${nixToSchemeDefMulti from.flags}
    ;; from flake.lock and flake sourceInfo metadata
    ${nixToSchemeDefMulti from.flake}
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; for json
    (define all-from-nix-jsexpr
      `(@ ${
        concatStringsSep "\n      " (attrsets.mapAttrsToList (lhs: rhs:
          if isAttrs rhs then
            "(${nixToSchemeExpr lhs} @ ,@${lhs})"
          else
            "(${nixToSchemeExpr lhs} . ,${lhs})") from.all)
      }))
  '';

  mkAppleScm = applePlatformsExtractedPaths:
    let
      toQq = name: relPath: ''("${name}" ${toGexp name relPath})'';
      toGexp = name: relPath:
        if isString relPath then
          '',(local-file "${relPath}" #:recursive? #t)''
        else
          "#f";
    in ''
      (define-module (extracted apple)
        #:use-module (guix gexp)
        #:export (apple-platforms-extracted))

      (define apple-platforms-extracted
        `(${
          concatStringsSep "\n    "
          (mapAttrsToList toQq applePlatformsExtractedPaths)
        }))
    '';
}
