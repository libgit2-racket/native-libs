{ self, nixpkgs, rkt }:
let

  flags = import ./flags.nix rkt;
  scheme = import ./scheme.nix { inherit self nixpkgs rkt; };
  inherit (import ./lib.nix { inherit (nixpkgs) lib; })
    mapToAttrs concatAttrsSuffixed;

  writeProvenance = pkgs: dir: ''
    mkdir -p ${dir}
    echo ${pkgs.buildPlatform.config} > ${dir}/built-on.txt
    echo Nix > ${dir}/built-by.txt
  '';

in {
  mkExtractedDarwin = _: pkgs: rec {
    name = "${pkgs.hostPlatform.qemuArch}-macosx";
    value = rec {
      built = pkgs.libgit2.overrideAttrs (oldAttrs: {
        version = rkt.libgit2.version;
        src = pkgs.fetchgit rkt.libgit2.src;
        patches = [ ];
        buildInputs = [ ];
        cmakeFlags = flags.common ++ flags.unix;
      });
      extracted = pkgs.runCommand "extracted-${name}-${rkt.pkgVersion}" { } ''
        mkdir -p $out/${name}/provenance
        cd $out/${name}
        cp ${built}/lib/libgit2.${rkt.soVersion}.dylib .
        ${writeProvenance pkgs "provenance"}
      '';
    };
  };

  mkBasePackages = { pkgs, applePlatformsExtracted }:
    let mkNixPkgName = name: "racket-libgit2-nix-${name}-${rkt.pkgVersion}";
    in (concatAttrsSuffixed applePlatformsExtracted) // rec {
      apple = pkgs.symlinkJoin {
        name = mkNixPkgName "extracted-apple-bundle";
        paths = nixpkgs.lib.mapAttrsToList (_: builtins.getAttr "extracted")
          applePlatformsExtracted;
      };

      guix-sans-apple = pkgs.runCommand (mkNixPkgName "guix-sans-apple") {
        passAsFile = [ "fromNixScm" "appleScm" ];
        inherit (scheme) fromNixScm;
        appleScm = scheme.mkAppleScm
          (builtins.mapAttrs (_: _: false) applePlatformsExtracted);
      } ''
        mkdir -p $out/guix-modules
        cp ${self}/channels.scm ${self}/manifest.scm $out
        cd $out/guix-modules
        cp -r ${self}/guix/* .
        cp $fromNixScmPath from-nix.scm
        chmod +w aux-files
        cp ${self}/LICENSE* ${self}/flake.lock ./aux-files/
        ${writeProvenance pkgs "aux-files/nix-provenance"}
        chmod +w extracted
        cp $appleScmPath extracted/apple.scm
      '';

      guix-with-apple = let
        applePlatformsExtractedPaths =
          builtins.mapAttrs (name: _: "apple/extracted-${name}")
          applePlatformsExtracted;
      in pkgs.runCommand (mkNixPkgName "guix-with-apple") {
        passAsFile = [ "appleScm" ];
        appleScm = scheme.mkAppleScm applePlatformsExtractedPaths;
      } ''
        cp -rL ${guix-sans-apple}/ $out
        chmod +w $out/guix-modules
        cd $out/guix-modules
        chmod -R +w extracted
        cd extracted
        rm apple.scm
        cp $appleScmPath apple.scm
        mkdir apple
        ${let cp = from: to: "cp -rL ${apple}/${from} ${to}";
        in builtins.concatStringsSep "\n"
        (nixpkgs.lib.attrsets.mapAttrsToList cp applePlatformsExtractedPaths)}
      '';
    };
}
