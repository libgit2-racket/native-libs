{ self, nixpkgs, systemForBuild, rkt ? import ../version.nix
, platforms ? import ./platforms.nix, ... }:
with nixpkgs.lib;
with builtins;
with asserts;
let
  pkgs = import nixpkgs { system = systemForBuild; };

  src = pkgs.fetchFromGitHub rkt.libgit2.src;

  racket = "${pkgs.racket}/bin/racket";

  scribble = "${pkgs.racket}/bin/scribble";

  scripts = "${self}/scripts";

  scriptArgs = let
    lock-info = (importJSON ../flake.lock).nodes.nixpkgs;
    # otherwise, `sourceInfo`s are JSON-ized as store paths
    # (libgit2 is ok without this)
    safeAttrs = [ "lastModifiedDate" "narHash" "rev" "owner" "repo" "ref" ];
    cleanseSourceInfoAttrs = filterAttrs (k: v: elem k safeAttrs);
    assertLockGitHub = key:
      assertMsg (lock-info.${key}.type == "github")
      ''lock-info.${key} is not of type "github"'';
  in assert assertLockGitHub "locked";
  assert assertLockGitHub "original"; {
    pkg-version = rkt.pkgVersion;
    breaking-change-label = rkt.breakingChangeLabel;
    platforms = attrsets.catAttrs "rktPlatform" platforms;
    libgit2-info = rkt.libgit2.src // { version = rkt.libgit2.version; };
    self-source-info = cleanseSourceInfoAttrs self.sourceInfo;
    "nixpkgs-source+lock-info" = cleanseSourceInfoAttrs
      (lock-info.original // lock-info.locked // nixpkgs.sourceInfo);
  };
  mkRKT_JSON_ARGSenv = args: { RKT_JSON_ARGS = toJSON args; };

  osSpecific = rec {
    libFileName = {
      windows = "libgit2-${rkt.soVersion}.dll";
      darwin = "libgit2.${rkt.soVersion}.dylib";
      unix = "libgit2.so.${rkt.soVersion}";
    };
    builtLibPath = ((mapAttrs (x: so: "lib/${so}") libFileName) // {
      windows = "bin/libgit2.dll";
    });
    patchLibCommand = {
      windows = "echo No patch command needed for Windows DLLs.";
      unix = ''
        ${pkgs.patchelf}/bin/patchelf \
           --set-rpath \$ORIGIN \
           ${libFileName.unix}
      '';
      darwin = ''
        ${racket} ${scripts}/patch-darwin-dylib.rkt \
          --llvm-objdump ${pkgs.llvm}/bin/llvm-objdump \
          --install-name-tool \
          ${pkgs.darwin.binutils-unwrapped}/bin/install_name_tool \
          ${libFileName.darwin}
      '';
    };
  };

  osSpecificForHostPlatform = { isWindows, isDarwin, ... }:
    let
      os =
        if isWindows then "windows" else if isDarwin then "darwin" else "unix";
    in mapAttrs (k: v: getAttr os v) osSpecific;

  mkForHost = { nixSystem, rktPlatform, crossAttr }:
    let

      pkgsMaybeCross = if nixSystem == systemForBuild then
        pkgs
      else
        pkgs.pkgsCross.${crossAttr};

      hostPlatform = pkgsMaybeCross.hostPlatform;

    in with osSpecificForHostPlatform hostPlatform; {

      name = rktPlatform;

      value = rec {

        built = pkgsMaybeCross.libgit2.overrideAttrs (oldAttrs: {
          version = rkt.libgit2.version;
          src = src;
          patches = [ ];
          buildInputs = [ ];
          cmakeFlags = (import ./flags.nix).forHostPlatform hostPlatform;
        });

        packed = pkgs.runCommandLocal "${rktPlatform}-${rkt.pkgVersion}"
          (mkRKT_JSON_ARGSenv (scriptArgs // {
            "arch+os" = rktPlatform;
            lib-filename = libFileName;
          })) ''
            mkdir -p $out/${rktPlatform}
            cd $out/${rktPlatform}
            cp \
               ${src}/COPYING \
               ${src}/AUTHORS \
               ${src}/git.git-authors \
               ${src}/docs/changelog.md \
               .
            cp ${src}/README.md README-libgit2.md
            cp ${self}/nix/gitignore-skel .gitignore
            cp ${built}/${builtLibPath} ${libFileName}
            chmod +w ${libFileName}
            ${patchLibCommand}
            chmod -w ${libFileName}
            ${racket} ${scripts}/mk-info-rkt.rkt --platform-pkg > info.rkt
            ${scribble} --markdown --dest-name README.md \
              ${scripts}/generate-readme.scrbl
          '';
      };
    };

  canBuildForHost = { crossAttr, ... }:
    pkgs.buildPlatform.isDarwin || ((isString crossAttr)
      && (!pkgs.pkgsCross.${crossAttr}.hostPlatform.isDarwin));

  packagesByHost =
    listToAttrs (map mkForHost (filter canBuildForHost platforms));

  meta = pkgs.runCommandLocal "native-libs-${rkt.pkgVersion}"
    (mkRKT_JSON_ARGSenv scriptArgs) ''
      mkdir -p $out/native-libs
      cd $out/native-libs
      cp ${self}/nix/gitignore-skel .gitignore
      ${racket} ${scripts}/mk-info-rkt.rkt --meta-pkg > info.rkt
    '';

  mkBundle = name: lst:
    pkgs.symlinkJoin {
      name = "racket-libgit2-native-pkgs-${rkt.pkgVersion}-${name}";
      paths = [ meta ] ++ (attrsets.catAttrs "packed" lst);
    };

in {

  src = src;

  packages = lists.foldr trivial.mergeAttrs {

    inherit meta;

    all = mkBundle "all" (attrValues packagesByHost);

  } (mapAttrsToList (host:
    { built, packed }: {
      "built-${host}" = built;
      "packed-${host}" = packed;
    }) packagesByHost);

}
