{ self, nixpkgs }:
let

  rkt = import ../version.nix;
  flags = (import ./flags.nix rkt);
  scripts = "${self}/scripts";

  cpGitignoreApacheMit = ''
    cp ${self}/nix/gitignore-skel .gitignore
    cp ${self}/LICENSE-Apache-2.0.txt ${self}/LICENSE-MIT.txt .
  '';
  mkInfoRkt = pkgs: flag: ''
    ${pkgs.racket}/bin/racket ${scripts}/mk-info-rkt.rkt ${flag} > info.rkt
  '';
  mkReadme = pkgs: scrbl: ''
    ${pkgs.racket}/bin/scribble --markdown --link-section \
      --dest-name README.md \
      ${scripts}/${scrbl}
  '';

  mkBuilt = { src, pkgsMaybeCross }:
    pkgsMaybeCross.libgit2.overrideAttrs (oldAttrs: {
      version = rkt.libgit2.version;
      src = src;
      patches = [ ];
      buildInputs = [ ];
      cmakeFlags = flags.forHostPlatform pkgsMaybeCross.hostPlatform;
    });

  mkPacked = pkgs:
    { isWindows, isDarwin, systemForBuild, rktPlatform, built, src, ... }@super:
    let
      libFileName = if isWindows then
        "libgit2-${rkt.soVersion}.dll"
      else if isDarwin then
        "libgit2.${rkt.soVersion}.dylib"
      else
        "libgit2.so.${rkt.soVersion}";

      builtLibPath =
        if isWindows then "bin/libgit2.dll" else "lib/${libFileName}";

      patchLibCommand = if isWindows then
        "echo No patch command needed for Windows DLLs."
      else if isDarwin then ''
        ${pkgs.racket}/bin/racket ${scripts}/patch-darwin-dylib.rkt \
          --llvm-objdump ${pkgs.llvm}/bin/llvm-objdump \
          --install_name_tool \
          ${pkgs.darwin.binutils-unwrapped}/bin/install_name_tool \
          ${libFileName}
      '' else ''
        ${pkgs.patchelf}/bin/patchelf --set-rpath \$ORIGIN ${libFileName}
      '';
    in super // {
      packed = runCmdWithRktJsonArgs {
        inherit pkgs systemForBuild;
        name = "${rktPlatform}-${rkt.pkgVersion}";
        extraArgs = {
          "arch+os" = rktPlatform;
          lib-filename = libFileName;
        };
        cmd = ''
          mkdir -p $out/${rktPlatform}
          cd $out/${rktPlatform}
          cp \
             ${src}/COPYING \
             ${src}/AUTHORS \
             ${src}/git.git-authors \
             ${src}/docs/changelog.md \
             .
          cp ${src}/README.md README-libgit2.md
          ${cpGitignoreApacheMit}
          cp ${built}/${builtLibPath} ${libFileName}
          chmod +w ${libFileName}
          ${patchLibCommand}
          chmod -w ${libFileName}
          ${mkInfoRkt pkgs "--platform-pkg"}
          ${mkReadme pkgs "platform-readme.scrbl"}
        '';
      };
    };

  mkMeta = pkgs:
    runCmdWithRktJsonArgs {
      inherit pkgs;
      name = "native-libs-${rkt.pkgVersion}";
      systemForBuild = pkgs.buildPlatform.config;
      cmd = ''
        mkdir -p $out/native-libs
        cd $out/native-libs
        ${cpGitignoreApacheMit}
        ${mkInfoRkt pkgs "--meta-pkg"}
        ${mkReadme pkgs "meta-readme.scrbl"}
      '';
    };

  runCmdWithRktJsonArgs = { name, systemForBuild, extraArgs ? { }, pkgs, cmd }:
    with builtins;
    with pkgs.lib;
    let
      lock-info = (importJSON ../flake.lock).nodes.nixpkgs;
      # otherwise, `sourceInfo`s are JSON-ized as store paths
      # (libgit2 is ok without this)
      safeAttrs = [ "lastModifiedDate" "narHash" "rev" "owner" "repo" "ref" ];
      cleanseSourceInfoAttrs = filterAttrs (k: v: elem k safeAttrs);
      assertLockGitHub = key:
        assertMsg (lock-info.${key}.type == "github")
        ''lock-info.${key} is not of type "github"'';
      env = {
        RKT_ENFORCE_ARGS = "1";
        RKT_JSON_ARGS = toJSON (extraArgs // {
          pkg-version = rkt.pkgVersion;
          breaking-change-label = rkt.breakingChangeLabel;
          system-for-build = systemForBuild;
          platforms = attrsets.catAttrs "rktPlatform" (import ./platforms.nix);
          libgit2-info = rkt.libgit2.src // { version = rkt.libgit2.version; };
          self-source-info = cleanseSourceInfoAttrs self.sourceInfo;
          "nixpkgs-source+lock-info" = assert assertLockGitHub "locked";
            assert assertLockGitHub "original";
            cleanseSourceInfoAttrs
            (lock-info.original // lock-info.locked // nixpkgs.sourceInfo);
        });
      };
    in pkgs.runCommand name env cmd;

in {
  mkPlatformsWithBuilt = { pkgs, platforms }@super:
    let src = pkgs.fetchFromGitHub rkt.libgit2.src;
    in super // {
      platformsWithBuilt = builtins.mapAttrs (rktPlatform:
        { pkgsMaybeCross }: rec {
          inherit src rktPlatform;
          isDarwin = pkgsMaybeCross.hostPlatform.isDarwin;
          isWindows = pkgsMaybeCross.hostPlatform.isWindows;
          systemForBuild = pkgsMaybeCross.buildPlatform.config;
          built = mkBuilt { inherit src pkgsMaybeCross; };
        }) platforms;
    };

  mkPlatformsWithPacked = { pkgs, platformsWithBuilt, ... }@super:
    super // {
      meta = mkMeta pkgs;
      platformsWithPacked =
        builtins.mapAttrs (_: mkPacked pkgs) platformsWithBuilt;
    };

  mkPackageBundles = { pkgs, meta, platformsWithPacked, ... }@super:
    with pkgs.lib;
    let
      partitioned = attrsets.mapAttrsToList (_:
        { isDarwin, packed, ... }:
        if isDarwin then {
          packedApple = packed;
        } else {
          packedSansApple = packed;
        }) platformsWithPacked;
      mkBundleName = name:
        "racket-libgit2-native-pkgs-${rkt.pkgVersion}-${name}";
    in super // rec {
      apple = pkgs.symlinkJoin {
        name = mkBundleName "apple-bundle";
        paths = attrsets.catAttrs "packedApple" partitioned;
      };
      sansApple = pkgs.symlinkJoin {
        name = mkBundleName "sans-apple-bundle";
        paths = [ meta ] ++ attrsets.catAttrs "packedSansApple" partitioned;
      };
      all = pkgs.symlinkJoin {
        name = mkBundleName "all";
        paths = [ apple sansApple ];
      };
      packages =
        lists.foldr trivial.mergeAttrs { inherit meta apple sansApple all; }
        (attrsets.mapAttrsToList (host:
          { built, packed, ... }: {
            "built-${host}" = built;
            "packed-${host}" = packed;
          }) platformsWithPacked);

    };
}
