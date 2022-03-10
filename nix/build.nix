{ self, nixpkgs }:
let

  rkt = import ../version.nix;
  flags = (import ./flags.nix rkt);
  scripts = "${self}/scripts";

  mkBuilt = { src, pkgsMaybeCross }:
    pkgsMaybeCross.libgit2.overrideAttrs (oldAttrs: {
      version = rkt.libgit2.version;
      src = src;
      patches = [ ];
      buildInputs = [ ];
      cmakeFlags = flags.forHostPlatform pkgsMaybeCross.hostPlatform;
    });

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
          platforms = attrsets.catAttrs "rktPlatform" platforms;
          libgit2-info = rkt.libgit2.src // { version = rkt.libgit2.version; };
          self-source-info = cleanseSourceInfoAttrs self.sourceInfo;
          "nixpkgs-source+lock-info" = assert assertLockGitHub "locked";
            assert assertLockGitHub "original";
            cleanseSourceInfoAttrs
            (lock-info.original // lock-info.locked // nixpkgs.sourceInfo);
        });
      };
    in pkgs.runCommand name env cmd;

  osSpecificVars = pkgs: rec {
    libFileName = {
      windows = "libgit2-${rkt.soVersion}.dll";
      darwin = "libgit2.${rkt.soVersion}.dylib";
      unix = "libgit2.so.${rkt.soVersion}";
    };
    builtLibPath = ((builtins.mapAttrs (x: so: "lib/${so}") libFileName) // {
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
        ${pkgs.racket} ${scripts}/patch-darwin-dylib.rkt \
          --llvm-objdump ${pkgs.llvm}/bin/llvm-objdump \
          --install_name_tool \
          ${pkgs.darwin.binutils-unwrapped}/bin/install_name_tool \
          ${libFileName.darwin}
      '';
    };
  };

in {
  mkPlatformsWithBuilt = { pkgs, platforms }@super:
    let src = pkgs.fetchFromGitHub rkt.libgit2.src;
    in super // {
      platformsWithBuilt = builtins.mapAttrs (rktPlatform:
        { pkgsMaybeCross }: rec {
          inherit src rktPlatform;
          isDarwin = pkgsMaybeCross.hostPlatform.isDarwin;
          isWindows = pkgsMaybeCross.hostPlatform.isWindows;
          os = if isWindows then
            "windows"
          else if isDarwin then
            "darwin"
          else
            "unix";
          systemForBuild = pkgsMaybeCross.buildPlatform.config;
          built = mkBuilt { inherit src pkgsMaybeCross; };
        }) platforms;
    };

  mkPlatformsWithPacked = { pkgs, platformsWithBuilt, ... }@super:
    let
      racket = "${pkgs.racket}/bin/racket";
      scribble = "${pkgs.racket}/bin/scribble";
      cpGitignoreApacheMit = ''
        cp ${self}/nix/gitignore-skel .gitignore
        cp ${self}/LICENSE-Apache-2.0.txt ${self}/LICENSE-MIT.txt .
      '';
      mkReadme = scrbl: ''
        ${scribble} --markdown --link-section --dest-name README.md \
           ${scripts}/${scrbl}
           '';
    in super // {
      meta = runCmdWithRktJsonArgs {
        inherit pkgs;
        name = "native-libs-${rkt.pkgVersion}";
        systemForBuild = pkgs.buildPlatform.config;
        cmd = ''
          mkdir -p $out/native-libs
          cd $out/native-libs
          ${cpGitignoreApacheMit}
          ${racket} ${scripts}/mk-info-rkt.rkt --meta-pkg > info.rkt
          ${mkReadme "meta-readme.scrbl"}
        '';
      };
      platformsWithPacked = builtins.mapAttrs (_:
        { os, systemForBuild, rktPlatform, built, ... }@super:
        with builtins.mapAttrs (k: v: builtins.getAttr os v)
          (osSpecificVars pkgs);
        super // {
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
              ${racket} ${scripts}/mk-info-rkt.rkt --platform-pkg > info.rkt
              ${mkReadme "platform-readme.scrbl"}
            '';
          };
        }) platformsWithBuilt;
    };

  mkPackageBundles = { pkgs, meta, platformsWithPacked, ... }@super:
    with pkgs.lib;
    let
      partitioned = attrsets.mapAttrsToList ({ isDarwin, packed, ... }:
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
