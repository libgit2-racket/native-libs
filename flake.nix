{
  description = "Flake to generate Racket libgit2 native library packages";

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOs";
      repo = "nixpkgs";
      # NOTE: the nixpkgs version used here determines MACOSX_VERSION_MIN
      ref = "nixos-21.11";
    };
  };

  outputs = { self, nixpkgs }:
    let

      rkt = import "${self}/version.nix";
      build = import "${self}/nix/build.nix" { inherit self nixpkgs rkt; };

      resolveCrossAttr = crossAttr:
        # Per <https://nixos.org/manual/nixpkgs/stable/#sec-cross-usage>,
        # this indirection should be unneeded when
        # <https://github.com/NixOS/nixpkgs/issues/34274 is fixed>.
        builtins.getAttr crossAttr nixpkgs.lib.systems.examples;
      darwinLocalSystem = { localSystem = resolveCrossAttr "x86_64-darwin"; };
      pkgsDarwin = import nixpkgs darwinLocalSystem;
      pkgsDarwinCross = import nixpkgs (darwinLocalSystem // {
        crossSystem = resolveCrossAttr "aarch64-darwin";
      });
      pkgsLinux = import nixpkgs { localSystem = resolveCrossAttr "gnu64"; };
      supportedBuildPlatforms = [ pkgsLinux pkgsDarwin ];

      darwinHosts =
        builtins.map build.mkDarwinHost [ pkgsDarwin pkgsDarwinCross ];

      mapToAttrs = proc: lst: builtins.listToAttrs (builtins.map proc lst);

      basePackages = mapToAttrs (pkgs: {
        name = pkgs.buildPlatform.system;
        value = build.mkPackagesForBuildPlatform pkgs darwinHosts;
      }) supportedBuildPlatforms;

      timeMachineBinName = "run-time-machine";

      commands = mapToAttrs (pkgs:
        let
          system = pkgs.buildPlatform.system;
          packages = builtins.getAttr system basePackages;
          mkForGuixPkg = { guixPkg, suffix ? "" }:
            let
              cmd-time-machine = pkgs.writeShellApplication {
                name = timeMachineBinName;
                runtimeInputs = [ ];
                text = let
                  text = ''
                    guix time-machine \
                      -C ${guixPkg}/channels.scm \
                      -- shell guix "$1" \
                      -L ${guixPkg}/guix-modules \
                      "''${@:2}"
                  '';
                in ''
                  echo ${text}
                  ${text}
                '';

              };
            in {
              packages = { "cmd-time-machine${suffix}" = cmd-time-machine; };
              apps = {
                "${timeMachineBinName}${suffix}" = {
                  type = "app";
                  program = "${cmd-time-machine}/bin/${timeMachineBinName}";
                };
              };
            };

          groups = builtins.map mkForGuixPkg [
            { guixPkg = packages.guix-with-apple; }
            {
              guixPkg = packages.guix-sans-apple;
              suffix = "-sans-apple";
            }
          ];
        in {
          name = system;
          value = with nixpkgs.lib;
            mapToAttrs (attr: {
              name = attr;
              value = lists.foldr trivial.mergeAttrs { }
                (attrsets.catAttrs attr groups);
            }) [ "packages" "apps" ];
        }) supportedBuildPlatforms;

    in rec {

      packages =
        builtins.mapAttrs (system: base: base // commands.${system}.packages)
        basePackages;

      defaultPackage =
        builtins.mapAttrs (_: builtins.getAttr "guix-with-apple") packages;

      apps = builtins.mapAttrs (_: { apps, ... }: apps) commands;

      defaultApp =
        builtins.mapAttrs (_: builtins.getAttr timeMachineBinName) apps;
    };
}
