     timeMachineBinName = "time-machine";

      commands = mapToAttrs (pkgs:
        let
          system = pkgs.buildPlatform.system;
          packages = builtins.getAttr system basePackages;
          mkForGuixPkg = { guixPkg, suffix ? "" }:
            let
              cmd-time-machine = pkgs.writeShellApplication {
                name = timeMachineBinName;
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
