{ lib }:
with lib;
with builtins; rec {

  mapToAttrs = proc: lst: listToAttrs (map proc lst);

  concatAttrsSuffixed = let
    addSuffix = suffix:
      attrsets.mapAttrs' (name: value: {
        inherit value;
        name = name + (if suffix == "" then "" else "-") + suffix;
      });
  in attrs:
  lists.foldr trivial.mergeAttrs { } (attrsets.mapAttrsToList addSuffix attrs);

}
