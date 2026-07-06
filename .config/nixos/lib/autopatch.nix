{ lib, patchesDir }:

let
  # Ensure baseDir is treated as a Nix path
  patchesPath = if builtins.isString patchesDir then /. + patchesDir else patchesDir;
  entries = builtins.readDir patchesPath;

  # Filter to only include directories
  pkgDirs = lib.filterAttrs (_: type: type == "directory") entries;

  # Helper function to recursively find all .patch files
  getPatchesFor = pkgName:
    let
      pkgPath = patchesPath + "/${pkgName}";
      allFiles = lib.filesystem.listFilesRecursive pkgPath;
    in
    builtins.filter (f: lib.hasSuffix ".patch" (builtins.baseNameOf f)) allFiles;

in
final: prev:
let
  # 1. Get a list of directory names that actually exist as packages in nixpkgs
  validPkgNames = builtins.filter (name: prev ? ${name}) (lib.attrNames pkgDirs);

  # 2. Compute the patches for each valid package
  # This creates an attrset like: { vim = [ ./patch1 ]; firefox = [ ./patch2 ]; }
  pkgPatches = lib.genAttrs validPkgNames (name: getPatchesFor name);

  # 3. Filter out packages that ended up having 0 patches
  pkgsWithPatches = lib.filterAttrs (name: patches: builtins.length patches > 0) pkgPatches;

in
# 4. Apply the overrides using the pre-computed patch lists
lib.mapAttrs (name: patches:
  prev.${name}.overrideAttrs (oldAttrs: {
    patches = (oldAttrs.patches or []) ++ patches;
  })
) pkgsWithPatches
