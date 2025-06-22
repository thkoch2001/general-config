[(final: prev:
{
  wpaperd = prev.wpaperd.overrideAttrs (old: {
    patches = (old.patches or []) ++ [
      ./patches/wpaperd_fix_140.patch
    ];
  });
})]
