let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs-unstable { };
in import nix/mk-shell.nix {
  pkgs = pkgs;
  haskellPackages = pkgs.haskell.packages.ghc901.extend (self: super: {
    word24 = super.callCabal2nix "word24" sources.word24 { };
    blaze-textual =
      super.callCabal2nix "blaze-textual" sources.blaze-textual { };
  });
}
