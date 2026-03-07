{
  hpkgs ? import ./nix/hpkgs.nix { },
  pkgs ? import ./nix/pkgs.nix { },
}:

hpkgs.shellFor {
  packages = ps: [ ps.mysql-haskell ];
  withHoogle = false;

  buildInputs = [
    hpkgs.haskell-language-server
    pkgs.ghcid
    pkgs.cabal-install
  ];
}
