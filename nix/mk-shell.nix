{ pkgs, haskellPackages }:

pkgs.mkShell {
  buildInputs = [
    (haskellPackages.ghcWithPackages (haskellPackages:
      with haskellPackages; [
        base
        binary
        binary-ieee754
        binary-parsers
        blaze-textual
        bytestring
        bytestring-lexing
        cryptonite
        io-streams
        memory
        monad-loops
        network
        scientific
        tcp-streams
        text
        time
        tls
        vector
        wire-streams
        word24
      ]))
    pkgs.cabal-install
    pkgs.cachix
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.hpack
    pkgs.niv
    pkgs.ormolu
  ];
}
