# I used chatgpt to generate this template and then just
# modified to how I normally use these things.
{
  description = "My Haskell project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-compat }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      hpkgs = pkgs.haskellPackages.override {
        overrides = hnew: hold: {
          mysql-pure = hnew.callCabal2nix "mysql-pure" ./. { };
          # untill they figure out how to compile the test suite
          crypton-x509 = pkgs.haskell.lib.markUnbroken (pkgs.haskell.lib.dontCheck hold.crypton-x509);
        };
      };
    in
    {
      defaultPackage.x86_64-linux =  hpkgs.mysql-pure;
      inherit pkgs;
      devShell.x86_64-linux = hpkgs.shellFor {
        packages = ps : [ ps."mysql-pure" ];
        withHoogle = true;

        buildInputs = [
          hpkgs.haskell-language-server
          pkgs.ghcid
          pkgs.cabal-install
        ];
      };
    };
}
