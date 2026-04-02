{
  pkgs ? import ./pkgs.nix { },
}:
pkgs.haskellPackages.override {
  overrides = hnew: hold: {
    mysql-haskell =
      pkgs.haskell.lib.overrideCabal (hnew.callCabal2nix "mysql-haskell" ../. { })
        {
          postBuild = ''
            mkdir -p $out/bin/integration
            cp ./dist/build/integration/integration $out/bin/integration/integration
          '';
          checkPhase = ''
            echo "tests run in VM"
          '';
        };
  };
}
