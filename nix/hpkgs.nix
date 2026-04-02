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
            mkdir -p $out/test/cert
            cp ./test/cert/ca.pem $out/test/cert/ca.pem
            cp ./test/cert/server-cert.pem $out/test/cert/server-cert.pem
            cp ./test/cert/server-key.pem $out/test/cert/server-key.pem
          '';
          checkPhase = ''
            echo "tests run in VM"
          '';
        };
  };
}
