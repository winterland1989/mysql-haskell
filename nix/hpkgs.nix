{
  pkgs ? import ./pkgs.nix { },
}:
pkgs.haskellPackages.override {
  overrides = hnew: hold: {
    mysql-haskell =
      pkgs.haskell.lib.overrideCabal (hnew.callCabal2nix "mysql-haskell" ../. { })
        {
          postBuild = ''
            mkdir -p $out/bin/test/test
            cp ./dist/build/test/test $out/bin/test/test-exe
            cp -r ./test/json-data $out/bin/test/test/json-data
            cp -r ./test/cert $out/bin/test/test/cert
          '';
          checkPhase = ''
            echo "tests run in VM"
          '';
        };
    ram = hnew.callHackageDirect {
      pkg = "ram";
      ver = "0.21.1";
      sha256 = "sha256-J+gP+rZft1xkxzxmvXcktnDIymRkjg5u5wmhEge3+GQ=";
    } {};
  };
}
