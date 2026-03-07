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
    ram = hnew.callHackageDirect {
      pkg = "ram";
      ver = "0.21.1";
      sha256 = "sha256-J+gP+rZft1xkxzxmvXcktnDIymRkjg5u5wmhEge3+GQ=";
    } {};
  };
}
