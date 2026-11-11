{
  pkgs ? import ./pkgs.nix { },
}:
let
  # Dependencies are taken as working, so their test suites are skipped.
  fromHackage = hnew: release: pkgs.haskell.lib.dontCheck (hnew.callHackageDirect release { });
in
pkgs.haskellPackages.override {
  overrides = hnew: hold: {
    mysql-haskell =
      pkgs.haskell.lib.overrideCabal (hnew.callCabal2nix "mysql-haskell" ../. { })
        {
          postBuild = ''
            mkdir -p $out/bin/integration
            cp ./dist/build/integration/integration $out/bin/integration/integration
            cp ./dist/build/integration-mariadb/integration-mariadb $out/bin/integration/integration-mariadb
            mkdir -p $out/test/cert
            cp ./test/cert/ca.pem $out/test/cert/ca.pem
            cp ./test/cert/client-cert.pem $out/test/cert/client-cert.pem
            cp ./test/cert/client-key.pem $out/test/cert/client-key.pem
            cp ./test/cert/server-cert.pem $out/test/cert/server-cert.pem
            cp ./test/cert/server-key.pem $out/test/cert/server-key.pem
          '';
          checkPhase = ''
            echo "tests run in VM"
          '';
        };
    # Decision: crypton 2 and the ram-based tls/x509 stack come from Hackage.
    # Updating the pin does not help: nixpkgs-unstable of 7 Oct 2026 still stops
    # at crypton 1.1, its tls/x509 cap crypton < 1.2, and it has dropped mysql80.
    crypton = fromHackage hnew {
      pkg = "crypton";
      ver = "2.2.6";
      sha256 = "sha256-Ll/UXRpvON64iincSgzFkcIA9CK9EeAygcjveYTlVoU=";
    };
    ram = fromHackage hnew {
      pkg = "ram";
      ver = "0.22.1";
      sha256 = "sha256-viwoo1u5jRGO2pTzQLlJ/PgkLmxeafTg8DDGTylpcPo=";
    };
    crypton-x509 = fromHackage hnew {
      pkg = "crypton-x509";
      ver = "1.10.1";
      sha256 = "sha256-HO3P1sWDmcesBB40ERkMTVEmON5Z6J5CZ7fCfm2jj0s=";
    };
    crypton-x509-store = fromHackage hnew {
      pkg = "crypton-x509-store";
      ver = "1.10.0";
      sha256 = "sha256-aPUxmoF6OQSq8xJyFfPQwG4+EAwoBZs3y6vxSERqVdI=";
    };
    # Decision: unwitch is not in the pinned nixpkgs, so it comes from Hackage.
    unwitch = fromHackage hnew {
      pkg = "unwitch";
      ver = "3.1.0";
      sha256 = "sha256-vE8uezsm3eAHdos/HQnMXSO3pkcZ2dTuUwobeTbYlf0=";
    };
    crypton-x509-system = fromHackage hnew {
      pkg = "crypton-x509-system";
      ver = "1.10.0";
      sha256 = "sha256-CweEELCYvhtGiodo7x0PK4lr8xxD6khrXsI5/ZLqHb8=";
    };
    crypton-x509-validation = fromHackage hnew {
      pkg = "crypton-x509-validation";
      ver = "1.10.1";
      sha256 = "sha256-zzhbkOS/0+NpG67dCPLH+rt8Hq31vEZeukEJKL3tRJE=";
    };
    tls = fromHackage hnew {
      pkg = "tls";
      ver = "2.5.1";
      sha256 = "sha256-24NGzNJMJUwMuCkyQSb+uLULBosb4b4/QP0VtH9NkKg=";
    };
    hpke = fromHackage hnew {
      pkg = "hpke";
      ver = "0.4.0";
      sha256 = "sha256-jIYXGVaSSM4DCiG3Sdu7U6x8+0JsoAcIff0A08FJb7U=";
    };
    # tls 2.5 needs ech-config >= 0.0.2; the pinned nixpkgs has 0.0.1.
    ech-config = fromHackage hnew {
      pkg = "ech-config";
      ver = "0.0.2";
      sha256 = "sha256-RQmvE/LDL003DAI473kZEIRd3B0WyGETzgsYPUldEpU=";
    };
  };
}
