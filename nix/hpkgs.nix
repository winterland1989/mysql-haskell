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
            mkdir -p $out/test/cert
            cp ./test/cert/ca.pem $out/test/cert/ca.pem
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
      ver = "2.1.7";
      sha256 = "sha256-mKWBG9HtZ8hLlSVbCqxBn8Kr4ZGxM2aFDiPEWuG0mFM=";
    };
    ram = fromHackage hnew {
      pkg = "ram";
      ver = "0.22.1";
      sha256 = "sha256-viwoo1u5jRGO2pTzQLlJ/PgkLmxeafTg8DDGTylpcPo=";
    };
    crypton-x509 = fromHackage hnew {
      pkg = "crypton-x509";
      ver = "1.9.2";
      sha256 = "sha256-b8GjnnVG1N9moeMgGQi6NbnCj22rsiE4F3mCD73R6qo=";
    };
    crypton-x509-store = fromHackage hnew {
      pkg = "crypton-x509-store";
      ver = "1.9.0";
      sha256 = "sha256-GQcCEDOYh9N42GSh/A0pYGuIRqk3zwZ/lpOGxFrnClQ=";
      # Hackage revision 2 widens crypton to < 2.2; the tarball says < 1.2.
      rev = { revision = "2"; sha256 = "1i2ccxl9b861yg61vvmzikps1aqs6hrzkycngmr1332xnacqlf5d"; };
    };
    crypton-x509-system = fromHackage hnew {
      pkg = "crypton-x509-system";
      ver = "1.9.0";
      sha256 = "sha256-a3rnvpO1xYVFxxIKhp7aObh+oVj49KJqGcv2NuaAgPs=";
    };
    crypton-x509-validation = fromHackage hnew {
      pkg = "crypton-x509-validation";
      ver = "1.9.1";
      sha256 = "sha256-YKufVgXC8qz80tScE3vENVrwJD1MgZYRNnjqirscrLA=";
      # Hackage revision 3 widens crypton to < 2.2; the tarball says < 1.2.
      rev = { revision = "3"; sha256 = "1l6ds52d8xhqiryf84vjpnpvzmyjmx05zg3h1x7k3picfmhj2gad"; };
    };
    tls = fromHackage hnew {
      pkg = "tls";
      ver = "2.4.9";
      sha256 = "sha256-e6Bitfc+xy5XDGpq/+LkgZ3cIC1doVKsJjkujbtIGE8=";
    };
    hpke = fromHackage hnew {
      pkg = "hpke";
      ver = "0.2.1";
      sha256 = "sha256-zTZ33VYYve8dD5GrNuFtaUlII7O2+3L80PuCDIFUc4U=";
    };
    mlkem = fromHackage hnew {
      pkg = "mlkem";
      ver = "0.2.3.0";
      sha256 = "sha256-shGCUx8g+79QVKnNOWSsKcjp2SRzBhqvxl1fzR9ohWk=";
    };
  };
}
