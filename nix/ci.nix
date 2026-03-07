{
  hpkgs ? import ./hpkgs.nix { },
  pkgs ? import ./pkgs.nix { },
}:
let
  package = import ../default.nix { inherit hpkgs; };
in
{
  build = package;
  integrated-checks = pkgs.testers.nixosTest {
    name = "mysql-haskell-test";
    testScript = ''
      server.start()
      server.wait_for_unit("mysql.service")
      server.wait_until_succeeds("mysql -u root -e 'SELECT 1'")
      server.succeed("mysql -u root -e \"CREATE USER 'testMySQLHaskell'@'localhost';\"")
      server.succeed("mysql -u root -e \"CREATE DATABASE testMySQLHaskell;\"")
      server.succeed("mysql -u root -e \"GRANT ALL ON testMySQLHaskell.* TO 'testMySQLHaskell'@'localhost';\"")
      server.succeed("mysql -u root -e \"GRANT BINLOG MONITOR, REPLICATION SLAVE ON *.* TO 'testMySQLHaskell'@'localhost';\"")
      print(server.succeed("${package}/bin/integration/integration"))
    '';
    nodes.server = {
      virtualisation.memorySize = 2048;
      virtualisation.diskSize = 1024;
      services.mysql = {
        enable = true;
        package = pkgs.mariadb;
        settings.mysqld = {
          max_allowed_packet = "256M";
          log_bin = "mysql-bin";
          server_id = 1;
          binlog_format = "ROW";
        };
      };
    };
  };
}
