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
      server.succeed("mysql -u root -e \"GRANT BINLOG MONITOR, REPLICATION SLAVE, CREATE USER ON *.* TO 'testMySQLHaskell'@'localhost';\"")
      server.succeed("mysql -u root -e \"SHOW VARIABLES LIKE 'have_ssl'\" | grep YES")
      print(server.succeed("MYSQL_TLS_CA_PATH=${package}/test/cert/ca.pem ${package}/bin/integration/integration"))
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
          ssl_ca   = "${package}/test/cert/ca.pem";
          ssl_cert = "${package}/test/cert/server-cert.pem";
          ssl_key  = "${package}/test/cert/server-key.pem";
        };
      };
    };
  };
  integrated-checks-mysql80 = pkgs.testers.nixosTest {
    name = "mysql-haskell-mysql80-test";
    testScript = ''
      server.start()
      server.wait_for_unit("mysql.service")
      server.wait_until_succeeds("mysql -u root -e 'SELECT 1'")

      server.succeed("mysql -u root -e \"CREATE DATABASE testMySQLHaskell;\"")

      # Main test user (mysql_native_password so existing tests including password change work over plain TCP)
      server.succeed("mysql -u root -e \"CREATE USER 'testMySQLHaskell'@'localhost' IDENTIFIED WITH mysql_native_password;\"")
      server.succeed("mysql -u root -e \"GRANT ALL ON testMySQLHaskell.* TO 'testMySQLHaskell'@'localhost';\"")
      server.succeed("mysql -u root -e \"GRANT REPLICATION SLAVE, REPLICATION CLIENT, CREATE USER ON *.* TO 'testMySQLHaskell'@'localhost';\"")

      # User with caching_sha2_password (MySQL 8.0 default) for SHA256 fast auth test
      server.succeed("mysql -u root -e \"CREATE USER 'testMySQLHaskellSha2'@'localhost' IDENTIFIED BY 'testPassword123';\"")
      server.succeed("mysql -u root -e \"GRANT ALL ON testMySQLHaskell.* TO 'testMySQLHaskellSha2'@'localhost';\"")

      # User with mysql_native_password for AuthSwitchRequest test
      server.succeed("mysql -u root -e \"CREATE USER 'testMySQLHaskellNative'@'localhost' IDENTIFIED WITH mysql_native_password BY 'nativePass123';\"")
      server.succeed("mysql -u root -e \"GRANT ALL ON testMySQLHaskell.* TO 'testMySQLHaskellNative'@'localhost';\"")

      # Pre-cache the caching_sha2_password verifier by logging in via unix socket
      server.succeed("mysql -u testMySQLHaskellSha2 -ptestPassword123 -e 'SELECT 1'")

      # Run the full integration test suite (sha2 tests are conditionally included for MySQL 8.0+)
      server.succeed("mysql -u root -e \"SHOW VARIABLES LIKE 'have_ssl'\" | grep YES")
      print(server.succeed("MYSQL_TLS_CA_PATH=${package}/test/cert/ca.pem ${package}/bin/integration/integration"))
    '';
    nodes.server = {
      virtualisation.memorySize = 2048;
      virtualisation.diskSize = 1024;
      services.mysql = {
        enable = true;
        package = pkgs.mysql80;
        settings.mysqld = {
          max_allowed_packet = "256M";
          log_bin = "mysql-bin";
          server_id = 1;
          binlog_format = "ROW";
          default_authentication_plugin = "caching_sha2_password";
          ssl_ca   = "${package}/test/cert/ca.pem";
          ssl_cert = "${package}/test/cert/server-cert.pem";
          ssl_key  = "${package}/test/cert/server-key.pem";
        };
      };
    };
  };
}
