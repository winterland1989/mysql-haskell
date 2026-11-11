-- 300,000 rows of the column types the employees table lacks: DECIMAL,
-- DATETIME with microseconds, DOUBLE and about 500 bytes of non-ASCII TEXT.
SET SESSION cte_max_recursion_depth = 300000;
DROP TABLE IF EXISTS mixed_types;
CREATE TABLE mixed_types (
    id      INT NOT NULL PRIMARY KEY,
    amount  DECIMAL(12, 2) NOT NULL,
    created DATETIME(6) NOT NULL,
    ratio   DOUBLE NOT NULL,
    body    TEXT NOT NULL
) CHARACTER SET utf8mb4;
INSERT INTO mixed_types
WITH RECURSIVE seq (n) AS (SELECT 1 UNION ALL SELECT n + 1 FROM seq WHERE n < 300000)
SELECT n,
       n * 1.25,
       TIMESTAMP('2020-01-01') + INTERVAL n * 7 SECOND + INTERVAL n MICROSECOND,
       n / 7,
       REPEAT(CONCAT('row ', n, ' café '), 30)
FROM seq;
