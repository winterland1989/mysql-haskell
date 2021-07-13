--  Sample employee database 
--  Copyright (C) 2007,2008, MySQL AB

USE testMySQLHaskell;

DROP TABLE insert_test;

CREATE TABLE insert_test (
    f1  INT                    ,
    f2  BIT(16)                ,
    f3  TINYINT                ,
    f4  TINYINT UNSIGNED       ,
    f5  SMALLINT               ,
    f6  SMALLINT UNSIGNED      ,
    f7  MEDIUMINT              ,
    f8  MEDIUMINT UNSIGNED     ,
    f9  INT                    ,
    f10  INT UNSIGNED          ,
    f11  BIGINT                ,
    f12  BIGINT UNSIGNED       ,
    f13  DECIMAL               ,
    f14  FLOAT                 ,
    f15  DOUBLE                ,
    f16  DATE                  ,
    f17  DATETIME              ,
    f18  TIMESTAMP             ,
    f19  TIME                  ,
    f20  YEAR                  ,
    f21  CHAR(100)             ,
    f22  VARCHAR(100)          ,
    f23  BINARY(100)           ,
    f24  VARBINARY(100)        ,
    f25  BLOB                  ,
    f26  TEXT                  ,
    f27  VARCHAR(100)          ,
    f28  BINARY(100)           ,
    f29  TEXT                  ,
    f30  TEXT                  
);

