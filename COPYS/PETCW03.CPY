           EXEC SQL DECLARE DTPR.PRDTW03 TABLE
               (
                   W03_CLIENTE        CHAR(8) NOT NULL,
                   W03_PRODUCTO       CHAR(2) NOT NULL,
                   W03_FECHA          CHAR(10) NOT NULL
               )
           END-EXEC.
