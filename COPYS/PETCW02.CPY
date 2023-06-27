           EXEC SQL DECLARE DTPR.PRDTW02 TABLE
               (
                   W02_CLIENTE        CHAR(8)  NOT NULL,
                   W02_FECHA_MOV      CHAR(10) NOT NULL,
                   W02_CONTRATO       CHAR(18) NOT NULL,
                   W02_VALOR          CHAR(13) NOT NULL
               )
           END-EXEC.
