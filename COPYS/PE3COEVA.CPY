       01 CAI-PE9CCAI0.
           05 IO1-ENTRADA.
              10 AI0-CLIENTE         PIC X(8).
              10 AI0-CONTRATO        PIC X(18).
              10 AI0-PRODUCTO        PIC XX.
           05 AI0-SALIDA.
              10 AI0-COD-RET         PIC XX.
              10 AI0-OFI2-ERROR1     PIC X(20).
              10 AI0-OFI2-ERROR2     PIC X(20).
              10 AI0-MAS-SQLCODE     PIC ZZZZ.
              10 AI0-IMPRESION       PIC X.
              10 AI0-CLIENTE-SAL     PIC X(8).
              10 AI0-CONTRATO-SAL    PIC X(18).
              10 AI0-FECHA           PIC X(10).
              10 AI0-VALOR           PIC X(13).
       05 PRDTMOV2 OCCURS 25 TIMES INDEXED BY I-IND 
           07 MOV-CLIENTE            PIC X(8).
           07 MOV-FECHA              PIC X(10).
           07 MOV-CONTRATO           PIC X(18).
           07 MOV-VALOR              PIC X(13).

       05 SW-FIN-CURSOR-MOV   PIC X VALUE 'N'.
           88 SI-CURSOR-MOV         VALUE 'S'.
           88 NO-CURSOR-MOV         VALUE 'N'.