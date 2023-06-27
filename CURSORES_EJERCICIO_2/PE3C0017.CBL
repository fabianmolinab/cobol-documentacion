
       ID DIVISION.
      ******************************************************************

       PROGRAM-ID.      PE3C0017.

      ******************************************************************
      *                                                                *
      *   FECHA ...........:  JUNIO 2023                               *
      *   AUTOR ...........:  FABIAN MOLINA                            *
      *                       INETUM(COLOMBIA)                         *
      *   LENGUAJE ........:  COBOL                                    *
      *                                                                *
      *                         DESCRIPCION:                           *
      *                         ============                           *
      *   PROGRAMA PADRE QUE HACE UNA CONSULTA DB2 POR MEDIO DE
      *   CURSORES                                                     *
      ******************************************************************

       AUTHOR.          FABIAN ANDRES MOLINA BERMUDEZ.
       DATE-WRITTEN.    26/JUN/2023.
       DATE-COMPILED.
      ******************************************************************
      *                      ENVIRONMENT DIVISION                      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-3090.
       OBJECT-COMPUTER. IBM-3090.
      ******************************************************************
      *                    INPUT OUTPUT SECTION                        *
      ******************************************************************
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      **************ARCHIVO DE ENTRADA CON LOS DATOS *******************
           SELECT ENTRADA   ASSIGN TO ENTRADA
                            FILE STATUS IS WS-ESTADO.
      *****************ARCHIVO DE SALIDA********************************
           SELECT SALIDA    ASSIGN TO SALIDA
                            FILE STATUS IS WS-ESTADO.
      ******************************************************************
      *                         DATA DIVISION                          *
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.

       FD  ENTRADA RECORDING MODE IS F
                     BLOCK CONTAINS 0 RECORDS.
       01 REG-ENTRADA1.
           05 REG-CLIENTE                  PIC X(8).
           05 REG-CONTRATO.
               07 REG-BANCO                PIC X(4).
               07 REG-OFICINA              PIC X(4).
               07 REG-PRODUCTO             PIC X(2).
               07 REG-CONSECUTIVO          PIC X(8).
       FD  SALIDA
                     RECORDING MODE IS F
                     BLOCK CONTAINS 0 RECORDS.
       01 REG-SALIDA             PIC X(150).
      ******************************************************************
      *                    WORKING-STORAGE SECTION                     *
      ******************************************************************
       WORKING-STORAGE SECTION.


      ******************************************************************
      *                VARIABLES DE CODIGOS DE RETORNO                 *
      ******************************************************************
       01 WS-ESTADO                      PIC XX    VALUE '00'.

      ******************************************************************
      *                           SWITCHES                             *
      ******************************************************************
       01 SWITCHES.
          05 SW-FIN-DATA1             PIC X      VALUE 'N'.
             88 SI-FIN-DATA1                     VALUE 'S'.
             88 NO-FIN-DATA1                     VALUE 'N'.

      ******************************************************************
      *                          CONTADORES                            *
      ******************************************************************
       01 CONTADORES.
          05 WS-LEIDOS-DATA           PIC 9(09) VALUE ZEROS.
          05 WS-ESCRITOS              PIC 9(09) VALUE ZEROS.

       01 WS-TITULO1.
          05 CTE-BBVA                 PIC X(80) VALUE
          '                                        BBVA'.
       01 WS-TITULO2.
          05 CTE-CLIENTE              PIC X(7) VALUE 'CLIENTE'.
          05 FILLER                   PIC X(5) VALUE SPACES.
          05 CTE-FECHA                PIC X(9) VALUE 'FECHA_MOV'.
          05 FILLER                   PIC X(5) VALUE SPACES.
          05 CTE-CONTRATO             PIC X(8) VALUE 'CONTRATO'.
          05 FILLER                   PIC X(9) VALUE SPACES.
          05 CTE-VALOR                PIC X(8) VALUE 'VALOR'.

       01 WS-CUERPO.
          05 WS-CLIENTE               PIC X(8).
          05 FILLER                   PIC XX VALUE SPACES.
          05 WS-FECHA                 PIC X(10).
          05 FILLER                   PIC XX VALUE SPACES.
          05 WS-CONTRATO              PIC X(18).
          05 FILLER                   PIC XX VALUE SPACES.
          05 WS-VALOR                 PIC ZZZ,ZZZ,ZZZ,ZZZ.
      ******************************************************************
      *                    LINKAGE SECTION                             *
      ******************************************************************
       COPY PE9CCAI0.

       LINKAGE SECTION.


      ******************************************************************
      *                       PROCEDURE DIVISION                       *
      ******************************************************************
       PROCEDURE DIVISION.

           PERFORM 01-INICIO
           PERFORM 02-PROCESO UNTIL SI-FIN-DATA1
           PERFORM 03-FINAL.
      ******************************************************************
      *                     000000-INICIO                              *
      *   - ABRIR ARCHIVOS DE ENTRADA Y SALIDA                         *
      *   - PRIMERA LECTURA DEL ARCHIVO DE ENTRADA                     *
      ******************************************************************
       01-INICIO.

           PERFORM 011-ABRIR-ARCHIVOS
           PERFORM 012-LEER-ARCHIVOS
           PERFORM 013-GENERAR-CABECERA.

      ******************************************************************
      *                     000010-ABIR-ARCHIVO                        *
      *   - SE ABRIR EL ARCHIVO DE ENTRADA Y EL DE SALIDA              *
      ******************************************************************
       011-ABRIR-ARCHIVOS.

           OPEN INPUT ENTRADA
           IF WS-ESTADO NOT = '00' AND NOT = '97'
              PERFORM 031-INI-ERR-PRO
              DISPLAY 'ERROR ABRIENDO ARCHIVO ENTRADA' WS-ESTADO
              PERFORM 032-FIN-ERR-PRO
           END-IF

           OPEN OUTPUT SALIDA
           IF WS-ESTADO NOT = '00' AND NOT = '97'
              CLOSE ENTRADA
              PERFORM 031-INI-ERR-PRO
              DISPLAY 'ERROR ABRIENDO ARCHIVO INFORM ' WS-ESTADO
              PERFORM 032-FIN-ERR-PRO
           END-IF.

       012-LEER-ARCHIVOS.
           INITIALIZE REG-ENTRADA1
           READ ENTRADA
              AT END
                 SET SI-FIN-DATA1    TO TRUE

              NOT AT END
                 ADD 1               TO WS-LEIDOS-DATA
           END-READ
           IF WS-ESTADO NOT = '00' AND NOT = '10'
              PERFORM 031-INI-ERR-PRO
              DISPLAY 'ERROR LEYENDO ARCHIVO DATAPER' WS-ESTADO
              PERFORM 032-FIN-ERR-PRO
           END-IF.
       013-GENERAR-CABECERA.
           MOVE WS-TITULO1 TO REG-SALIDA
           PERFORM 99-ESCRIBIR
           MOVE WS-TITULO2 TO REG-SALIDA
           PERFORM 99-ESCRIBIR.

      ******************************************************************
      *                        100000-PROCESO                          *
      ******************************************************************
       02-PROCESO.
           PERFORM 021-CARGUE-VARIABLES

           PERFORM 012-LEER-ARCHIVOS.

       021-CARGUE-VARIABLES.
           INITIALIZE CAI-PE9CCAI0
             MOVE REG-CLIENTE  TO AI0-CLIENTE
             MOVE REG-CONTRATO TO AI0-CONTRATO

             CALL  'PE3C0018' USING CAI-PE9CCAI0

             EVALUATE AI0-COD-RET
               WHEN '00'
                   PERFORM 66-GENERAR-CUERPO
               WHEN '35'
                   DISPLAY 'NO SE ENCUENTRA EL CLIENTE'
               WHEN OTHER
                   PERFORM 031-INI-ERR-PRO
                   DISPLAY 'ERROR DE RUTINA CAI-PE9CCAI0' AI0-COD-RET
                   DISPLAY 'DESCRIPCION 1' AI0-OFI2-ERROR1
                   PERFORM 032-FIN-ERR-PRO
             END-EVALUATE.

      ******************************************************************
      *                       200000-FINAL                             *
      ******************************************************************
       03-FINAL.
           CLOSE SALIDA ENTRADA
           PERFORM 032-FIN-ERR-PRO
           STOP RUN.
      ******************************************************************
      *                      200001-INI-ERR-PRO                        *
      ******************************************************************
       031-INI-ERR-PRO.
           DISPLAY '************* TERMINACION ANORMAL *************'
           DISPLAY 'LEIDOS ENTRADA1       : ' WS-LEIDOS-DATA
           DISPLAY 'REGISTROS ESCRITOS    : ' WS-ESCRITOS.

      ******************************************************************
      *                       200002-FIN-ERR-PRO                       *
      ******************************************************************
       032-FIN-ERR-PRO.
           MOVE 12 TO RETURN-CODE
           STOP RUN.
      ******************************************************************
      *                    200003-MSGFINPRO                            *
      ******************************************************************
       034-MSGFINPRO.
           DISPLAY 'LEIDOS ENTRADA1       : ' WS-LEIDOS-DATA
           DISPLAY 'REGISTROS ESCRITOS    : ' WS-ESCRITOS.
      ******************************************************************
      ******************************************************************
      *                                                                *
      *                    66-GENERAR-CUERPO
      ******************************************************************
       66-GENERAR-CUERPO.
             MOVE REG-CLIENTE       TO WS-CLIENTE
             MOVE AI0-FECHA         TO WS-FECHA
             MOVE REG-CONTRATO      TO WS-CONTRATO
             MOVE AI0-VALOR         TO WS-VALOR
             MOVE WS-CUERPO         TO REG-SALIDA
             PERFORM 99-ESCRIBIR.

      ******************************************************************
      *                        999999-ESCRIBIR                         *
      ******************************************************************
       99-ESCRIBIR.
           WRITE REG-SALIDA
           IF WS-ESTADO NOT = '00'
              PERFORM 031-INI-ERR-PRO
              DISPLAY 'ERROR WRITE ARCHIVO INFORME ' WS-ESTADO
              PERFORM 032-FIN-ERR-PRO
           ELSE
      *       INITIALIZE WS-INFORME
              ADD 1   TO WS-ESCRITOS
           END-IF.

