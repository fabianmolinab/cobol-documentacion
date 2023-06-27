       ID DIVISION.
      ******************************************************************

       PROGRAM-ID.      PE3C0018.

      ******************************************************************
      *                                                                *
      *   FECHA ...........:  JUNIO 2023                               *
      *   AUTOR ...........:  FABIAN ANDRES MOLINA BERMUDEZ
      *                       INETUM(COLOMBIA)                         *
      *   LENGUAJE ........:  COBOL                                    *
      *                                                                *
      *                         DESCRIPCION:
      *           RUTINA QUE BUSCA Y FILTRA EN LA BASE DE DATOS
      *                         ============                           *
      ******************************************************************

       AUTHOR.          FABIAN ANDRES MOLINA BERMUDEZ.
       DATE-WRITTEN.    23/JUN/2023.
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
      ******************************************************************
      *                         DATA DIVISION                          *
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
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
           05 SW-FIN-DATA            PIC X       VALUE 'N'.
             88 SI-FIN-DATA                      VALUE 'S'.
             88 NO-FIN-DATA                      VALUE 'N'.
           05 SW-FIN-CURSOR          PIC X       VALUE 'N'.
             88 SI-FIN-CURSOR                    VALUE 'S'.
             88 NO-FIN-CURSOR                    VALUE 'N'.
      ******************************************************************
      *                          CONTADORES                            *
      ******************************************************************
       01 CONTADORES.
          05 WS-LEIDOS-OFICINA           PIC 9(09) VALUE ZEROS.
          05 WS-CONTADOR                 PIC 9(09) VALUE ZEROS.
      ******************************************************************
      *                       VARIABLES DE TRABAJO                     *
      ******************************************************************

       01 PRDTW02.
           05 W02-CLIENTE                       PIC X(8).
           05 W02-FECHA-MOV                     PIC X(10).
           05 W02-CONTRATO                      PIC X(18).
           05 W02-VALOR                         PIC X(13).

       01 WS-TABLAS.
           05 WS-TABLA-DATA OCCURS 50 TIMES INDEXED BY I-IND.
               07 WS-TABLA-CLIENTE              PIC X(10).
               07 WS-TABLA-FECHA                PIC X(10).
               07 WS-TABLA-CONTRATO             PIC X(18).
               07 WS-TABLA-VALOR                PIC X(13).

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE PETCOW02
           END-EXEC.

      ******************************************************************
      ******              DECLARACION DEL CURSOR1              *********
           EXEC SQL
               DECLARE CURSOR_1 CURSOR FOR
               SELECT *
               FROM MAINTAIN.DTPR.PRDTW02
               WHERE W02_CLIENTE   =  :W02-CLIENTE
               AND   W02_CONTRATO  =  :W02-CONTRATO
           END-EXEC.

       LINKAGE SECTION.
       COPY PE9CCAI0.
      ******************************************************************
      *                       PROCEDURE DIVISION                       *
      ******************************************************************
       PROCEDURE DIVISION USING CAI-PE9CCAI0.
           PERFORM 01-INICIO
           PERFORM 02-PROCESO  UNTIL SI-FIN-DATA
           PERFORM 03-FINAL.
      ******************************************************************
      *                           01-INICIO                            *
      *   - ABRIR ARCHIVOS DE ENTRADA Y SALIDA                         *
      *   - PRIMERA LECTURA DEL ARCHIVO DE ENTRADA                     *
      ******************************************************************
       01-INICIO.
           PERFORM 011-INICIALIZACION-DE-VARIABLES
           PERFORM 012-VALIDACION-DE-VARIABLES
           PERFORM 013-ABRIR-CURSOR
           PERFORM 014-CARGAR-TABLA VARYING I-IND FROM 1 BY 1
                   UNTIL I-IND > 20
                   OR SI-FIN-CURSOR.

       011-INICIALIZACION-DE-VARIABLES.
           MOVE  '00'           TO     AI0-COD-RET
           SET NO-FIN-DATA      TO     TRUE
           SET NO-FIN-CURSOR    TO     TRUE
           INITIALIZE WS-TABLAS
           INITIALIZE WS-CONTADOR.

       012-VALIDACION-DE-VARIABLES.
           IF AI0-CLIENTE IS NOT NUMERIC
             MOVE '10'          TO     AI0-COD-RET
             MOVE 'NO VALIDO'   TO     AI0-OFI2-ERROR2
             PERFORM 03-FINAL
           END-IF

           IF AI0-CONTRATO IS NOT NUMERIC
             MOVE '10'          TO     AI0-COD-RET
             MOVE 'NO VALIDO'   TO     AI0-OFI2-ERROR2
             PERFORM 03-FINAL
           END-IF.

       013-ABRIR-CURSOR.
           MOVE AI0-CLIENTE     TO     W02-CLIENTE
           MOVE AI0-CONTRATO    TO     W02-CONTRATO

           EXEC SQL
               OPEN CURSOR_1
           END-EXEC

           IF SQLCODE NOT EQUAL ZEROS
               MOVE '10'                   TO AI0-COD-RET
               MOVE 'ERROR EN EL CURSOR 1' TO AI0-OFI2-ERROR2
               PERFORM 03-FINAL
           END-IF.


       014-CARGAR-TABLA.
           MOVE W02-CLIENTE      TO  WS-TABLA-CLIENTE(I-IND)
           MOVE W02-FECHA-MOV    TO  WS-TABLA-FECHA(I-IND)
           MOVE W02-CONTRATO     TO  WS-TABLA-CONTRATO(I-IND)
           MOVE W02-VALOR        TO  WS-TABLA-VALOR(I-IND)

           PERFORM 021-FETCH-CURSOR.

      ******************************************************************
      *                        PROCESO                                 *
      *   PROCESO PARA GENERAR INFORME QUE TOMA COMO ENTRADA LOS DATOS *
      *   DE BASEDATA, LO ORDENA POR OFICINA                           *
      ******************************************************************
       02-PROCESO.
           PERFORM 021-FETCH-CURSOR.
       021-FETCH-CURSOR.
           EXEC SQL
               FETCH CURSOR_1
               INTO :W02-CLIENTE,
                    :W02-FECHA-MOV,
                    :W02-CONTRATO,
                    :W02-VALOR
           END-EXEC
           EVALUATE SQLCODE
              WHEN ZEROS
                   PERFORM 02101-BUSCAR-FECHA
                   ADD 1                          TO WS-CONTADOR
               WHEN +100
                    SET SI-FIN-CURSOR TO TRUE
                    IF WS-CONTADOR = ZEROS
                       MOVE '35'                  TO AI0-COD-RET
                       MOVE 'NO ENCONTRE CURSOR1' TO AI0-OFI2-ERROR1
                   END-IF
               WHEN OTHER
                   MOVE '30'                      TO AI0-COD-RET
                   MOVE 'ERROR CONEXION BD'       TO AI0-OFI2-ERROR1
                   MOVE SQLCODE                   TO AI0-MAS-SQLCODE
           END-EVALUATE.

       02101-BUSCAR-FECHA.
            SET I-IND TO 1

           SEARCH WS-TABLA-DATA
               AT END
                    CONTINUE
               WHEN WS-TABLA-CLIENTE(I-IND)  =     W02-CLIENTE
                    SET SI-FIN-DATA           TO    TRUE
                    SET SI-FIN-CURSOR         TO    TRUE
           END-SEARCH.

      ******************************************************************
      *                       200000-FINAL                             *
      ******************************************************************
       03-FINAL.
           IF SI-FIN-DATA
               MOVE W02-CLIENTE        TO AI0-CLIENTE-SAL
               MOVE W02-FECHA-MOV      TO AI0-FECHA
               MOVE W02-CONTRATO       TO AI0-CONTRATO-SAL
               MOVE W02-VALOR          TO AI0-VALOR
            ELSE
               MOVE '35'               TO AI0-COD-RET
               MOVE 'NO ENCONTRE REG ' TO AI0-OFI2-ERROR1
               MOVE W02-FECHA-MOV      TO AI0-OFI2-ERROR2
           END-IF

           PERFORM 021-CERRAR-CURSOR

           GOBACK.
       021-CERRAR-CURSOR.
           EXEC SQL
               CLOSE CURSOR_1
           END-EXEC

           IF SQLCODE NOT EQUAL ZEROS
               MOVE '100'                        TO AI0-COD-RET
               MOVE 'ERR CERRANDO EL CURSOR1'    TO AI0-OFI2-ERROR2
           END-IF.