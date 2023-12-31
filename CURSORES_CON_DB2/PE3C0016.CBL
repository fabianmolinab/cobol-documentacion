       ID DIVISION.
      ******************************************************************

       PROGRAM-ID.      PE3C0016.

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
           05 SW-FIN-DATA            PIC X         VALUE 'N'.
             88 SI-FIN-DATA                        VALUE 'S'.
             88 NO-FIN-DATA                        VALUE 'N'.
           05 SW-FIN-CURSOR-1        PIC X         VALUE 'N'.
             88 SI-FIN-CURSOR-1                    VALUE 'S'.
             88 NO-FIN-CURSOR-1                    VALUE 'N'.
           05 SW-FIN-CURSOR-2        PIC X         VALUE 'N'.
             88 SI-FIN-CURSOR-2                    VALUE 'S'.
             88 NO-FIN-CURSOR-2                    VALUE 'N'.
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

       01 PRDTW03.
           05 W03-CLIENTE                       PIC X(8).
           05 W03-PRODUCTO                      PIC XX.
           05 W03-FECHA                         PIC X(10).

       01 WS-TABLAS.
           05 WS-TABLA-DATAPHON OCCURS 20 TIMES INDEXED BY I-IND.
               07 WS-TABLA-FECHA                    PIC X(10).

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE PETCOW02
           END-EXEC.

           EXEC SQL
               INCLUDE PETCOW03
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

      ******************************************************************
      ******              DECLARACION DEL CURSOR2               *******
           EXEC SQL
               DECLARE CURSOR_2 CURSOR FOR
               SELECT W03_FECHA
               FROM MAINTAIN.DTPR.PRDTW03
               WHERE W03_CLIENTE   =  :W02-CLIENTE
               AND   W03_PRODUCTO  =  :W03-PRODUCTO
           END-EXEC.
       LINKAGE SECTION.
       COPY PE9CCAI0.

      ******************************************************************
      *                       PROCEDURE DIVISION                       *
      ******************************************************************
       PROCEDURE DIVISION USING CAI-PE9CCAI0.
           PERFORM 000000-INICIO
           PERFORM 100000-PROCESO  UNTIL SI-FIN-CURSOR-1
           PERFORM 200000-FINAL.
      ******************************************************************
      *                     000000-INICIO                              *
      *   - ABRIR ARCHIVOS DE ENTRADA Y SALIDA                         *
      *   - PRIMERA LECTURA DEL ARCHIVO DE ENTRADA                     *
      ******************************************************************
       000000-INICIO.
           MOVE  '00'           TO AI0-COD-RET
           SET NO-FIN-DATA      TO TRUE
           SET NO-FIN-CURSOR-1  TO TRUE
           SET NO-FIN-CURSOR-2  TO TRUE
           INITIALIZE WS-TABLAS
           INITIALIZE WS-CONTADOR
           PERFORM 000001-VALIDACION-DE-VARIABLES
           PERFORM 000002-ABRIR-CURSOR-1
           PERFORM 000003-ABRIR-CURSOR-2
           PERFORM 000004-FETCH-CURSOR-2
           PERFORM 000005-CARGAR-TABLA VARYING I-IND FROM 1 BY 1
                   UNTIL I-IND > 20 OR SI-FIN-CURSOR-2
           PERFORM  100005-CERRAR-CURSOR-2.

       000001-VALIDACION-DE-VARIABLES.
           IF AI0-CLIENTE IS NOT NUMERIC
             MOVE '10'          TO     AI0-COD-RET
             MOVE 'NO VALIDO'   TO     AI0-OFI2-ERROR2
             PERFORM 200000-FINAL
           END-IF

           IF AI0-CONTRATO IS NOT NUMERIC
             MOVE '10'          TO     AI0-COD-RET
             MOVE 'NO VALIDO'   TO     AI0-OFI2-ERROR2
             PERFORM 200000-FINAL
           END-IF

           IF AI0-PRODUCTO IS NOT NUMERIC
             MOVE '10'          TO     AI0-COD-RET
             MOVE 'NO VALIDO'   TO     AI0-OFI2-ERROR2
             PERFORM 200000-FINAL
           END-IF.

       000002-ABRIR-CURSOR-1.
           MOVE AI0-CLIENTE            TO W02-CLIENTE
           MOVE AI0-CONTRATO           TO W02-CONTRATO

           EXEC SQL
               OPEN CURSOR_1
           END-EXEC

      *    VALIDACION DE ERRORES
           IF SQLCODE NOT EQUAL ZEROS
               MOVE '10'                   TO AI0-COD-RET
               MOVE 'ERROR EN EL CURSOR 1' TO AI0-OFI2-ERROR2
               PERFORM 200000-FINAL
           END-IF.

       000003-ABRIR-CURSOR-2.
           MOVE AI0-PRODUCTO           TO W03-PRODUCTO

           EXEC SQL
               OPEN CURSOR_2
           END-EXEC

      *    VALIDACION DE ERRORES
           IF SQLCODE NOT EQUAL ZEROS
               MOVE '10'                   TO AI0-COD-RET
               MOVE 'ERROR EN EL CURSOR 2' TO AI0-OFI2-ERROR2
               PERFORM 200000-FINAL
           END-IF.

       000004-FETCH-CURSOR-2.
           EXEC SQL
               FETCH CURSOR_2
                   INTO :W03-FECHA
           END-EXEC.
           EVALUATE SQLCODE
              WHEN   ZEROS
                   CONTINUE
               WHEN +100
                   SET SI-FIN-CURSOR-2            TO TRUE
               WHEN OTHER
                   MOVE '20'                      TO AI0-COD-RET
                   MOVE 'ERROR EN EL CURSOR 2'    TO AI0-OFI2-ERROR1
                   MOVE SQLCODE                   TO AI0-MAS-SQLCODE
           END-EVALUATE.

       000005-CARGAR-TABLA.
           MOVE  W03-FECHA        TO WS-TABLA-FECHA(I-IND)
           PERFORM 000004-FETCH-CURSOR-2.
      ******************************************************************
      *                        100000-PROCESO                          *
      *   PROCESO PARA GENERAR INFORME QUE TOMA COMO ENTRADA LOS DATOS *
      *   DE BASEDATA, LO ORDENA POR OFICINA                           *
      ******************************************************************
       100000-PROCESO.
           PERFORM 100001-FETCH-CURSOR-1.
       100001-FETCH-CURSOR-1.
           EXEC SQL
               FETCH CURSOR_1
               INTO :W02-CLIENTE,
                    :W02-FECHA-MOV,
                    :W02-CONTRATO,
                    :W02-VALOR
           END-EXEC
           EVALUATE SQLCODE
              WHEN ZEROS
                   PERFORM 100002-BUSCAR-FECHA
                   ADD 1                          TO WS-CONTADOR
               WHEN +100
                    SET SI-FIN-CURSOR-1 TO TRUE
                    IF WS-CONTADOR = ZEROS
                       MOVE '35'                  TO AI0-COD-RET
                       MOVE 'NO ENCONTRE CURSOR1' TO AI0-OFI2-ERROR1
                   END-IF
               WHEN OTHER
                   MOVE '30'                      TO AI0-COD-RET
                   MOVE 'ERROR CONEXION BD'       TO AI0-OFI2-ERROR1
                   MOVE SQLCODE                   TO AI0-MAS-SQLCODE
           END-EVALUATE.

       100002-BUSCAR-FECHA.
            SET I-IND TO 1

           SEARCH WS-TABLA-DATAPHON
               AT END
                    CONTINUE
               WHEN WS-TABLA-FECHA(I-IND)  =     W02-FECHA-MOV
                    SET SI-FIN-DATA           TO    TRUE
                    SET SI-FIN-CURSOR-1       TO    TRUE
           END-SEARCH.

      ******************************************************************
      *                       200000-FINAL                             *
      ******************************************************************
       200000-FINAL.
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

           PERFORM 100004-CERRAR-CURSOR-1

           GOBACK.
       100004-CERRAR-CURSOR-1.
           EXEC SQL
               CLOSE CURSOR_1
           END-EXEC

           IF SQLCODE NOT EQUAL ZEROS
               MOVE '100'                        TO AI0-COD-RET
               MOVE 'ERR CERRANDO EL CURSOR1'    TO AI0-OFI2-ERROR2
      *        PERFORM 200000-FINAL
           END-IF.

       100005-CERRAR-CURSOR-2.
           EXEC SQL
               CLOSE CURSOR_2
           END-EXEC

           IF SQLCODE NOT EQUAL ZEROS
               MOVE '100'                        TO AI0-COD-RET
               MOVE 'ERR CERRANDO EL CURSOR2'    TO AI0-OFI2-ERROR2
               PERFORM 200000-FINAL
           END-IF.