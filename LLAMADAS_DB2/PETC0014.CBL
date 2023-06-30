       ID DIVISION.
      ******************************************************************

       PROGRAM-ID.      PE3C0014.

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
       DATE-WRITTEN.    22/JUN/2023.
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

       01 PRDTW02.
           05 W02-CLIENTE                       PIC X(8).
           05 W02-FECHA-MOV                     PIC X(10).
           05 W02-CONTRATO                      PIC X(18).
           05 W02-VALOR                         PIC X(13).
       01 PRDTW03.
           05 W03-CLIENTE                       PIC X(8).
           05 W03-PRODUCTO                      PIC XX.
           05 W03-FECHA                         PIC X(10).


           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE PETCOW02
           END-EXEC.

           EXEC SQL
               INCLUDE PETCOW03
           END-EXEC.

       LINKAGE SECTION.
       COPY PE9CCAI0.

      ******************************************************************
      *                       PROCEDURE DIVISION                       *
      ******************************************************************
       PROCEDURE DIVISION USING CAI-PE9CCAI0.
           PERFORM 000000-INICIO
           PERFORM 100000-PROCESO 
           PERFORM 200000-FINAL.
      ******************************************************************
      *                     000000-INICIO                              *
      *   - ABRIR ARCHIVOS DE ENTRADA Y SALIDA                         *
      *   - PRIMERA LECTURA DEL ARCHIVO DE ENTRADA                     *
      ******************************************************************
       000000-INICIO.
           MOVE  '00' TO AI0-COD-RET
           PERFORM 000001-VALIDACION.

       000001-VALIDACION.
           IF AI0-CLIENTE IS NOT NUMERIC
             MOVE '10'          TO AI0-COD-RET
             MOVE 'NO VALIDO'   TO AI0-OFI2-ERROR2
             PERFORM 200000-FINAL
           END-IF.

           IF AI0-CONTRATO IS NOT NUMERIC
             MOVE '10'          TO AI0-COD-RET
             MOVE 'NO VALIDO'   TO AI0-OFI2-ERROR2
             PERFORM 200000-FINAL
           END-IF.

           IF AI0-PRODUCTO IS NOT NUMERIC
             MOVE '10'          TO AI0-COD-RET
             MOVE 'NO VALIDO'   TO AI0-OFI2-ERROR2
             PERFORM 200000-FINAL
           END-IF.

      ******************************************************************
      *                        100000-PROCESO                          *
      *   PROCESO PARA GENERAR INFORME QUE TOMA COMO ENTRADA LOS DATOS *
      *   DE BASEDATA, LO ORDENA POR OFICINA                           *
      ******************************************************************
       100000-PROCESO.
           MOVE AI0-CLIENTE  TO W02-CLIENTE
           MOVE AI0-CONTRATO TO W02-CONTRATO
           MOVE AI0-PRODUCTO TO W03-PRODUCTO
           PERFORM 100001-BUSCAR-BASE-DE-DATOS.

       100001-BUSCAR-BASE-DE-DATOS.
           EXEC SQL
               SELECT *
               INTO :W02-CLIENTE, :W02-FECHA-MOV,  :W02-CONTRATO,
                     :W02-VALOR
               FROM MAINTAIN.DTPR.PRDTW02
               WHERE W02_CLIENTE =  :W02-CLIENTE
               AND   W02_CONTRATO = :W02-CONTRATO
               AND   W02_FECHA_MOV =(SELECT W03_FECHA
                                     FROM MAINTAIN.DTPR.PRDTW03
                                     WHERE W03_CLIENTE = :W02-CLIENTE
                                     AND  W03_PRODUCTO = :W03-PRODUCTO)
           END-EXEC

      *AQUI EVALUAMOS LOS ERRORES TRAIDOS DE LA BASE DE DATOS
           EVALUATE SQLCODE
               WHEN   ZEROS
      *            MOVE W02-CONTRATO  TO AI0-CONTRATO-SAL
      *            MOVE W02-CLIENTE   TO AI0-CLIENTE-SAL
                   MOVE W02-FECHA-MOV TO AI0-FECHA
                   MOVE W02-VALOR     TO AI0-VALOR
               WHEN +100
                   MOVE '20'                   TO AI0-COD-RET
                   MOVE 'NO EXISTE LA OFICINA' TO AI0-OFI2-ERROR1
               WHEN OTHER
                   MOVE '15'                   TO AI0-COD-RET
                   MOVE 'ERROR CONEXION BD'    TO AI0-OFI2-ERROR1
                   MOVE SQLCODE                TO AI0-MAS-SQLCODE
           END-EVALUATE.

      ******************************************************************
      *                       200000-FINAL                             *
      ******************************************************************
       200000-FINAL.
           GOBACK.
