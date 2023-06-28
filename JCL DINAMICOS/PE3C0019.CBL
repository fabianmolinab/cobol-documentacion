       ID DIVISION.
      ******************************************************************

       PROGRAM-ID.      PE3C0019.

      ******************************************************************
      *                                                                *
      *   FECHA ...........:  JUNIO 2023                               *
      *   AUTOR ...........:  FABIAN ANDRES MOLINA BERMUDEZ
      *                       INETUM(COLOMBIA)                         *
      *   LENGUAJE ........:  COBOL                                    *
      *                                                                *
      *                         DESCRIPCION:
      *           EJERCICIO DE JCL DINAMICO
      *                         ============                           *
      ******************************************************************

       AUTHOR.          FABIAN ANDRES MOLINA BERMUDEZ.
       DATE-WRITTEN.    27/JUN/2023.
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

      *****************ARCHIVO DE SALIDA********************************
           SELECT SALIDA    ASSIGN TO SALIDA
                            FILE STATUS IS WS-ESTADO.
      ******************************************************************
      *                         DATA DIVISION                          *
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  SALIDA
                     RECORDING MODE IS F
                     BLOCK CONTAINS 0 RECORDS.
       01 REG-SALIDA             PIC X(80).
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
          05 WS-LEIDOS                   PIC 9(09) VALUE ZEROS.
          05 WS-CONTADOR                 PIC 9(09) VALUE ZEROS.
          05 WS-ESCRITOS                 PIC 9(09) VALUE ZEROS.

      ******************************************************************
      *                       VARIABLES DE TRABAJO                     *
      ******************************************************************
       01 WS-VARIABLES.
          05 WS-LINEA.
             10 LINEA-01-20              PIC X(20).
             10 LINEA-21-40              PIC X(20).
             10 LINEA-41-60              PIC X(20).
             10 LINEA-61-72              PIC X(12).
             10 LINEA-73-80              PIC X(08).

          05 WS-ASTERISCOS               PIC X(72) VALUE ALL '*'.

       01 PRDTW03.
           05 W03-CLIENTE                       PIC X(8).
           05 W03-PRODUCTO                      PIC XX.
           05 W03-FECHA                         PIC X(10).

      ******************************************************************
      *                           CONSTANTES                           *
      ******************************************************************
       01 WS-CONSTANTES.
          05 CTE-COMILLA                 PIC X(01) VALUE X'7D'.
          05 CTE-SORT-FIELDS             PIC X(11) VALUE 'SORT FIELDS'.

      *-------------------------  TABLAS -------------------------------
       01 WS-TABLAS.
           05 WS-TABLA-DATA OCCURS 50 TIMES INDEXED BY I-IND.
               07 WS-TABLA-PRODUCTO                PIC XX.

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE PETCOW03
           END-EXEC.


      ******************************************************************
      ******              DECLARACION DEL CURSOR              **********
           EXEC SQL
               DECLARE CURSOR_1 CURSOR FOR
               SELECT DISTINCT(W03_PRODUCTO)
               FROM MAINTAIN.DTPR.PRDTW03
           END-EXEC.
      *-----------------------------------------------------------------
       01 WS-ERRORES.
           05 WS-COD-RET                        PIC 99.
           05 WS-SQLCODE                        PIC ZZZZ.
           05 WS-ERROR-1                        PIC X(20).
           05 WS-ERROR-2                        PIC X(20).

       01 REG-PARM.
           05 PARM-LONG                         PIC X(02).
           05 PARM-AMBI                         PIC X(01) VALUE 'P'.
           05 PARM-ARCH                         PIC X(34)
           VALUE 'COBP.PE.FWD1.PPED003.SORT.PERSONAS'.

       LINKAGE SECTION.
      ******************************************************************
      *                       PROCEDURE DIVISION                       *
      ******************************************************************
       PROCEDURE DIVISION.
           PERFORM 01-INICIO
           PERFORM 02-PROCESO
           PERFORM 03-FINAL.
      ******************************************************************
      *                           01-INICIO                            *
      *   - ABRIR ARCHIVOS DE ENTRADA Y SALIDA                         *
      *   - PRIMERA LECTURA DEL ARCHIVO DE ENTRADA                     *
      ******************************************************************
       01-INICIO.

           MOVE '00'            TO     WS-COD-RET
           PERFORM 011-INICIALIZACION-DE-VARIABLES
           PERFORM 012-ABRIR-ARCHIVOS
           PERFORM 012-ABRIR-CURSOR
           PERFORM 021-FETCH-CURSOR
           PERFORM 014-CARGAR-TABLA VARYING I-IND FROM 1 BY 1
                   UNTIL I-IND > 20
                   OR SI-FIN-CURSOR
           PERFORM 031-CERRAR-CURSOR.
      ******************************************************************
      *                     000010-ABIR-ARCHIVO                        *
      *   - SE ABRIR EL ARCHIVO DE ENTRADA Y EL DE SALIDA              *
      ******************************************************************

       011-INICIALIZACION-DE-VARIABLES.
           SET NO-FIN-CURSOR    TO     TRUE
           INITIALIZE WS-TABLAS
           INITIALIZE WS-CONTADOR
           INITIALIZE PRDTW03.

       012-ABRIR-ARCHIVOS.
           OPEN OUTPUT SALIDA
           IF WS-ESTADO NOT = '00' AND NOT = '97'
              PERFORM 032-INI-ERR-PRO
              DISPLAY 'ERROR ABRIENDO ARCHIVO INFORM ' WS-ESTADO
              PERFORM 033-FIN-ERR-PRO
           END-IF.

       012-ABRIR-CURSOR.

           EXEC SQL
               OPEN CURSOR_1
           END-EXEC

           IF SQLCODE NOT EQUAL ZEROS
               MOVE '10'                   TO WS-COD-RET
               MOVE 'ERROR EN EL CURSOR1'   TO WS-ERROR-1
               PERFORM 03-FINAL
           END-IF.

       014-CARGAR-TABLA.
           MOVE W03-PRODUCTO      TO  WS-TABLA-PRODUCTO(I-IND)
           PERFORM 021-FETCH-CURSOR.
      ******************************************************************
      *                        PROCESO                                 *
      *   PROCESO PARA GENERAR INFORME QUE TOMA COMO ENTRADA LOS DATOS *
      *   DE BASEDATA, LO ORDENA POR OFICINA                           *
      ******************************************************************
       02-PROCESO.
           PERFORM 022-GENERAR-CABECERA
           PERFORM 023-GENERAR-BORRADO
           PERFORM 025-GENERAR-CUERPO.

       021-FETCH-CURSOR.
           EXEC SQL
               FETCH CURSOR_1
               INTO :W03-PRODUCTO
           END-EXEC

           EVALUATE SQLCODE
              WHEN ZEROS
                   CONTINUE
               WHEN +100
                    SET SI-FIN-CURSOR TO TRUE
               WHEN OTHER
                   MOVE '30'                      TO WS-COD-RET
                   MOVE 'ERROR CONEXION BD'       TO WS-ERROR-1
                   MOVE SQLCODE                   TO WS-SQLCODE
           END-EVALUATE.

       022-GENERAR-CABECERA.
           INITIALIZE WS-LINEA

           MOVE '//'                   TO LINEA-01-20(1:2)
           MOVE PARM-AMBI              TO LINEA-01-20(3:1)
           MOVE 'PPED0012'             TO LINEA-01-20(4:7)
           MOVE ' JOB CLASS'           TO LINEA-01-20(11:10)
           MOVE '=B,REGION=0M,MSGCLAS' TO LINEA-21-40
           MOVE 'S=H,MSGLEVEL=(1,1)'   TO LINEA-41-60
           PERFORM 99-ESCRIBIR

           MOVE '//'                   TO LINEA-01-20(1:2)
           MOVE 'JOBLIB   DD DSN=CO'   TO LINEA-01-20(3:18)
           MOVE 'B'                    TO LINEA-21-40(1:1)
           MOVE PARM-AMBI              TO LINEA-21-40(2:1)
           MOVE '.ALTAMIRA.LOADLIB.'   TO LINEA-21-40(3:18)
           MOVE 'BATCH,DISP=SHR'       TO LINEA-41-60
           PERFORM 99-ESCRIBIR

           MOVE WS-ASTERISCOS          TO WS-LINEA
           MOVE '//'                   TO LINEA-01-20(1:2)
           PERFORM 99-ESCRIBIR

           MOVE '//* SISTEMA     : TP' TO LINEA-01-20
           MOVE '(FONDOS)'             TO LINEA-21-40
           MOVE '*'                    TO LINEA-61-72(12:1)
           PERFORM 99-ESCRIBIR

           MOVE '//* PROCESO     : TP' TO LINEA-01-20
           MOVE 'A121A.'               TO LINEA-21-40
           MOVE '*'                    TO LINEA-61-72(12:1)
           PERFORM 99-ESCRIBIR

           MOVE '//* EJECUCION   : AN' TO LINEA-01-20
           MOVE 'UAL.'                 TO LINEA-21-40
           MOVE '*'                    TO LINEA-61-72(12:1)
           PERFORM 99-ESCRIBIR

           MOVE '//* OBJETIVO    : SE' TO LINEA-01-20
           MOVE 'PARAR ARCHIVO DESCAR' TO LINEA-21-40
           MOVE 'GA TABLA TPDTEXT POR' TO LINEA-41-60
           MOVE ' FONDO Y   '          TO LINEA-61-72(1:11)
           MOVE '*'                    TO LINEA-61-72(12:1)
           PERFORM 99-ESCRIBIR

           MOVE '//*               OR' TO LINEA-01-20
           MOVE 'DENAR POR FONDO, CON' TO LINEA-21-40
           MOVE 'TRATO Y FECHA DE OPE' TO LINEA-41-60
           MOVE 'RACION     '          TO LINEA-61-72
           MOVE '*'                    TO LINEA-61-72(12:1)
           PERFORM 99-ESCRIBIR

           MOVE '//* REALIZO     : FA' TO LINEA-01-20
           MOVE 'BIAN MOLINA         ' TO LINEA-21-40
           MOVE '(INETUM,COLOMBIA)   ' TO LINEA-41-60
           MOVE '*'                    TO LINEA-61-72(12:1)
           PERFORM 99-ESCRIBIR

           MOVE '//* FECHA       : JU' TO LINEA-01-20
           MOVE 'NIO DE 2023.'         TO LINEA-21-40
           MOVE '*'                    TO LINEA-61-72(12:1)
           PERFORM 99-ESCRIBIR

           MOVE WS-ASTERISCOS          TO WS-LINEA
           MOVE '//'                   TO LINEA-01-20(1:2)
           PERFORM 99-ESCRIBIR.

      ******************************************************************
      *                                                                *
      *                      100200-GENERAR-BORRADO                    *
      *                                                                *
      *   SE CREA EL PRIMER PASO DEL JCL, EL CUAL ES EL BORRADO DE LOS *
      *   FICHEROS                                                     *
      ******************************************************************
       023-GENERAR-BORRADO.
           MOVE WS-ASTERISCOS          TO WS-LINEA
           MOVE '//'                   TO LINEA-01-20(1:2)
           PERFORM 99-ESCRIBIR

           MOVE '//* PASO 06: BORRADO' TO LINEA-01-20
           MOVE ' DE ARCHIVOS DE TRAB' TO LINEA-21-40
           MOVE 'AJO'                  TO LINEA-41-60
           MOVE '*'                    TO LINEA-61-72(12:1)
           PERFORM 99-ESCRIBIR

           MOVE WS-ASTERISCOS          TO WS-LINEA
           MOVE '//'                   TO LINEA-01-20(1:2)
           PERFORM 99-ESCRIBIR

           MOVE '//TP121A06 EXEC  PGM' TO LINEA-01-20
           MOVE '=IDCAMS'              TO LINEA-21-40
           PERFORM 99-ESCRIBIR

           MOVE '//SYSPRINT DD  SYSOU' TO LINEA-01-20
           MOVE 'T=*'                  TO LINEA-21-40
           PERFORM 99-ESCRIBIR

           MOVE '//SYSOUT   DD  SYSOU' TO LINEA-01-20
           MOVE 'T=*'                  TO LINEA-21-40
           PERFORM 99-ESCRIBIR

           MOVE '//SYSIN    DD  *'     TO LINEA-01-20
           PERFORM 99-ESCRIBIR

           PERFORM 024-TARJETA-DELETE
                    VARYING I-IND FROM 1 BY 1
                    UNTIL   I-IND > 100
                    OR WS-TABLA-PRODUCTO(I-IND) = SPACES OR LOW-VALUES

           MOVE '  SET MAXCC=0'        TO LINEA-01-20
           PERFORM 99-ESCRIBIR

           MOVE '  SET LASTCC=0'       TO LINEA-01-20
           PERFORM 99-ESCRIBIR.

      ******************************************************************
      *                                                                *
      *                       100210-TARJETA-DELETE                    *
      *                                                                *
      *   SE CREA EL DELETE DE LOS FICHEROS DEL JCL DINAMICO           *
      ******************************************************************
       024-TARJETA-DELETE.
           MOVE '  DELETE COB'           TO LINEA-01-20(1:12)
           MOVE PARM-AMBI                TO LINEA-01-20(13:1)
           MOVE '.TP.FJS'                TO LINEA-01-20(14:7)
           MOVE 'M1.TPA1214.TP120119.'   TO LINEA-21-40
           MOVE 'EXT'                    TO LINEA-41-60(1:3)
           MOVE WS-TABLA-PRODUCTO(I-IND) TO LINEA-41-60(4:2)
           MOVE '.SORT'                  TO LINEA-41-60(6:5)
           PERFORM 99-ESCRIBIR.

      ******************************************************************
      *                                                                *
      *                      100300-GENERAR-CUERPO                     *
      *                                                                *
      *   SE CREA EL CUERPO DEL JCL EL CUAL CONTIENE LA DESCRIPCION DEL*
      *   PASO Y LA CREACION DE LOS FICHEROS DE SALIDA A PARTIR DEL    *
      *   ARCHIVO DESCARGA DE LA TABLA TPDTEXT                         *
      ******************************************************************
       025-GENERAR-CUERPO.
           MOVE WS-ASTERISCOS          TO WS-LINEA
           MOVE '//'                   TO LINEA-01-20(1:2)
           PERFORM 99-ESCRIBIR

           MOVE '//* PASO 03: ORDENA ' TO LINEA-01-20
           MOVE 'POR FONDO, CONTRATO,' TO LINEA-21-40
           MOVE ' FECHA DE OPERACION ' TO LINEA-41-60
           MOVE 'Y SEPARA   '          TO LINEA-61-72(1:11)
           MOVE '*'                    TO LINEA-61-72(12:1)
           PERFORM 99-ESCRIBIR

           MOVE '//*          POR FON' TO LINEA-01-20
           MOVE 'DO'                   TO LINEA-21-40
           PERFORM 99-ESCRIBIR

           MOVE WS-ASTERISCOS          TO WS-LINEA
           MOVE '//'                   TO LINEA-01-20(1:2)
           PERFORM 99-ESCRIBIR

           MOVE '//TP121A03 EXEC  PGM' TO LINEA-01-20
           MOVE '=SORT,COND=(4,LT)'    TO LINEA-21-40
           PERFORM 99-ESCRIBIR

           MOVE '//SYSPRINT DD  SYSOU' TO LINEA-01-20
           MOVE 'T=*'                  TO LINEA-21-40
           PERFORM 99-ESCRIBIR

           MOVE '//SYSTSPRT DD  SYSOU' TO LINEA-01-20
           MOVE 'T=*'                  TO LINEA-21-40
           PERFORM 99-ESCRIBIR

           MOVE '//SYSOUT   DD  SYSOU' TO LINEA-01-20
           MOVE 'T=*'                  TO LINEA-21-40
           PERFORM 99-ESCRIBIR

           MOVE '//SORTIN   DD  DSN='  TO LINEA-01-20(1:19)
           MOVE PARM-ARCH(1:1)         TO LINEA-01-20(20:1)
           MOVE PARM-ARCH(2:20)        TO LINEA-21-40
           MOVE PARM-ARCH(22:13)       TO LINEA-41-60(1:13)
           MOVE ','                    TO LINEA-41-60(14:1)
           PERFORM 99-ESCRIBIR

           MOVE '//             DISP=' TO LINEA-01-20
           MOVE 'SHR'                  TO LINEA-21-40
           PERFORM 99-ESCRIBIR

           PERFORM 026-ARCHIVOS-SALIDA
                   VARYING I-IND FROM 1 BY 1
                   UNTIL   I-IND > 100
                   OR WS-TABLA-PRODUCTO(I-IND) = SPACES OR LOW-VALUES

           MOVE '//SYSIN    DD  *'     TO LINEA-01-20
           PERFORM 99-ESCRIBIR

           MOVE SPACES                 TO LINEA-01-20(1:2)
           MOVE CTE-SORT-FIELDS        TO LINEA-01-20(3:11)
           MOVE '=COPY'                TO LINEA-01-20(14:5)
           PERFORM 99-ESCRIBIR

           PERFORM 027-SEPARACION-ARCHIVOS
                   VARYING I-IND FROM 1 BY 1
                   UNTIL   I-IND > 100
                   OR WS-TABLA-PRODUCTO(I-IND) = SPACES OR LOW-VALUES.

      ******************************************************************
      *                                                                *
      *                     100310-ARCHIVOS-SALIDA                     *
      *                                                                *
      *   SE DEFINEN LOS NOMBRES DE LOS ARCHIVOS Y SUS ATRIBUTOS       *
      ******************************************************************
       026-ARCHIVOS-SALIDA.
           MOVE '//C'                     TO LINEA-01-20(1:3)
           MOVE WS-TABLA-PRODUCTO(I-IND)  TO LINEA-01-20(4:4)
           MOVE '    DD  DSN=C'           TO LINEA-01-20(8:13)
           MOVE 'OB'                      TO LINEA-21-40(1:2)
           MOVE PARM-AMBI                 TO LINEA-21-40(3:1)
           MOVE '.TP.FJSM1.TPA1214'       TO LINEA-21-40(4:17)
           MOVE '.TP120119.EXT'           TO LINEA-41-60(1:13)
           MOVE WS-TABLA-PRODUCTO(I-IND)  TO LINEA-41-60(14:2)
           MOVE '.SORT'                   TO LINEA-41-60(16:5)
           MOVE ','                       TO LINEA-61-72(1:1)
           PERFORM 99-ESCRIBIR

           MOVE '//             DISP=' TO LINEA-01-20
           MOVE '(NEW,CATLG,DELETE),U' TO LINEA-21-40
           MOVE 'NIT=SYSDA,'           TO LINEA-41-60
           PERFORM 99-ESCRIBIR

           MOVE '//             SPACE' TO LINEA-01-20
           MOVE '=(CYL,(100,50),RLSE)' TO LINEA-21-40
           MOVE ','                    TO LINEA-41-60
           PERFORM 99-ESCRIBIR

           MOVE '//             DCB=(' TO LINEA-01-20
           MOVE 'RECFM=FB,BLKSIZE=0)'  TO LINEA-21-40
           PERFORM 99-ESCRIBIR.

      ******************************************************************
      *                                                                *
      *                     100320-SEPARACION-ARCHIVOS                 *
      *                                                                *
      *   SE CREA EL CONTENIDO DE LA TARJERA SYSIN, EL CUAL SEPARARA   *
      *   POR CADA FONDO                                               *
      ******************************************************************
       027-SEPARACION-ARCHIVOS.
           MOVE ' OUTFIL FNAMES=C'          TO LINEA-01-20(1:17)
           MOVE WS-TABLA-PRODUCTO(I-IND)    TO LINEA-01-20(17:2)
           MOVE ','                         TO LINEA-01-20(19:1)
           PERFORM 99-ESCRIBIR

           MOVE '         INCLUDE=(9,'          TO LINEA-01-20
           MOVE '2,CH,EQ,C'                     TO LINEA-21-40(1:9)
           MOVE CTE-COMILLA                     TO LINEA-21-40(10:1)
           MOVE WS-TABLA-PRODUCTO(I-IND)        TO LINEA-21-40(11:2)
           MOVE CTE-COMILLA                     TO LINEA-21-40(13:1)
           MOVE ')'                             TO LINEA-21-40(14:1)
           PERFORM 99-ESCRIBIR.

      ******************************************************************
      *                        999999-ESCRIBIR                         *
      ******************************************************************
       99-ESCRIBIR.
           WRITE REG-SALIDA FROM WS-LINEA
           IF WS-ESTADO NOT = '00'
              PERFORM 032-INI-ERR-PRO
              DISPLAY 'ERROR WRITE ARCHIVO SALIDA ' WS-ESTADO
              PERFORM 033-FIN-ERR-PRO
           ELSE
              INITIALIZE WS-LINEA
              ADD 1   TO WS-ESCRITOS
           END-IF.


      ******************************************************************
      *                       200000-FINAL                             *
      ******************************************************************
       03-FINAL.
           CLOSE  SALIDA
           PERFORM 034-MSGFINPRO
           STOP RUN.

       031-CERRAR-CURSOR.
           EXEC SQL
               CLOSE CURSOR_1
           END-EXEC

           IF SQLCODE NOT EQUAL ZEROS
               MOVE '100'                        TO WS-COD-RET
               MOVE 'ERR CERRANDO EL CURSOR1'    TO WS-ERROR-2
           END-IF.


      ******************************************************************
      *                      200001-INI-ERR-PRO                        *
      ******************************************************************
       032-INI-ERR-PRO.
           DISPLAY '************* TERMINACION ANORMAL *************'
           DISPLAY 'LEIDOS                : ' WS-LEIDOS
           DISPLAY 'REGISTROS ESCRITOS    : ' WS-ESCRITOS.

      ******************************************************************
      *                       200002-FIN-ERR-PRO                       *
      ******************************************************************
       033-FIN-ERR-PRO.
           MOVE 12 TO RETURN-CODE
           STOP RUN.

      ******************************************************************
      *                    200003-MSGFINPRO                            *
      ******************************************************************
       034-MSGFINPRO.
           DISPLAY 'LEIDOS ENTRADA1       : ' WS-LEIDOS
           DISPLAY 'REGISTROS ESCRITOS    : ' WS-ESCRITOS.
      ******************************************************************
