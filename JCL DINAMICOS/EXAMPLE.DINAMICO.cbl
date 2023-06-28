       ID DIVISION.
      ******************************************************************

       PROGRAM-ID.      TP3CDIN1.

      ******************************************************************
      *                                                                *
      *   FECHA ...........:  SEPTIEMBRE 2010                          *
      *   AUTOR ...........:  LUIS GIOVANNI MU#OZ GUTIERREZ            *
      *                       GESFOR(COLOMBIA)                         *
      *   LENGUAJE ........:  COBOL                                    *
      *                                                                *
      *                         DESCRIPCION:                           *
      *                         ============                           *
      *   PROCESO PARA GENERAR JCL DINAMICO QUE TOMA COMO ENTRADA LA   *
      *   DESCARGA DE LA TABLA TPDTEXT, LO ORDENA POR CONTRATO Y FECHA *
      *   DE OPERACION Y LOS SEPARA POR FONDO                          *
      ******************************************************************

       AUTHOR.          LUIS GIOVANNI MU#OZ GUTIERREZ.
       DATE-WRITTEN.    07/SEP/2010.
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

      **************ARCHIVO DE ENTRADA CON LOS FONDOS*******************
           SELECT ENTRADA1 ASSIGN TO ENTRADA1
                            FILE STATUS IS WS-ESTADO.
      *****************ARCHIVO DE SALIDA JCL DINAMICO*******************
           SELECT SALIDA1 ASSIGN TO SALIDA1
                            FILE STATUS IS WS-ESTADO.

      ******************************************************************
      *                         DATA DIVISION                          *
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.

       FD  ENTRADA1
                     RECORDING MODE IS F
                     BLOCK CONTAINS 0 RECORDS.
       01  REG-ENTRADA1.
           05 REG-EN1-CSUBPRD            PIC X(04).
      *----------------------------------------------
       FD  SALIDA1
                     RECORDING MODE IS F
                     BLOCK CONTAINS 0 RECORDS.
       01  REG-SALIDA1                   PIC X(80).
      *----------------------------------------------
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
          05 SW-FIN-ENTRADA1             PIC X     VALUE 'N'.
             88 SI-FIN-ENTRADA1                    VALUE 'S'.
             88 NO-FIN-ENTRADA1                    VALUE 'N'.

      ******************************************************************
      *                          CONTADORES                            *
      ******************************************************************
       01 CONTADORES.
          05 WS-LEIDOS-ENTRADA1          PIC 9(09) VALUE ZEROS.
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

      ******************************************************************
      *                           CONSTANTES                           *
      ******************************************************************
       01 WS-CONSTANTES.
          05 CTE-COMILLA                 PIC X(01) VALUE X'7D'.
          05 CTE-SORT-FIELDS             PIC X(11) VALUE 'SORT FIELDS'.

      ******************************************************************
      *                         TABLAS EN MEMORIA                      *
      ******************************************************************
       01 WS-TABLAS.
          05 WS-TAB-FONDOS OCCURS 100 TIMES INDEXED BY X.
             10 TAB-CSUBPRD              PIC X(04).

      ******************************************************************
       LINKAGE SECTION.
      ******************************************************************
       01 REG-PARM.
           05 PARM-LONG                  PIC X(02).
           05 PARM-AMBI                  PIC X(01).
           05 PARM-ARCH                  PIC X(38).

      ******************************************************************
      *                       PROCEDURE DIVISION                       *
      ******************************************************************
       PROCEDURE DIVISION USING REG-PARM.

           PERFORM 000000-INICIO
           PERFORM 100000-PROCESO
           PERFORM 200000-FINAL.

      ******************************************************************
      *                                                                *
      *                     000000-INICIO                              *
      *                                                                *
      *   - ABRIR ARCHIVOS DE ENTRADA Y SALIDA                         *
      *   - PRIMERA LECTURA DEL ARCHIVO DE ENTRADA                     *
      *   - CARGA EN TABLA DE MEMORIA EL ARCHIVO DE ENTRADA            *
      ******************************************************************
       000000-INICIO.
           INITIALIZE WS-TABLAS

           PERFORM 000010-OPEN-ARCHIVO
           PERFORM 000020-LEER-ENTRADA1
           PERFORM 000030-CARGAR-TABLA VARYING X FROM 1 BY 1
                                       UNTIL X > 100
                                       OR SI-FIN-ENTRADA1.

      ******************************************************************
      *                                                                *
      *                     000010-OPEN-ARCHIVO                        *
      *                                                                *
      *   - SE ABRE EL ARCHIVO DE ENTRADA Y EL DE SALIDA               *
      ******************************************************************
       000010-OPEN-ARCHIVO.
           OPEN INPUT ENTRADA1
           IF WS-ESTADO NOT = '00' AND NOT = '97'
              PERFORM 200001-INI-ERR-PRO
              DISPLAY 'ERROR ABRIENDO ARCHIVO ENTRADA1 ' WS-ESTADO
              PERFORM 200002-FIN-ERR-PRO
           END-IF

           OPEN OUTPUT SALIDA1
           IF WS-ESTADO NOT = '00' AND NOT = '97'
              CLOSE ENTRADA1
              PERFORM 200001-INI-ERR-PRO
              DISPLAY 'ERROR ABRIENDO ARCHIVO SALIDA1 ' WS-ESTADO
              PERFORM 200002-FIN-ERR-PRO
           END-IF.

      ******************************************************************
      *                                                                *
      *                   000020-LEER-ENTRADA1                         *
      *                                                                *
      *   SE LEE EL ARCHIVO ENTRADA1 EL CUAL CONTIENE LOS NOMBRES DE   *
      *   LOS FONDOS A PROCESAR                                        *
      ******************************************************************
       000020-LEER-ENTRADA1.
           INITIALIZE REG-ENTRADA1
           READ ENTRADA1
              AT END
                 SET SI-FIN-ENTRADA1 TO TRUE
              NOT AT END
                 ADD 1               TO WS-LEIDOS-ENTRADA1
           END-READ
           IF WS-ESTADO NOT = '00' AND NOT = '10'
              PERFORM 200001-INI-ERR-PRO
              DISPLAY 'ERROR LEYENDO ARCHIVO ENTRADA1 ' WS-ESTADO
              PERFORM 200002-FIN-ERR-PRO
           END-IF.

      ******************************************************************
      *                                                                *
      *                   000030-CARGAR-TABLA                          *
      *                                                                *
      *   SE CARGA EN MEMORIA LOS REGISTROS DEL ARCHIVO DE ENTRADA     *
      ******************************************************************
       000030-CARGAR-TABLA.
           MOVE REG-EN1-CSUBPRD      TO TAB-CSUBPRD(X)
           PERFORM 000020-LEER-ENTRADA1.

      ******************************************************************
      *                                                                *
      *                        100000-PROCESO                          *
      *                                                                *
      *   PROCESO PARA GENERAR JCL DINAMICO QUE TOMA COMO ENTRADA LA   *
      *   DESCARGA DE LA TABLA TPDTEXT, LO ORDENA POR CONTRATO Y FECHA *
      *   DE OPERACION Y LOS SEPARA POR FONDO                          *
      ******************************************************************
       100000-PROCESO.
           PERFORM 100100-GENERAR-CABECERA
           PERFORM 100200-GENERAR-BORRADO
           PERFORM 100300-GENERAR-CUERPO.

      ******************************************************************
      *                                                                *
      *                       100100-GENERAR-CABECERA                  *
      *                                                                *
      *   SE CREA LA CABECERA DEL JCL DINAMICO                         *
      ******************************************************************
       100100-GENERAR-CABECERA.
           INITIALIZE WS-LINEA

           MOVE '//'                   TO LINEA-01-20(1:2)
           MOVE PARM-AMBI              TO LINEA-01-20(3:1)
           MOVE 'TPA121A'              TO LINEA-01-20(4:7)
           MOVE ' JOB CLASS'           TO LINEA-01-20(11:10)
           MOVE '=B,REGION=0M,MSGCLAS' TO LINEA-21-40
           MOVE 'S=H,MSGLEVEL=(1,1)'   TO LINEA-41-60
           PERFORM 999999-ESCRIBIR

           MOVE '//'                   TO LINEA-01-20(1:2)
           MOVE 'JOBLIB   DD DSN=CO'   TO LINEA-01-20(3:18)
           MOVE 'B'                    TO LINEA-21-40(1:1)
           MOVE PARM-AMBI              TO LINEA-21-40(2:1)
           MOVE '.ALTAMIRA.LOADLIB.'   TO LINEA-21-40(3:18)
           MOVE 'BATCH,DISP=SHR'       TO LINEA-41-60
           PERFORM 999999-ESCRIBIR

           MOVE WS-ASTERISCOS          TO WS-LINEA
           MOVE '//'                   TO LINEA-01-20(1:2)
           PERFORM 999999-ESCRIBIR

           MOVE '//* SISTEMA     : TP' TO LINEA-01-20
           MOVE '(FONDOS)'             TO LINEA-21-40
           MOVE '*'                    TO LINEA-61-72(12:1)
           PERFORM 999999-ESCRIBIR

           MOVE '//* PROCESO     : TP' TO LINEA-01-20
           MOVE 'A121A.'               TO LINEA-21-40
           MOVE '*'                    TO LINEA-61-72(12:1)
           PERFORM 999999-ESCRIBIR

           MOVE '//* EJECUCION   : AN' TO LINEA-01-20
           MOVE 'UAL.'                 TO LINEA-21-40
           MOVE '*'                    TO LINEA-61-72(12:1)
           PERFORM 999999-ESCRIBIR

           MOVE '//* OBJETIVO    : SE' TO LINEA-01-20
           MOVE 'PARAR ARCHIVO DESCAR' TO LINEA-21-40
           MOVE 'GA TABLA TPDTEXT POR' TO LINEA-41-60
           MOVE ' FONDO Y   '          TO LINEA-61-72(1:11)
           MOVE '*'                    TO LINEA-61-72(12:1)
           PERFORM 999999-ESCRIBIR

           MOVE '//*               OR' TO LINEA-01-20
           MOVE 'DENAR POR FONDO, CON' TO LINEA-21-40
           MOVE 'TRATO Y FECHA DE OPE' TO LINEA-41-60
           MOVE 'RACION     '          TO LINEA-61-72
           MOVE '*'                    TO LINEA-61-72(12:1)
           PERFORM 999999-ESCRIBIR

           MOVE '//* REALIZO     : LU' TO LINEA-01-20
           MOVE 'IS GIOVANNI MU#OZ GU' TO LINEA-21-40
           MOVE 'TIERREZ (GESFOR COLO' TO LINEA-41-60
           MOVE 'MBIA)'                TO LINEA-61-72(1:5)
           MOVE '*'                    TO LINEA-61-72(12:1)
           PERFORM 999999-ESCRIBIR

           MOVE '//* FECHA       : SE' TO LINEA-01-20
           MOVE 'PTIEMBRE DE 2010.'    TO LINEA-21-40
           MOVE '*'                    TO LINEA-61-72(12:1)
           PERFORM 999999-ESCRIBIR

           MOVE WS-ASTERISCOS          TO WS-LINEA
           MOVE '//'                   TO LINEA-01-20(1:2)
           PERFORM 999999-ESCRIBIR.

      ******************************************************************
      *                                                                *
      *                      100200-GENERAR-BORRADO                    *
      *                                                                *
      *   SE CREA EL PRIMER PASO DEL JCL, EL CUAL ES EL BORRADO DE LOS *
      *   FICHEROS                                                     *
      ******************************************************************
       100200-GENERAR-BORRADO.
           MOVE WS-ASTERISCOS          TO WS-LINEA
           MOVE '//'                   TO LINEA-01-20(1:2)
           PERFORM 999999-ESCRIBIR

           MOVE '//* PASO 06: BORRADO' TO LINEA-01-20
           MOVE ' DE ARCHIVOS DE TRAB' TO LINEA-21-40
           MOVE 'AJO'                  TO LINEA-41-60
           MOVE '*'                    TO LINEA-61-72(12:1)
           PERFORM 999999-ESCRIBIR

           MOVE WS-ASTERISCOS          TO WS-LINEA
           MOVE '//'                   TO LINEA-01-20(1:2)
           PERFORM 999999-ESCRIBIR

           MOVE '//TP121A06 EXEC  PGM' TO LINEA-01-20
           MOVE '=IDCAMS'              TO LINEA-21-40
           PERFORM 999999-ESCRIBIR

           MOVE '//SYSPRINT DD  SYSOU' TO LINEA-01-20
           MOVE 'T=*'                  TO LINEA-21-40
           PERFORM 999999-ESCRIBIR

           MOVE '//SYSOUT   DD  SYSOU' TO LINEA-01-20
           MOVE 'T=*'                  TO LINEA-21-40
           PERFORM 999999-ESCRIBIR

           MOVE '//SYSIN    DD  *'     TO LINEA-01-20
           PERFORM 999999-ESCRIBIR

           PERFORM 100210-TARJETA-DELETE
                          VARYING X FROM 1 BY 1
                          UNTIL   X > 100
                          OR      TAB-CSUBPRD(X) = SPACES OR LOW-VALUES

           MOVE '  SET MAXCC=0'        TO LINEA-01-20
           PERFORM 999999-ESCRIBIR

           MOVE '  SET LASTCC=0'       TO LINEA-01-20
           PERFORM 999999-ESCRIBIR.

      ******************************************************************
      *                                                                *
      *                       100210-TARJETA-DELETE                    *
      *                                                                *
      *   SE CREA EL DELETE DE LOS FICHEROS DEL JCL DINAMICO           *
      ******************************************************************
       100210-TARJETA-DELETE.
           MOVE '  DELETE COB'         TO LINEA-01-20(1:12)
           MOVE PARM-AMBI              TO LINEA-01-20(13:1)
           MOVE '.TP.FJS'              TO LINEA-01-20(14:7)
           MOVE 'M1.TPA1214.TP120119.' TO LINEA-21-40
           MOVE 'EXT'                  TO LINEA-41-60(1:3)
           MOVE TAB-CSUBPRD(X)         TO LINEA-41-60(4:4)
           MOVE '.SORT'                TO LINEA-41-60(8:5)
           PERFORM 999999-ESCRIBIR.

      ******************************************************************
      *                                                                *
      *                      100300-GENERAR-CUERPO                     *
      *                                                                *
      *   SE CREA EL CUERPO DEL JCL EL CUAL CONTIENE LA DESCRIPCION DEL*
      *   PASO Y LA CREACION DE LOS FICHEROS DE SALIDA A PARTIR DEL    *
      *   ARCHIVO DESCARGA DE LA TABLA TPDTEXT                         *
      ******************************************************************
       100300-GENERAR-CUERPO.
           MOVE WS-ASTERISCOS          TO WS-LINEA
           MOVE '//'                   TO LINEA-01-20(1:2)
           PERFORM 999999-ESCRIBIR

           MOVE '//* PASO 03: ORDENA ' TO LINEA-01-20
           MOVE 'POR FONDO, CONTRATO,' TO LINEA-21-40
           MOVE ' FECHA DE OPERACION ' TO LINEA-41-60
           MOVE 'Y SEPARA   '          TO LINEA-61-72(1:11)
           MOVE '*'                    TO LINEA-61-72(12:1)
           PERFORM 999999-ESCRIBIR

           MOVE '//*          POR FON' TO LINEA-01-20
           MOVE 'DO'                   TO LINEA-21-40
           PERFORM 999999-ESCRIBIR

           MOVE WS-ASTERISCOS          TO WS-LINEA
           MOVE '//'                   TO LINEA-01-20(1:2)
           PERFORM 999999-ESCRIBIR

           MOVE '//TP121A03 EXEC  PGM' TO LINEA-01-20
           MOVE '=SORT,COND=(4,LT)'    TO LINEA-21-40
           PERFORM 999999-ESCRIBIR

           MOVE '//SYSPRINT DD  SYSOU' TO LINEA-01-20
           MOVE 'T=*'                  TO LINEA-21-40
           PERFORM 999999-ESCRIBIR

           MOVE '//SYSTSPRT DD  SYSOU' TO LINEA-01-20
           MOVE 'T=*'                  TO LINEA-21-40
           PERFORM 999999-ESCRIBIR

           MOVE '//SYSOUT   DD  SYSOU' TO LINEA-01-20
           MOVE 'T=*'                  TO LINEA-21-40
           PERFORM 999999-ESCRIBIR

           MOVE '//SORTIN   DD  DSN='  TO LINEA-01-20(1:19)
           MOVE PARM-ARCH(1:1)         TO LINEA-01-20(20:1)
           MOVE PARM-ARCH(2:20)        TO LINEA-21-40
           MOVE PARM-ARCH(22:17)       TO LINEA-41-60(1:17)
           MOVE ','                    TO LINEA-41-60(18:1)
           PERFORM 999999-ESCRIBIR

           MOVE '//             DISP=' TO LINEA-01-20
           MOVE 'SHR'                  TO LINEA-21-40
           PERFORM 999999-ESCRIBIR

           PERFORM 100310-ARCHIVOS-SALIDA
                          VARYING X FROM 1 BY 1
                          UNTIL   X > 100
                          OR      TAB-CSUBPRD(X) = SPACES OR LOW-VALUES

           MOVE '//SYSIN    DD  *'     TO LINEA-01-20
           PERFORM 999999-ESCRIBIR

           MOVE SPACES                 TO LINEA-01-20(1:2)
           MOVE CTE-SORT-FIELDS        TO LINEA-01-20(3:11)
           MOVE '=(1,13,'              TO LINEA-01-20(14:7)
           MOVE 'CH,A,17,10,CH,A,45,1' TO LINEA-21-40
           MOVE '6,CH,A)'              TO LINEA-41-60
           PERFORM 999999-ESCRIBIR

           PERFORM 100320-SEPARACION-ARCHIVOS
                          VARYING X FROM 1 BY 1
                          UNTIL   X > 100
                          OR      TAB-CSUBPRD(X) = SPACES OR LOW-VALUES.

      ******************************************************************
      *                                                                *
      *                     100310-ARCHIVOS-SALIDA                     *
      *                                                                *
      *   SE DEFINEN LOS NOMBRES DE LOS ARCHIVOS Y SUS ATRIBUTOS       *
      ******************************************************************
       100310-ARCHIVOS-SALIDA.
           MOVE '//'                   TO LINEA-01-20(1:2)
           MOVE TAB-CSUBPRD(X)         TO LINEA-01-20(3:4)
           MOVE '     DD  DSN=C'       TO LINEA-01-20(7:14)
           MOVE 'OB'                   TO LINEA-21-40(1:2)
           MOVE PARM-AMBI              TO LINEA-21-40(3:1)
           MOVE '.TP.FJSM1.TPA1214'    TO LINEA-21-40(4:17)
           MOVE '.TP120119.EXT'        TO LINEA-41-60(1:13)
           MOVE TAB-CSUBPRD(X)         TO LINEA-41-60(14:4)
           MOVE '.SO'                  TO LINEA-41-60(18:3)
           MOVE 'RT,'                  TO LINEA-61-72
           PERFORM 999999-ESCRIBIR

           MOVE '//             DISP=' TO LINEA-01-20
           MOVE '(NEW,CATLG,DELETE),U' TO LINEA-21-40
           MOVE 'NIT=SYSDA,'           TO LINEA-41-60
           PERFORM 999999-ESCRIBIR

           MOVE '//             SPACE' TO LINEA-01-20
           MOVE '=(CYL,(100,50),RLSE)' TO LINEA-21-40
           MOVE ','                    TO LINEA-41-60
           PERFORM 999999-ESCRIBIR

           MOVE '//             DCB=(' TO LINEA-01-20
           MOVE 'RECFM=FB,BLKSIZE=0)'  TO LINEA-21-40
           PERFORM 999999-ESCRIBIR.

      ******************************************************************
      *                                                                *
      *                     100320-SEPARACION-ARCHIVOS                 *
      *                                                                *
      *   SE CREA EL CONTENIDO DE LA TARJERA SYSIN, EL CUAL SEPARARA   *
      *   POR CADA FONDO                                               *
      ******************************************************************
       100320-SEPARACION-ARCHIVOS.
           MOVE '  OUTFIL FNAMES='     TO LINEA-01-20(1:16)
           MOVE TAB-CSUBPRD(X)         TO LINEA-01-20(17:4)
           MOVE ','                    TO LINEA-21-40
           PERFORM 999999-ESCRIBIR

           MOVE '         INCLUDE=(1,' TO LINEA-01-20
           MOVE '4,CH,EQ,C'            TO LINEA-21-40(1:9)
           MOVE CTE-COMILLA            TO LINEA-21-40(10:1)
           MOVE TAB-CSUBPRD(X)         TO LINEA-21-40(11:4)
           MOVE CTE-COMILLA            TO LINEA-21-40(15:1)
           MOVE ')'                    TO LINEA-21-40(16:1)
           PERFORM 999999-ESCRIBIR.

      ******************************************************************
      *                        999999-ESCRIBIR                         *
      ******************************************************************
       999999-ESCRIBIR.
           WRITE REG-SALIDA1 FROM WS-LINEA
           IF WS-ESTADO NOT = '00'
              PERFORM 200001-INI-ERR-PRO
              DISPLAY 'ERROR WRITE ARCHIVO SALIDA1 ' WS-ESTADO
              PERFORM 200002-FIN-ERR-PRO
           ELSE
              INITIALIZE WS-LINEA
              ADD 1   TO WS-ESCRITOS
           END-IF.

      ******************************************************************
      *                       200000-FINAL                             *
      ******************************************************************
       200000-FINAL.
           CLOSE ENTRADA1 SALIDA1
           PERFORM 200003-MSGFINPRO
           STOP RUN.

      ******************************************************************
      *                      200001-INI-ERR-PRO                        *
      ******************************************************************
       200001-INI-ERR-PRO.
           DISPLAY '************* TERMINACION ANORMAL *************'
           DISPLAY 'LEIDOS ENTRADA1       : ' WS-LEIDOS-ENTRADA1
           DISPLAY 'REGISTROS ESCRITOS    : ' WS-ESCRITOS.

      ******************************************************************
      *                       200002-FIN-ERR-PRO                       *
      ******************************************************************
       200002-FIN-ERR-PRO.
           MOVE 12 TO RETURN-CODE
           STOP RUN.

      ******************************************************************
      *                    200003-MSGFINPRO                            *
      ******************************************************************
       200003-MSGFINPRO.
           DISPLAY 'LEIDOS ENTRADA1       : ' WS-LEIDOS-ENTRADA1
           DISPLAY 'REGISTROS ESCRITOS    : ' WS-ESCRITOS.
      ******************************************************************
