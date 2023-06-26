//PPED0012 JOB  CLASS=B,REGION=0M,MSGCLASS=H,MSGLEVEL=(1,1)             
//JOBLIB   DD   DSN=COBP.ALTAMIRA.LOADLIB.BATCH,DISP=SHR                
//********************************************************************* 
//* 20230620 FABIAN MOLINA
//**********************************************************************
//*          BORRA ARCHIVOS DE TRABAJO                                *   
//**********************************************************************
//PED12020 EXEC PGM=IDCAMS                                              
//SYSPRINT DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSIN    DD *
     DELETE COBP.PE.FWD1.PPED0003.SORT.CLIYCONT
     DELETE COBP.PE.FWSD1.PPED0030.SALIDACU
     SET MAXCC=0                                                        
     SET LASTCC=0

//*ORGANIZANDO DATOS DE ENTRADA POR CLIENTE Y CONTRATO
//TB000001 EXEC PGM=SORT
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=COBP.PE.FWD1.PPED003.SORT.PERSONAS,
//            DISP=SHR
//SORTOUT  DD DSN=COBP.PE.FWD1.PPED0003.SORT.CLIYCONT,
//            DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,
//            SPACE=(CYL,(100,50),RLSE),
//            DCB=(RECFM=FB,BLKSIZE=0)
//SYSIN    DD *
     SORT FIELDS=(19,8,CH,A)
     OUTREC FIELDS=(19,8,1,18)
//TP000002 EXEC PGM=PE3C0013
//SYSPRINT DD  SYSOUT=*                                                 
//SYSTSPRT DD  SYSOUT=*                                                 
//SYSOUT   DD  SYSOUT=*                                                 
//ENTRADA  DD  DSN=COBP.PE.FWD1.PPED0003.SORT.CLIYCONT,
//             DISP=SHR                                                 
//SALIDA   DD  DSN=COBP.PE.FWSD1.PPED0030.SALIDACU,
//             DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,                      
//             SPACE=(CYL,(100,50),RLSE),                               
//             DCB=(RECFM=FB,BLKSIZE=0)                                 
