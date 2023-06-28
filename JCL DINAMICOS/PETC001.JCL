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
     DELETE COBP.PE.FWSD1.PPED0055.SALIDA
     SET MAXCC=0                                                        
     SET LASTCC=0

//TP000002 EXEC PGM=PE3C0019
//SYSPRINT DD  SYSOUT=*                                                 
//SYSTSPRT DD  SYSOUT=*                                                 
//SYSOUT   DD  SYSOUT=*                                                 
//SALIDA   DD  DSN=COBP.PE.FWSD1.PPED0055.SALIDA,
//             DISP=(NEW,CATLG,DELETE),UNIT=SYSDA,                      
//             SPACE=(CYL,(100,50),RLSE),                               
//             DCB=(RECFM=FB,BLKSIZE=0)                                 
