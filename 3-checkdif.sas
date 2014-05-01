%LET JOBID=C0K,GO=YES
%JCLSETUP: %CREATE 1,1
%PARSE   DATE,&$DATE
%LET YY=&$1
%LET MM=&$2
%LET DD=&$3
%LET SDATE=&YY-&MM-&DD
%WRITE SDATE=&YY-&MM-&DD
%LET TR1=20
%LET TRY=&TR1&SDATE
%LET TDATE=&MM&DD&YY
%REM   TRY &TRY
%/&$USERID&JOBID JOB 1910-0000-1000,TIME=(0,20),REGION=6M,TYPRUN=HOLD,
%/    MSGLEVEL=(1,1),MSGCLASS=M,NOTIFY=&$USERID
/*JOBPARM LINES=999
%/*
%/SCRUNC   EXEC SERV
OPT,NOSETRC,NOERROR
SCR,,MSZHZ.TSPROD.TOOLLIST.BK
%/*
%/RENAME EXEC SERV
REN,,MSZHZ.TSPROD.TOOLLIST,MSZHZ.TSPROD.TOOLLIST.BK
%/*
%/MAKE EXEC GUTSUTIL,CONTROL='$'
%/SYSPUNCH DD DSN=&&TEMPC,
%/      DCB=(LRECL=240,BLKSIZE=23280,RECFM=FB),
%/       SPACE=(TRK,(10,10),RLSE),
%/      UNIT=SYSSQ,DISP=(NEW,PASS,DELETE)
%/SYSIN DD *
$PUNCH  MSZHZ.TEMPCON1
%/*
%/MAKE EXEC GUTSUTIL,CONTROL='$'
%/SYSPUNCH DD DSN=&&TEMPR,
%/      DCB=(LRECL=240,BLKSIZE=23280,RECFM=FB),
%/       SPACE=(TRK,(10,10),RLSE),
%/      UNIT=SYSSQ,DISP=(NEW,PASS,DELETE)
%/SYSIN DD *
$PUNCH  MSZHZ.TEMPARC1
%/MAKE EXEC GUTSUTIL,CONTROL='$'
%/SYSPUNCH DD DSN=&&TEMPP,
%/      DCB=(LRECL=240,BLKSIZE=23280,RECFM=FB),
%/       SPACE=(TRK,(10,10),RLSE),
%/      UNIT=SYSSQ,DISP=(NEW,PASS,DELETE)
%/SYSIN DD *
$PUNCH  MSZHZ.TEMPPRD1
%/*
%/MAKE EXEC GUTSUTIL,CONTROL='$'
%/SYSPUNCH DD DSN=&&TEMPD,
%/      DCB=(LRECL=240,BLKSIZE=23280,RECFM=FB),
%/       SPACE=(TRK,(10,10),RLSE),
%/      UNIT=SYSSQ,DISP=(NEW,PASS,DELETE)
%/SYSIN DD *
$PUNCH  MSZHZ.TEMPDEV1
%/*
%/SAS   EXEC SAS,OPTIONS='EMAILHOST=SMTP.INFORES.COM',
%/      WORK='1200,600',REGION=6M,SORT=10
%/FT22F001 DD   SYSOUT=*
%/*
%/GUTSOUTC DD SYSOUT=(F,&$USERID),DEST=GUTS,DSN=&&CPYPR2CN
%/GUTSOUTR DD SYSOUT=(F,&$USERID),DEST=GUTS,DSN=&&CPYPR2AR
%/*
%/LSTCONT DD DSN=&&TEMPC,DISP=(OLD,DELETE)
%/LSTARCH DD DSN=&&TEMPR,DISP=(OLD,DELETE)
%/LSTPROD DD DSN=&&TEMPP,DISP=(OLD,DELETE)
%/LSTDVLP DD DSN=&&TEMPD,DISP=(OLD,DELETE)
%/ORIGLIST  DD DSN=MSZHZ.TSPROD.TOOLLIST.BK,DISP=SHR
%/ORIGOUT  DD DSN=MSZHZ.TSPROD.TOOLLIST,
%/         DISP=(NEW,CATLG,DELETE),UNIT=USR,SPACE=(TRK,(20,20),RLSE),
%/         DCB=(LRECL=60,BLKSIZE=8880,RECFM=FB)
%/*
%/SYSOUT   DD DUMMY
%/SYSIN DD *
%SET        TEMPORARY,NOCOMMAND

 OPTIONS NOCENTER MLOGIC SYMBOLGEN MPRINT;

DATA DATE2DAY;
 INFILE CARDS;
 INPUT DDATE1 YYMMDD10.;
 CARDS;
&TRY
;
DATA DATE2DAY;
 SET DATE2DAY;
 MONTH=TRIM(LEFT(PUT(DDATE1,MONNAME.)));
 MONTH1=SUBSTR(MONTH,1,3);
 DATESTRING=PUT(DAY(DDATE1),Z2.)||"-"||
            TRIM(MONTH1)||"-"||
            PUT(DDATE1,YEAR4.);
KEEP DATESTRING DDATE1;
RUN;
PROC SQL NOPRINT;
SELECT DATESTRING INTO: SDATE1 FROM DATE2DAY;
QUIT;

     %MACRO READFILE(FILE=);
 DATA &&FILE;
  INFILE &&FILE DLM='*' DSD ;
  INPUT  @4 TOOLNAM :$17.
  ;
  TNAME=SUBSTR(TOOLNAM,8,8);
 RUN;
*ROC PRINT DATA=&&FILE(OBS=5);

 PROC SORT;
  BY TNAME;

   %MEND READFILE;

     %READFILE(FILE=LSTCONT);
     %READFILE(FILE=LSTARCH);
     %READFILE(FILE=LSTPROD);
     %READFILE(FILE=LSTDVLP);

PROC SQL;
  SELECT COUNT(*) INTO: COUNTCNT FROM LSTCONT ;
  SELECT COUNT(*) INTO: COUNTARC FROM LSTARCH ;
  SELECT COUNT(*) INTO: COUNTPRD FROM LSTPROD ;
  SELECT COUNT(*) INTO: COUNTDEV FROM LSTDVLP ;
QUIT;


DATA PRODNOCONT CONTNOPROD;
 MERGE LSTPROD(IN=A) LSTCONT (IN=B);
 BY TNAME;
 IF A AND NOT B THEN OUTPUT PRODNOCONT;
 IF B AND NOT A THEN OUTPUT CONTNOPROD;
RUN;

DATA PRODNOARCH ARCHNOPROD;
 MERGE LSTPROD(IN=A) LSTARCH (IN=B);
 BY TNAME;
 IF A AND NOT B THEN OUTPUT PRODNOARCH;
 IF B AND NOT A THEN OUTPUT ARCHNOPROD;
RUN;

PROC SORT DATA=PRODNOARCH;
 BY TNAME;

PROC SORT DATA=PRODNOCONT;
 BY TNAME;
RUN;

PROC SQL;
  SELECT COUNT(*) INTO:NOARCH  FROM PRODNOARCH;
  SELECT COUNT(*) INTO:NOCONT  FROM PRODNOCONT;
QUIT;

  %MACRO MYMAC1;

   %IF &&NOARCH GT 0 OR &&NOCONT GT 0 %THEN %DO;

****************************************************************************;
****************************************************************************;
                /*   TSCONT DIRECTORY PROCESSING  */
****************************************************************************;
****************************************************************************;

      %IF &&NOCONT GT 0 %THEN %DO;

      DATA PRD2CONT;
       SET PRODNOCONT;
        HEADER='TSCOPY ';
        SEP=',';
        FROM='TSPROD.';
        TO=',TSCONT.';
        SUBM=HEADER||TRIM(FROM)||TRIM(TNAME)||TRIM(TO)||TNAME;
      RUN;

      PROC PRINT DATA=PRD2CONT (OBS=5);
       VAR TNAME SUBM;
      RUN;

      DATA _NULL_;
       SET PRD2CONT;
       FILE GUTSOUTC NOTITLES;
         PUT @1 SUBM;
      RUN;

   /*   REPORT OF TOOLS TO BE COPIED TO TSCONT DIRECTORY   */

      DATA CONTDIR;
      SET PRODNOCONT;
      KEEP TNAME;

      PROC SORT DATA=CONTDIR;
       BY TNAME;

      DATA CONTDIR;
       SET CONTDIR;
       LENGTH TARG $300.;
       RETAIN TARG;
       BY TNAME;
        IF FIRST.TNAME THEN DO;
          END;
        TARG = TRIM(TARG)||" "||TNAME;
        IF LAST.TNAME THEN OUTPUT;
       RUN;

      DATA CONTDIR ;* (RENAME= (NNAME1=TARG));
       SET CONTDIR;
       NNAME=TRIMN(RIGHT(TARG ));
       NNAME1=TRIMN(LEFT(NNAME ));
      *DROP TARG NNAME;
      RUN;

   /*  RETAIN LIST OF TOOLS TO BE COPIED TO        */
       /*        TSCONT DIRECTORIES  */

    %LET DSID = %SYSFUNC(OPEN(CONTDIR));
    %LET NOBS = %SYSFUNC(ATTRN(&&DSID,NOBS));

      DATA CONTDIR1;
         SET CONTDIR(FIRSTOBS=&&NOBS);
      RUN;

      PROC PRINT DATA=CONTDIR1 (OBS=10);
      RUN;

      PROC SQL;
        SELECT TARG INTO:CONTDIR0 FROM CONTDIR1;
      QUIT;

       PROC CONTENTS DATA=CONTDIR1;

       PROC PRINT DATA=CONTDIR1;
        TITLE 'CHECK1 ';
       RUN;

    %END;   /* END OF TSCONT CHECK  */


****************************************************************************;
****************************************************************************;
                /*   TSARCH DIRECTORY PROCESSING  */
****************************************************************************;
****************************************************************************;
       %IF &&NOARCH GT 0 %THEN %DO;

      DATA PRD2ARCH;
       SET PRODNOARCH;
       HEADER='TSCOPY ';
       SEP=',';
       FROM='TSPROD.';
       TO=',TSARCH.';
       SUBM=HEADER||TRIM(FROM)||TRIM(TNAME)||TRIM(TO)||TNAME;
      RUN;

      PROC PRINT DATA=PRD2ARCH(OBS=5);
       VAR TNAME SUBM;
      RUN;

      DATA _NULL_;
       SET PRD2ARCH;
       FILE GUTSOUTR NOTITLES;
         PUT @1 SUBM;
      RUN;


    /*   REPORT OF TOOLS TO BE COPIED TO TSARCH DIRECTORY   */

      DATA ARCHDIR;
        SET PRODNOARCH;
      KEEP TNAME;

      PROC SORT DATA=ARCHDIR;
       BY TNAME;

      DATA ARCHDIR;
       SET ARCHDIR;
       LENGTH TARG $300.;
       RETAIN TARG;
       BY TNAME;
        IF FIRST.TNAME THEN DO;
          END;
        TARG = TRIM(TARG)||" "||TNAME;
        IF LAST.TNAME THEN OUTPUT;
       RUN;

      DATA ARCHDIR; * (RENAME= (NNAME1=TARG ));
      SET ARCHDIR;
       NNAME=TRIMN(RIGHT(TARG ));
       NNAME1=TRIMN(LEFT(NNAME ));
      *DROP TARG NNAME;
      RUN;

   /*  RETAIN LIST OF TOOLS TO BE COPIED TO        */
       /*        TSARCH DIRECTORIES  */

    %LET DSID = %SYSFUNC(OPEN(ARCHDIR));
    %LET TOBS2= %SYSFUNC(ATTRN(&&DSID,NOBS));


      DATA ARCHDIR1;
         SET ARCHDIR(FIRSTOBS=&&TOBS2);
      RUN;

      PROC PRINT DATA=ARCHDIR1 (OBS=10);
      RUN;

      PROC SQL;
        SELECT TARG INTO:ARCHDIR0 FROM ARCHDIR1;
      QUIT;

       PROC CONTENTS DATA=ARCHDIR1;

       PROC PRINT DATA=ARCHDIR1;
         TITLE 'CHECK2 ';
        RUN;

    %END;   /* END OF TSARCH CHECK  */

    /* CHECK IF BOTH THERE ARE TOOLS TO BE COPIED TO   */
          /*   TSARCH AND TSCONT DIRECTORIES   */
          /*   AND THEN SEND A MAIL   */


   %IF &&NOARCH GT 0 AND &&NOCONT GT 0 %THEN %DO;
       %LET TOLIST="Orion.Zhao@iriworldwide.com";

       %LET CCLIST1="Gail.Daily@iriworldwide.com";
       %LET CCLIST2="Cynthia.Schumacher@iriworldwide.com";
       %LET CCLIST3="Chetan.Sharma2@GENPACT.COM";
       %LET CCLIST4="  ";
       %LET CCLIST5="Vivek.Dangeti@GENPACT.COM";
       %LET CCLIST=&&CCLIST1 &&CCLIST2;

          FILENAME MYMAIL EMAIL TO=(&&TOLIST)

           SUBJECT="DIRECTORY UPDATED MAIL";

DATA _NULL_;
  FILE MYMAIL;
PUT 'Hi All, ';
PUT ' ';
PUT ' The below is the weekly report status of the ';
PUT '   ';
PUT '           TSPROD, TSCONT, TSARCH, TSDVLP directories';
PUT ' ';
PUT " There are &&COUNTCNT tools in TSCONT Directory as on &&SDATE1";
PUT " There are &&COUNTARC tools in TSARCH Directory as on &&SDATE1";
PUT " There are &&COUNTPRD tools in TSPROD Directory as on &&SDATE1";
PUT " There are &&COUNTDEV tools in TSDVLP Directory as on &&SDATE1";
PUT ' ';
PUT "There are &&NOBS TSPROD tools to be transferred to TSCONT directory";
PUT '   ';
PUT ' The below mentioned are the list of tools should be copied to TSCONT';
PUT "&&CONTDIR0";
PUT ' ';
PUT "There are &&TOBS2 TSPROD tools to be transferred to TSARCH directory.";
PUT ' ';
PUT "The below mentioned are the list of tools should be copied to TSARCH";
put "&&ARCHDIR0";
PUT ' ';
PUT ' ';
put " Submit the below 2 tools,      ";
put "  which will copy the tools to TSCONT/TSARCH Directories";
put "  1. MSZHZ.CPYPR2CN  ";
put "  2. MSZHZ.CPYPR2AR  ";
PUT ' ';
PUT 'Thanks,';
PUT "&$USERID";
RUN;



    %END;


   %IF &&NOARCH GT 0 AND &&NOCONT EQ 0 %THEN %DO;

       %LET TOLIST="Orion@*******.com";
          FILENAME MYMAIL EMAIL TO=(&&TOLIST)

           SUBJECT="DIRECTORY UPDATED MAIL";
DATA _NULL_;
  FILE MYMAIL;
PUT 'Hi All, ';
PUT ' ';
PUT ' The below is the weekly report status of the ';
PUT '   ';
PUT '           TSPROD, TSCONT, TSARCH, TSDVLP directories';
PUT ' ';
PUT " There are &&COUNTCNT tools in TSCONT Directory as on &&SDATE1";
PUT " There are &&COUNTARC tools in TSARCH Directory as on &&SDATE1";
PUT " There are &&COUNTPRD tools in TSPROD Directory as on &&SDATE1";
PUT " There are &&COUNTDEV tools in TSDVLP Directory as on &&SDATE1";
PUT ' ';
PUT "There are NO TSPROD tools to be transferred to TSCONT directory";
PUT '   ';
PUT "There are &&TOBS2 TSPROD tools to be transferred to TSARCH directory.";
PUT ' ';
PUT "The below mentioned are the list of tools should be copied to TSARCH";
put "&&ARCHDIR0";
PUT ' ';
PUT ' ';
put " Submit the below tool,      ";
put "  which will copy the tools to TSARCH Directories";
put "  1. MSZHZ.CPYPR2AR  ";
PUT ' ';
PUT 'Thanks,';
PUT "&$USERID";
RUN;

    %END;


   %IF &&NOARCH EQ 0 AND &&NOCONT GT 0 %THEN %DO;
       %LET TOLIST="Orion@*******.com";
          FILENAME MYMAIL EMAIL TO=(&&TOLIST)

           SUBJECT="DIRECTORY UPDATED MAIL";
DATA _NULL_;
  FILE MYMAIL;
PUT 'Hi All, ';
PUT ' ';
PUT ' The below is the weekly report status of the ';
PUT '   ';
PUT '           TSPROD, TSCONT, TSARCH, TSDVLP directories';
PUT ' ';
PUT " There are &&COUNTCNT tools in TSCONT Directory as on &&SDATE1";
PUT " There are &&COUNTARC tools in TSARCH Directory as on &&SDATE1";
PUT " There are &&COUNTPRD tools in TSPROD Directory as on &&SDATE1";
PUT " There are &&COUNTDEV tools in TSDVLP Directory as on &&SDATE1";
PUT ' ';
PUT "There are NO TSPROD tools to be transferred to TSARCH directory";
PUT '   ';
PUT "There are &&TOBS2 TSPROD tools to be transferred to TSCONT directory.";
PUT ' ';
PUT "The below mentioned are the list of tools should be copied to TSCONT";
put "&&CONTDIR0";
PUT ' ';
PUT ' ';
put " Submit the below tool,      ";
put "  which will copy the tools to TSCONT Directories";
put "  1. MSZHZ.CPYPR2CN  ";
PUT ' ';
PUT 'Thanks,';
PUT "&$USERID";
RUN;


    %END;

    %END;

   %MEND MYMAC1;


  %MACRO MYMAC2;

     %IF &&NOARCH EQ 0 AND &&NOCONT EQ 0 %THEN %DO;
       %LET TOLIST="Orion@*******.com";
          FILENAME MYMAIL EMAIL TO=(&&TOLIST)

       SUBJECT="DIRECTORY UPDATED MAIL";

DATA _NULL_;
  FILE MYMAIL;
PUT 'Hi All, ';
PUT ' ';
PUT ' The below is the weekly report status of the ';
PUT '   ';
PUT '           TSPROD, TSCONT, TSARCH, TSDVLP directories';
PUT ' ';
PUT " There are &&COUNTCNT tools in TSCONT Directory as on &&SDATE1";
PUT " There are &&COUNTARC tools in TSARCH Directory as on &&SDATE1";
PUT " There are &&COUNTPRD tools in TSPROD Directory as on &&SDATE1";
PUT " There are &&COUNTDEV tools in TSDVLP Directory as on &&SDATE1";
PUT ' ';
PUT " There are no tools to be transferred to TSCONT/TSARCH this week";
PUT ' ';
PUT "There are &&NOCONT TSPROD tools to be transferred to TSCONT";
PUT "There are &&NOARCH TSPROD tools to be transferred to TSARCH";
PUT ' ';
PUT ' ';
PUT 'Thanks,';
PUT "&$USERID";
RUN;


    %END;

   %MEND MYMAC2;

  %MYMAC1;
  %MYMAC2;




DATA TSARCH;
 LENGTH DIRECTORY $8.;
 FATE ="&TDATE";
 DIRECTORY="TSARCH";
 TOOL =&&COUNTARC;
 WEEK1=INPUT(FATE,MMDDYY8.);
RUN;

DATA TSCONT;
 LENGTH DIRECTORY $8.;
 FATE="&TDATE";
 DIRECTORY="TSCONT";
 TOOL=&&COUNTCNT;
 WEEK1=INPUT(FATE,MMDDYY8.);
RUN;

DATA TSDVLP;
 LENGTH DIRECTORY $8.;
 FATE ="&TDATE";
 DIRECTORY="TSDVLP";
 TOOL =&&COUNTDEV;
 WEEK1=INPUT(FATE,MMDDYY8.);
RUN;

DATA TSPROD;
 LENGTH DIRECTORY $8.;
 FATE="&TDATE";
 DIRECTORY="TSPROD";
 TOOL=&&COUNTPRD;
 WEEK1=INPUT(FATE,MMDDYY8.);
RUN;

DATA ALLPROD (DROP=FATE);
SET TSARCH TSCONT TSDVLP TSPROD;
RUN;

PROC SORT;
BY DIRECTORY;
RUN;

PROC PRINT DATA=ALLPROD;TITLE 'ALLPROD DATA';
RUN;

DATA ORIGLIST;
INFILE ORIGLIST;
INPUT
  @1 WEEK1 DATE10.
  @15 DIRECTORY $8.
  @25 TOOL
;
RUN;

DATA ORIGLIST;
SET ORIGLIST;
RUN;

PROC PRINT; TITLE 'ORIGPROD DATA';
RUN;

DATA ORIGPROD;
SET ORIGLIST;*DROP=FATE);
RUN;

PROC SORT DATA=ORIGPROD;
 BY WEEK1  ;
RUN;

PROC CONTENTS DATA=ORIGPROD;
PROC CONTENTS DATA=ALLPROD;


PROC APPEND DATA=ALLPROD BASE=ORIGPROD FORCE;
RUN;

PROC PRINT;
 FORMAT WEEK1 DDMMYY10.;
RUN;

DATA _NULL_;
SET ORIGPROD;
FILE ORIGOUT ;
PUT
  @1 WEEK1 DATE9.
  @15 DIRECTORY $8.
  @25 TOOL
;
RUN;



  ENDSAS;
%IFNONE  N-NO=&GO %PERFORM %ENDSUBMIT
