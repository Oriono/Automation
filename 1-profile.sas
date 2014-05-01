%PARSE   DATE,&$DATE
%LET DAY=&$8.
%CONTROL CLEAR
%BEGIFANY  Monday=&DAY 33
%WRITE DO YOU WANT TO START WEEKLY TOOL SUBMISSION?
%WRITE *    PLEASE ENTER                                       *
%WRITE *    "Y" TO SUBMIT PROCESS AND "N" NOT TO SUBMIT        *
%LET COMMND=&?
%BEGIFANY  Y=&COMMND 32
%WRITE *********************************************************
%WRITE *                                                                                       *
%WRITE *       SUBMITTING WEEKLY PRODUCTION TOOLS UPDATE           *
%WRITE *                                                                                       *
%WRITE *          HAVE YOU SUBMITTED MSZHZ.TSLTGEN                       *
%WRITE *          TOOLLIST UPDATE ALREADY? (Y OR N)                        *
%WRITE *                                                                                       *
%WRITE *                                                                                       *
%WRITE *********************************************************
%LET WISH2=&?
%BEGIF &WISH2=N 22
%EXIT
%ENDIF   20
%BEGIF &WISH2=Y 31
%WRITE PLEASE ENTER "Y" AGAIN FOR CONFIRMATION
%LET CONF2=&?
%BEGIF &CONF2=Y 30
%MSZHZ.CLEANLOG
%MSZHZ.CHECKDIF
%MSZHZ.WHATSNEW
%ENDIF   26
%ENDIF   23
%ENDIF   9
%ENDIF   4
%WRITE
%WRITE

####this is the program running once mainframe terminal starts####
####it automately runs every Monday####
####submitting thress other jobs in a batch####
