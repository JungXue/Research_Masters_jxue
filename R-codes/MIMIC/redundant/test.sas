PROC IMPORT OUT= WORK.mimic3 
            DATAFILE= "H:\Desktop\7.1 No time to waste\Stats 798A Resear
ch Master\R-codes\MIMIC\MIMIC.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
