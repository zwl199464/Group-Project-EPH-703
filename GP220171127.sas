ods graphics on;
ods graphics / reset imagemap ;
ods noproctitle;

PROC Import
   Datafile="~/EPH 703/whas500_tks.csv"
   Out=work.eph703source
   DBMS=csv 
   Replace;
   Getnames=YES;

RUN;

proc format; 
	value gender 0="Male" 1="Female";
	value yn 0="NO" 1="YES";
	value miord 0="First" 1="Recurrent";
	value mitype 0="non Q-wave" 1="Q-wave";
	value year 1="1997" 2="1999" 3="2001";
	value stat 0="Alive" 1="Dead";


     	
run;
	
*Creat analysis dataset to analysis the data;


data analysis;
set work.eph703source;

format gender gender. cvd afb sho chf av3 yn. miroid miroid. mitype mitype. year year. dstat fstat stat.;

label id="Identification Code";
label age="Age at Hosipistal Admission";
label gender="Gender";
label hr="Initial Heart Rate";
label sysbp="Initial Systolic Blood Pressure";
label diasbp="Initail Diastolic Blood pressure";
label bmi="Blood Mass Index";
label cvd="History of Cariovascular Disease";
label afb="Atrail Fibrillation";
label sho="Cardiogenic Shock";
label chf="Congestive heart Compications";
label av3="Complete Heart Block";
label miord="MI Order";
label mitype="MI Type";
label year="Cohort Year";
label admitdate="Hospital Admission date";
label disdate="Hospital Dicharge Date";
label fdate="Date of Last Follow Up";
label dstat="Dicharge Status from Hospital";
label lenfol="Total Length of Follow-up";
label fstat="Vital Statuss ar Last Follow UP";
label Ios="Length of Hospital Stay";

run;

 
proc contents data=analysis;
run;

*checking for missing value;
/*proc freq data=analysis;
tables age--miroid;  
run;*/
proc means data=analysis n nmiss maxdec=2;
 var age--miroid   ;
run;
*Provide describtive statistics for the data;
proc means data=analysis n nmiss mean std  median q1 q3 min max maxdec=2;
 var age--miroid   ;
title "Descriptive Statistics on continuos variables";
run;title;
ods output Corr.PearsonCorr = counts;
ods output Corr.SimpleStats = ss;
 ods output Corr.VarInformation= vi;
proc corr data= analysis;
title "Correlation Coeffecient Matrix";
run;title;
proc sql;
    create table corre  as 
        select Variable,Label,los,plos
            from counts as c
            where los >0.1 or los <-0.1 
       ;
quit;
proc print data=corre;
run;
*Scatter plot for the data;
proc sgscatter data=analysis;
 plot los*(gender hr afb sho chf);
 title "Scatter plot";
run;title;
proc reg data=analysis;
title "Regression Model ";
      model los = gender hr afb sho chf / ss1 ss2 stb clb covb corrb vif r  partial ;
run; title;

   proc glm data=analysis;
      class gender afb sho chf;
      model los=gender afb sho chf hr gender*hr afb*hr sho*hr chf*hr;
      means gender afb sho chf / tukey deponly;
     
   run;

data test ;
set analysis;
loghr=log(hr);
rate=los/hr;
sqrthr=sqrt(hr);
sqrtlos=sqrt(los);
sqhr=(hr**2);
sqlos =(los**2);
loglos =log(los);
output;
run;
ods output Corr.PearsonCorr = counts;
ods output Corr.SimpleStats = ss;
 ods output Corr.VarInformation= vi;
proc corr data= test;
title "Correlation Coeffecient Matrix";
run;title;
proc sql;
    create table corre  as 
        select Variable,Label,los,plos,loglos,ploglos,sqlos,psqlos,
        sqrtlos,psqrtlos
            from counts as c
            where los >0.1 or los <-0.1 or
            loglos >0.1 or loglos <-0.1 or
            sqlos >0.1 or sqlos <-0.1 or
             sqrtlos >0.1 or sqrtlos <-0.1
       ;
quit;
proc print data=corre;
run;

proc glm data=analysis;
class gender ;
      model los=hr gender /ss1;
      means gender / hovtest=levene hovtest=bartlett;
     
   run;