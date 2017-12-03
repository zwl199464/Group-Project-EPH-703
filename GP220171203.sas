*ods option setting;
ods graphics on;
ods graphics / reset imagemap ;
ods noproctitle;
*import the data;
PROC Import
   Datafile="~/EPH 703/whas500_tks.csv"
   Out=work.eph703source
   DBMS=csv 
   Replace;
   Getnames=YES; 

RUN; /* n = 500, variables = 22 */
*Create formats;
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

   format gender gender. cvd afb sho chf av3 yn. 
          miord miord. mitype mitype. year year. dstat fstat stat.;
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
   /*Check for bad data*/
   where admitdate<=disdate<=fdate and 0< age < 200 ;
run;
*Get a general look at the data;
proc contents data=analysis;
run;
*checking for missing value;
proc means data=analysis n nmiss maxdec=2;
 var id--fstat   ;
run;
*Provide describtive statistics for the data;
proc means data=analysis n nmiss mean std  median q1 q3 min max maxdec=2;
    var age--fstat ;
    title "Descriptive Statistics on continuos variables";
run;title;
*Correlation matrix;
ods output Corr.PearsonCorr = counts;
ods output Corr.SimpleStats = ss;
ods output Corr.VarInformation= vi;
proc corr data= analysis;
    title "Correlation Coeffecient Matrix";
run;title;
*Find variable with |r|>0.1 ;
proc sql;
    create table corre  as 
        select Variable,Label,los,plos
            from counts as c
            where los >0.1 or los <-0.1 
       ;
quit;
proc print data=corre;
run;
*Scatter plot for the data with variable
 that have meaningfull correlation with the dependent RV.;
proc sgscatter data=analysis;
     plot los*(gender hr afb sho chf);
     title "Scatter plot";
run;title;
*Use stepwise selection method to verify the hypothesis above;
proc reg data=analysis;
      title "Regression Model ";
      model los = age--fdate dstat--fstat /  selection = stepwise details 
      ss1 ss2 stb clb cp vif partial ;
run; title;
* Use glm to test interaction predictors;
proc glm data=analysis;
      class gender afb sho chf;
      model los=gender afb sho chf hr gender*hr afb*hr sho*hr chf*hr;
      means gender afb sho chf / tukey deponly;
run;  
* Use scatter plot with the initial regression model to see
 if it's necessary to have transformation;
proc sgscatter data=analysis;
      plot los*(gender hr afb sho chf)/
      reg =(nogroup clm degree=2) grid ;
      title "Scatter plot with initial model";
run;title;
*Add transformation for both dependent and independent RV.;
data test ;
   set analysis;
       loghr=log(hr);
       rate= sysbp/diasbp;
       sqrthr=sqrt(hr);
       sqrtlos=sqrt(los);
       sqhr=(hr**2);
       sqlos =(los**2);
       loglos =log(los);
       cbhr = (hr**3);
       qthr = (hr**4);
       gh=gender*hr;
       ah= afb*hr ;
       sh= sho*hr ;
       ch=chf*hr;
    output;
run;
*Find variable with |r|>0.1 in the new dataset;
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
            sqlos >0.1 or sqlos <-0.1   or
            sqrtlos >0.1 or sqrtlos <-0.1
       ;
quit;
proc print data=corre;
run;
*Test all of the RV. with different transformed dependent RV.;
%macro reg (vr);
proc reg data=test;
        model &vr.=age--fdate  dstat--sqrthr sqhr cbhr--ch/ 
              selection = stepwise details;
run;
%mend;
%reg(los);
%reg(loglos);
%reg(sqlos);
%reg(sqrtlos);
*Using step wise selection method with higher significant level to rule out
those unnecessary RV.;
proc reg data=test;
     model loglos = afb sho chf gender dstat sqhr ch / 
           slentry=0.05 slstay=0.05 selection = stepwise details;
     model los= ah chf sh dstat gender/ 
           slentry=0.05 slstay=0.05 selection = stepwise details;
     model sqlos= ah chf sh diasbp/ 
           slentry=0.05 slstay=0.05 selection = stepwise details;
     model sqrtlos= afb chf gender sh dstat admitdate disdate age gh/ 
           slentry=0.05 slstay=0.05 selection = stepwise details;
run;
*Run the selected  model; 
proc reg data=test;
     model loglos = afb sho chf gender dstat   / ;
     model los= ah chf sh dstat /;
     model sqlos= ah chf sh diasbp/noint;
     model sqrtlos= afb chf gender  dstat sh /;
run;
*polynomial Regression Quadratic test;
proc reg data= test ;
     model los= hr sqhr cbhr qthr / clb cp vif partial ;
      test1: test hr=0;
      test2: test sqhr=0;
      test3: test cbhr=0;
run;
* Using different method to select model
 based on the pick in the correlation matrix;
%macro select(method);
proc reg data=test;
     model los=gender afb sho chf hr
           sqhr cbhr qthr loghr sqrthr /  
           selection = &method.;
run;
%mend;

%select(forward);
%select(backward);
%select(stepwise);
*Test for better model with different transformed dependent RV.;
%macro best(vrr);
proc reg data=test;
    model &vrr. = afb sho chf /noint ;
    model &vrr. = afb sho chf hr /noint;
    model &vrr. = afb sho chf hr /;
    model &vrr. = afb sho chf hr gender /;
    model &vrr. = afb sho chf hr gender /noint;
run;
%mend;

%best(los);
%best(loglos);
%best(sqlos);
%best(sqrtlos);
*Based on the result above 
test for chf and gender to see if they are necessary;
%macro choose(vrrr);
proc reg data=test;
     model &vrrr. = afb sho chf hr gender /noint;
      test1: test chf=0;
      test2: test gender=0;
run;
proc reg data=test;
      model &vrrr. = afb sho  hr gender /noint;
run;
%mend;

%choose(loglos);
%choose(sqrtlos);
*Run the final model;
ods trace on;
ods output StudResCooksDChart = scd;
ods output ResidualStatistics = rs;
ods output DiagnosticsPanel = dgp;
ods output ResidualPlot=rp;
ods output PartialPlot=pp;
proc reg data=test ;
       model sqrtlos = afb sho  hr gender /
       noint vif r  partial ss1 ss2 stb clb covb corrb;
run;
ods trace off;
*Check for outlier;
proc sgplot data=scd;
       histogram CooksD/ scale=COUNT;
run;

proc sgplot data=dgp ;
       scatter x= HatDiagonal y = Rstudent;
run;
/*Thank you*/


/*
proc sql;
CREATE TABLE blah AS
select dgp.PredictedValue,
       test.hr,
       test.sqrtlos
       from dgp,test;
run;
quit;
*/


      
      
      
       

