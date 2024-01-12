

/*Summary of Plasma Concentrations of MG010 (Unit) – Phase I*/
dm 'log;clear;'; 
%include "call setup.sas"; 
%let pgmname=qc_I_14_2_7_1.sas; 
%let dsname =qc_I_14_2_7_1;  
dm 'log;clear;'; 
proc delete data = work._all_ ;run ; 
 
 
 

data adsl1;
   set ads.adsl(where=(PKPOPFL='Y' and POOLN=1));
   run;

Proc sql noprint;  
     select trt01a,count(USUBJID) into: trt1-,:n1- 
	 from adsl1
     group by TRT01A;
quit;

%put &trt1   &n1 ;

 data Adpc1;
   set ads.adpc(where=(PKPOPFL='Y' and Pooln=1 and Paramcd='MGCONC'));
run;
 


proc sort data=Adpc1/*(where=(DTYPE ne 'ZERO' and DTYPE ne 'HALFLLOQ'))*/ out=stat1;by trt01an trt01a avisitn avisit pctpt pctptnum;run;
data stat1;
  set stat1;
  if not missing(aval) and (aval ne 0) then ln_aval=log(aval);
run;




proc means data=stat1 noprint;
  by trt01an trt01a avisitn avisit pctpt pctptnum;
  var aval;
  output out=stat2 n=_n mean=_mean stddev=_stddev max=_max min=_min median=_median;
run;

proc sort data=Adpc1(where=(DTYPE eq 'ZERO' or DTYPE eq 'HALFLLOQ')) out=stat_blq;
		by trt01an trt01a avisitn avisit pctpt pctptnum;
run;
proc means data=stat_blq noprint;
  by trt01an trt01a avisitn avisit pctpt pctptnum;
  var aval;
  output out=stat_blq1 n=n_nblq;
run;

proc sort data=stat1;by trt01an trt01a avisitn avisit pctpt pctptnum;run;

proc means data=stat1 noprint;
  by trt01an trt01a avisitn avisit pctpt pctptnum;
  var ln_aval;
  output out=stat_log mean=a_mean stddev=a_stddev;
run;

data all(drop= _TYPE_ _FREQ_);
   merge stat2(in=a) stat_blq1(in=b) stat_log(in=c);
   by trt01an trt01a avisitn avisit pctpt pctptnum;
   if a or b or c;
run;

data all1;
  set all;
  if not missing(a_mean) then geo_mean=exp(a_mean);
 
  if not missing(a_stddev) then do;
	   geo_cv=(sqrt(exp(a_stddev**2) - 1)) * 100;
  end;

    if (_mean ne 0) then  cv=_STDDEV/_mean*100;
   
run;



data dummy;
length stat $200. trt01a $200. avisit $200. pctpt $50.;
  do trt01a ='Cohort -1: 100mg MG010 + 200mg sorafenib', 'Cohort 1: 200mg MG010 + 200mg sorafenib','Cohort 2: 400mg MG010 + 200mg sorafenib';
	if trt01a ='Cohort -1: 100mg MG010 + 200mg sorafenib' then trt01an=1;
	  if trt01a = 'Cohort 1: 200mg MG010 + 200mg sorafenib' then trt01an=2;
	    if trt01a = 'Cohort 2: 400mg MG010 + 200mg sorafenib' then trt01an=3;
      do avisit='Day 1/Cycle 1';
	   avisitn=1001;
	    do pctpt ='0.5hr pre','0.5hr post','1hr post','2hr post', '3hr post', '4hr post', '5hr post','6hr post',  '8hr post',  '12hr post',  '24hr post';
			    
                if pctpt= '0.5hr pre' then pctptnum=-0.5;
				if pctpt= '0.5hr post' then pctptnum=0.5;
				if pctpt= '1hr post' then pctptnum=1;
                if pctpt= '2hr post'  then pctptnum=2;
				if pctpt= '3hr post' then pctptnum=3;
				if pctpt= '4hr post' then pctptnum=4;
				if pctpt= '5hr post'  then pctptnum=5;
				if pctpt= '6hr post'  then pctptnum=6;
				if pctpt= '8hr post'  then pctptnum=8;
				if pctpt= '12hr post'  then pctptnum=12;
				
				if pctpt= '24hr post' then pctptnum=24;


				do stat= 'n','nBLQ','Mean','SD','CV%','Median','Minimum','Maximum','Geometric Mean','Geometric CV%';
				    if stat='n' then statn=1;
                    if stat='nBLQ' then statn=2;
					if stat='Mean' then statn=3;
					if stat='CV%' then statn=5;
					if stat='SD' then statn=4;
					if stat='Median' then statn=6;
					if stat='Minimum' then statn=7;
					if stat='Maximum' then statn=8;
					if stat='Geometric Mean' then statn=9;
					if stat='Geometric CV%' then statn=10;
                     
					 aval=0;
				     output;
				end;
			
	   end;
	end;
  end;
run;
proc sort data=dummy;by trt01an trt01a avisitn avisit pctptnum pctpt;run;
proc sort data=all1;by trt01an trt01a avisitn avisit pctptnum pctpt;run;

data final1;
    merge dummy(in=a) all1;
	by trt01an trt01a avisitn avisit pctptnum pctpt;
	if a;
run;

data final2;
  set final1;
  length value $10.;
 
    if stat='n' and not missing(_n) then value=strip(put(_N,5.));
	  
    else if stat='nBLQ' and not missing(N_NBLQ) then value=strip(put(N_NBLQ,5.));
	 
	else if stat='Mean' and not missing(_mean) then value=strip(put(_mean,8.2));
	
	else if stat='CV%' and not missing(cv) then value=strip(put(cv,8.1));
	
	else if stat='SD' and not missing(_STDDEV) then value=strip(put(_STDDEV,8.2));
	
	else if stat='Median' and not missing(_MEDIAN) then value=strip(put(_MEDIAN,8.2));
	
	else if stat='Minimum' and not missing(_min) then value=strip(put(_min,8.1));
	
	else if stat='Maximum' and not missing(_max) then value=strip(put(_max,8.1));
	
	else if stat='Geometric Mean' and not missing(GEO_MEAN) then value=strip(put(GEO_MEAN,8.2));
	
	else if stat='Geometric CV%' and not missing(GEO_CV) then value=strip(put(GEO_CV,8.1));
	keep trt01an trt01a statn stat avisitn avisit pctptnum pctpt value;
run;

proc sort data=final2;by  avisitn avisit statn stat pctpt pctptnum;run;
proc transpose data=final2 out= final3(drop=_NAME_);
  by  avisitn avisit statn stat pctpt pctptnum;
  id trt01an;
  var value;
run;

proc sort data=final3;by  avisitn avisit  pctptnum statn stat;run;
data final4(drop=pctpt);
  length pctp1 $200. col2-col3 $200. txt $200.;
  set final3;
                if pctpt= '0.5hr pre' then pctp1='Pre-dose';
				else if pctpt= '0.5hr post' then pctp1='0.5 hours post-dose';
				else if pctpt= '1hr post' then pctp1='1 hours post-dose';
                else if pctpt= '2hr post'  then pctp1='2 hours post-dose';
				else if pctpt= '3hr post' then pctp1='3 hours post-dose';
				else if pctpt= '4hr post' then pctp1='4 hours post-dose';
				else if pctpt= '5hr post'  then pctp1='5 hours post-dose';
				else if pctpt= '6hr post'  then pctp1='6 hours post-dose';
				else if pctpt= '8hr post'  then pctp1='8 hours post-dose';
				else if pctpt= '12hr post'  then pctp1='12 hours post-dose';
							
				else if pctpt= '24hr post' then pctp1='24 hours post-dose';
				 txt=stat;


				  if txt in ('n' 'nBLQ') and missing(_2) then col2='0';
				    else if txt in ('Mean'  'SD' 'Median' 'Geometric Mean') and missing(_2) then col2='0.00';
					  else if txt in ('Geometric CV%' 'CV%') and missing(_2)  then col2='-';
					    else if txt in ('Minimum' 'Maximum') and missing(_2) then col2='0.00';
					      else col2=_2;
	  
   				  if txt in ('n' 'nBLQ') and missing(_3) then col3='0';
				    else if txt in ('Mean'  'SD' 'Median' 'Geometric Mean') and missing(_3) then col3='0.00';
					  else if txt in ('Geometric CV%' 'CV%') and missing(_3)  then col3='-';
					    else if txt in ('Minimum' 'Maximum') and missing(_3) then col3='0.00';
					      else col3=_3;

				if txt in ('n' 'nBLQ' ) then do;
					col1='0';
				
				end;
				else do;
				    col1='-';
				
				end;
			
			  
run;

data  V_tabdat.&dsname.;
   set final4;
   rename pctp1=pctpt;
   keep col2 col3 txt pctp1;
run;
proc compare b=V_tabdat.&dsname. c=tabdat.t_I_14_2_7_1 list;
run;


