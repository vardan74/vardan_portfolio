 /*Summary of Overall Survival – Phase I*/
 
dm 'log;clear;'; 
%include "call setup.sas"; 
%let pgmname=qc_t_I_14_2_5.sas; 
%let dsname =qc_t_I_14_2_5;  
dm 'log;clear;'; 
proc delete data = work._all_ ;run ; 
 

 

data adsl1;
   set ads.adsl(where=(FASFL='Y' and POOLN=1));
   run;

Proc sql noprint ;
     
     select TRT01p, count(distinct USUBJID) as bign
	  into :grp1-, :N1-
	   
	    from adsl1
		group by TRT01p;
quit;

%put &grp1 &N1 &grp2 &N2;
 
data adtte1;
  set ads.adtte(where=(FASFL='Y' and POOLN=1));
run;


proc sql noprint;

  create table by_cns as
      select trt01p,trt01pn,CNSR,count(usubjid) as by_cnt
	 from adtte1
	 where not missing(aval) and paramcd in ('OS')
	 group by trt01p,trt01pn,CNSR;

  create table all_cns as
      select trt01p,trt01pn,count(usubjid) as cns_cnt
	 from adtte1
	 where not missing(aval) and paramcd in ('OS')
	 group by trt01p,trt01pn;

  create table cns as
      select a.trt01p,a.trt01pn,b.CNSR,b.by_cnt,a.cns_cnt,strip(put(round(by_cnt),best.)) || ' ' || '(' || strip(put(round((by_cnt/cns_cnt*100),0.1),4.1)) || ')' as value
	 from by_cns as b left join all_cns as a
	 on b.trt01p=a.trt01p;

	   create table n as
      select trt01p,trt01pn,strip(put(count(usubjid),best.))  as value  
	 from adtte1
	 where not missing(aval) and paramcd in ('OS')
	 group by trt01p,trt01pn;

		
quit;

proc sort data=adtte1(where=(paramcd in ('OS') and not missing(aval))) out=adtte2;
  
  by usubjid trt01p;
run;

data adtte2;
   set adtte2;
aval_m=aval/30.4375;
run;

ods output Quartiles=quart;
proc lifetest data=adtte2 method=KM  conftype=loglog alpha=0.05 alphaqt= 0.05 ;
  time aval*cnsr(1);
  strata trt01p;
  
run;

proc lifetest data=adtte2 method=KM conftype=loglog alpha=0.05 alphaqt= 0.05 intervals= 3 6 9 12 OUTSURV=estim_m plot=(s);
  time aval_m*cnsr(1);
  strata trt01p;
  
run;

data f;
  set n cns;
  drop BY_CNT CNS_CNT;
run;
data f;
  set f;
  output;
  if _N_ =1 then do;
    value='0';
    trt01pn=1;
	trt01p='Cohort -1: 100mg MG010 + 200mg sorafenib';
   output;
  end;
 run;


data cns_dummy;
length value $200.;
 do trt01p = 'Cohort -1: 100mg MG010 + 200mg sorafenib', 'Cohort 1: 200mg MG010 + 200mg sorafenib', 'Cohort 2: 400mg MG010 + 200mg sorafenib';
    if trt01p = 'Cohort -1: 100mg MG010 + 200mg sorafenib' then trt01pn=1;
      else if trt01p = 'Cohort 1: 200mg MG010 + 200mg sorafenib' then trt01pn=2;
        else if trt01p = 'Cohort 2: 400mg MG010 + 200mg sorafenib' then trt01pn=3;
       do cnsr = 0, 1;
		   value='0';
		   ord=1;
	       output;
	   end;
end;

run;

proc sort data=f;by trt01pn  cnsr;run;
proc sort data=cns_dummy;by trt01pn  cnsr;run;

data f1;
merge cns_dummy(in=a) f;
 by trt01pn  cnsr;
 ord=1;
run;

data q_dummy;

 do trt01p = 'Cohort -1: 100mg MG010 + 200mg sorafenib', 'Cohort 1: 200mg MG010 + 200mg sorafenib', 'Cohort 2: 400mg MG010 + 200mg sorafenib';
    if trt01p = 'Cohort -1: 100mg MG010 + 200mg sorafenib' then trt01pn=1;
      else if trt01p = 'Cohort 1: 200mg MG010 + 200mg sorafenib' then trt01pn=2;
        else if trt01p = 'Cohort 2: 400mg MG010 + 200mg sorafenib' then trt01pn=3;
     do PERCENT= 25, 50, 75;   
		   value=0;
		   ord=2;
	       output;
	 end;  

end;

run;

data quart;
  set quart;
  if trt01p = 'Cohort -1: 100mg MG010 + 200mg sorafenib' then trt01pn=1;
      else if trt01p = 'Cohort 1: 200mg MG010 + 200mg sorafenib' then trt01pn=2;
        else if trt01p = 'Cohort 2: 400mg MG010 + 200mg sorafenib' then trt01pn=3;
run;

proc sort data=quart(rename=(ESTIMATE=value));by trt01pn percent;run;
proc sort data=q_dummy;by trt01pn percent;run;

data quart1;
merge q_dummy(in=a) quart;
 by trt01pn percent;
 if a;
run;

data quart2(rename=(value1=value));
  set  quart1;
   length value1  $50.;
   if trt01p = 'Cohort -1: 100mg MG010 + 200mg sorafenib' then value1='0';
   else if missing(UPPERLIMIT) and  missing(lowerlimit) and not missing(value) then value1=strip(put(value,4.1)) || ' ' || '(' || 'N.A.' || ',' || ' ' || 'N.A.' || ')';
      
   else if missing(UPPERLIMIT) and missing(value) and missing(lowerlimit) then value1='N.A.' || ' ' || '(' || 'N.A.' || ',' || ' ' || 'N.A.' || ')';

   else if not missing(UPPERLIMIT) and not missing(value) and not missing(lowerlimit) then 
	value1=strip(put(value,4.1)) || ' ' || '(' || strip(put(lowerlimit,5.1)) || ',' || ' ' || strip(put(UPPERLIMIT,4.1)) || ')';
   else if missing(UPPERLIMIT) and  not missing(value) and not missing(lowerlimit) then 
	value1=strip(put(value,4.1)) || ' ' || '(' || strip(put(lowerlimit,5.1)) || ',' || ' ' || 'N.A.' || ')';
  else if missing(UPPERLIMIT) and   missing(value) and not missing(lowerlimit) then 
	value1='N.A.' || ' ' || '(' || strip(put(lowerlimit,5.1)) || ',' || ' ' || 'N.A.' || ')';


keep trt01p trt01pn percent value1 ord;
run;


data pfs_dummy;

 do trt01p = 'Cohort -1: 100mg MG010 + 200mg sorafenib', 'Cohort 1: 200mg MG010 + 200mg sorafenib', 'Cohort 2: 400mg MG010 + 200mg sorafenib';
    if trt01p = 'Cohort -1: 100mg MG010 + 200mg sorafenib' then trt01pn=1;
      else if trt01p = 'Cohort 1: 200mg MG010 + 200mg sorafenib' then trt01pn=2;
        else if trt01p = 'Cohort 2: 400mg MG010 + 200mg sorafenib' then trt01pn=3;
     do aval_m= 3, 6, 9,12;   
		   value='0';
		   ord=3;
	       output;
	 end;  

end;

run;

proc sort data=all_cns;by trt01pn;run;
proc sort data=pfs_dummy;by trt01pn;run;

data pfs_dummy1;
   merge pfs_dummy(in=a) all_cns;
   by trt01pn;
   if a;
   if trt01pn=1 then CNS_CNT=0;
run;

data Estim_m;
  set Estim_m;
  if trt01p = 'Cohort -1: 100mg MG010 + 200mg sorafenib' then trt01pn=1;
      else if trt01p = 'Cohort 1: 200mg MG010 + 200mg sorafenib' then trt01pn=2;
        else if trt01p = 'Cohort 2: 400mg MG010 + 200mg sorafenib' then trt01pn=3;
proc sort;by trt01pn;
run;
data Estim_m;
  merge Estim_m(in=a) all_cns;
  by trt01pn;
  if a;
run;



proc sort data=pfs_dummy1;by trt01pn aval_m;run;
proc sort data=Estim_m;by trt01pn aval_m;run;

data Estim_max(keep=trt01pn lastaval);
	set Estim_m;
	by trt01pn aval_m;
	lastaval = aval_m;
	if last.trt01pn then output;
run;

data estim1;
   merge pfs_dummy1(in=a) Estim_m ;
   by trt01pn aval_m;
run;
data estim1;
   merge estim1(in=a) Estim_max ;
   by trt01pn ;
run;


proc sort data=estim1;by trt01pn aval_m;run;


data estim2;
length value $200.;
	set estim1;
	by trt01pn aval_m;
	retain SURVIVAL SDF_LCL  SDF_UCL temp  tempupper templow aval;

    if first.trt01pn then do;
		if not missing(survival) and missing(_CENSOR_) then do;aval=aval_m;temp=survival;tempupper=SDF_UCL; templow=SDF_LCL;end;
		  
	end;
	else do;
	  if not missing(survival) and _CENSOR_=0 then do;temp=survival;tempupper=SDF_UCL; aval=aval_m;templow=SDF_LCL;end;
	    else do;survival=temp;end;
		  if _CENSOR_=1 then do;SDF_UCL=tempupper; SDF_LCL=templow;end;
		  if missing(_CENSOR_) or  _CENSOR_=1 then do;SDF_UCL=tempupper; SDF_LCL=templow;survival=temp;end;
		  if _CENSOR_=1 and aval_m > aval then do; call missing(SDF_UCL,SDF_LCL,survival);end;
		  if  missing(_CENSOR_) and aval_m > lastaval then do; call missing(SDF_UCL,SDF_LCL,survival);end;
		  if not missing(_CENSOR_) then do; SDF_UCL=tempupper; SDF_LCL=templow;survival=temp;end;
	end;

      if aval>=0 and aval<3 then qtr1=1;
	    else if aval>=3 and aval<6 then qtr1=2;
	     else if aval>=6 and aval<9 then qtr1=3;
	      else if aval>=9 and aval=<12 then qtr1=4;

       if aval_m in(0 3) then qtr2=1;
	     else if aval_m=6 then qtr2=2;
		  else if aval_m=9then qtr2=3;
		   else if aval_m=12 then qtr2=4;


/*		   if qtr1<qtr2 then call missing(SDF_UCL,survival,SDF_LCL);*/
	if aval_m in (3 6 9 12);
run;


data estim3;
   set estim2;
   length lcl ucl $200.;
if survival gt 0 then do;
   surv=put(round(survival*100,.1),5.1);
   if not missing(SDF_LCL) then lcl=put(round(SDF_LCL*100,.1),5.1);
     else lcl='N.A.';
   if not missing(SDF_UCL) then ucl=put(round(SDF_UCL*100,.1),5.1);
     else ucl='N.A.';
    value= strip(surv) || ' (' || strip(lcl) || ', ' || strip(ucl) || ')';
end;
	else if not missing(survival) then value='0';
	 else value='N.A.';

keep trt01p trt01pn value aval_m ord;
run;

data final;
  set f1(in=a) quart2(in=b) estim3(in=c);
 
run;

proc sort data=final out=final1;
  by cnsr aval_m percent ord;
run;

proc transpose data=final1 out=final2 prefix=trt;
 by cnsr aval_m percent ord;
 id trt01pn;
 var value;
run;

proc sort data=final2 out= final3;
  by ord cnsr percent aval_m;
run;

data final4;
   set final3;
   length txt1 txt2 $200.;
	if ord=1 then do;
		 txt1='Descriptive';
		 if missing(cnsr) then txt2='n';
		   else if cnsr=0 then txt2='Event n(%)';
		     else if cnsr=1 then txt2='Censored n(%)';
	end;

	if ord=2 then do;
		 txt1='Overall Survival (days)';
		 if percent=25 then txt2='25 Percentile (95% CI)';
		   else if percent=50 then txt2='Median (95% CI)';
		     else if percent=75 then txt2='75 Percentile (95% CI)';
	end;

	if ord=3 then do;
		 txt1='Survival Rate (95% CI)';
		 if aval_m=3 then txt2='Month 3 (%)';
		   else if aval_m=6 then txt2='Month 6 (%)';
		     else if aval_m=9 then txt2='Month 9 (%)';
			 else if aval_m=12 then txt2='Month 12 (%)';
	end;


run;

data final4_;
length col1  col2   col3  $200.;
  set final4;
  	col1=trt1;
	col2=trt2;
	col3=trt3;
  keep txt1 txt2 col1-col3;
run;



data v_tabdat.&dsname;
   set final4_;
run;
proc compare b=tabdat.t_I_14_2_5 c= work.final4_ listall;
run;
