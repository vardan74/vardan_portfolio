
************************************************************ ;
*  Program name     : qc_t_I_14_2_3.sas
*  Project          : X:\BioMetrics\SAS programming\Greenlight Clinical\MG 010-001 02\csr\val\pg\tables\
*  Written by       :  Vardan Isakhanyan  
*  Date of creation : Mon 11/07/2022 (mm/dd/yyyy) 
*  Description      : To Validate Table qc_t_I_14_2_3 
*  Input file       : 
*  Output file      : qc_t_I_14_2_3.sas7bdat, qc_t_I_14_2_3.rtf 
*  Revision History : 
*  Date      Author   Description of the change 
********************************************************************************** ; 

/*Summary of Progression-free Survival – Phase I*/

dm 'log;clear;'; 
%include "call setup.sas"; 
%let pgmname=qc_t_I_14_2_3.sas; 
%let dsname =qc_t_I_14_2_3;  
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
	 where not missing(aval) and paramcd in ('PFS')
	 group by trt01p,trt01pn,CNSR;

  create table all_cns as
      select trt01p,trt01pn,count(usubjid) as cns_cnt
	 from adtte1
	 where not missing(aval) and paramcd in ('PFS')
	 group by trt01p,trt01pn;

  create table cns as
      select a.trt01p,a.trt01pn,b.CNSR,b.by_cnt,a.cns_cnt,strip(put(by_cnt,3.)) || ' ' || '(' || strip(put(round((by_cnt/cns_cnt*100),0.1),5.1)) || ')' as value
	 from by_cns as b left join all_cns as a
	 on b.trt01p=a.trt01p;

  create table n as
      select trt01p,trt01pn,count(usubjid)  as value  
	 from adtte1
	 where not missing(aval) and paramcd in ('PFS')
	 group by trt01p,trt01pn;

		
quit;

data cns;
	set cns;
	value = tranwrd(value,'100.0', '100');
run;

proc sort data=adtte1(where=(paramcd in ('PFS') and not missing(aval))) out=adtte2;
  
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

data n;
	set n(rename = (value = value1));
	value = strip(put(value1,best.));
	drop value1;
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
merge q_dummy(in=a) quart(in = b);
 by trt01pn percent;
 if a or b;
run;

data quart2(rename=(value1=value));
length value1  $50. value $200;
  set  quart1(rename=(value=value2));
   
   value = strip(put(value2,5.1));
   if missing(value) then value = 'N.A.';
   if missing(UPPERLIMIT) and trt01pn ne 1 then value1=strip(value) || ' ' || '(' || strip(put(lowerlimit,5.1))
     || ',' || ' ' || 'N.A.' || ')';
   else value1='0';
   
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

data estim1;
   merge pfs_dummy1(in=a) Estim_m;
   by trt01pn aval_m;
run;

data estim2;
	set estim1;
	by trt01pn aval_m;
	retain temp temp2;
	if first.trt01pn then do;
		if missing(survival) and cns_cnt eq 0 then survival = 0;
		temp = survival; 
		if not missing(_CENSOR_) then temp2 = _CENSOR_;
		else temp2 = 0;
	end;
	else do;
		if missing(_CENSOR_) then _CENSOR_ = temp2;
		else temp2 = _CENSOR_;
		if not missing(survival) then temp = survival;
		if missing(survival) and _CENSOR_ eq 0 then survival = temp;
		else if missing(survival) and _CENSOR_ eq 1 then call missing(survival);
	end;
	if aval_m in (3 6 9 12) then output;
run;

data estim3;
length lcl ucl value $200.;
   set estim2;
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
		 txt1='Progression-free Survival (days)';
		 if percent=25 then txt2='25 Percentile (95% CI)';
		   else if percent=50 then txt2='Median (95% CI)';
		     else if percent=75 then txt2='75 Percentile (95% CI)';
	end;

	if ord=3 then do;
		 txt1='Progression-free Survival Rate@n(95% CI)';
		 if aval_m=3 then txt2='Month 3 (%)';
		   else if aval_m=6 then txt2='Month 6 (%)';
		     else if aval_m=9 then txt2='Month 9 (%)';
			 else if aval_m=12 then txt2='Month 12 (%)';
	end;


run;

data final4;
	length COL1 - COL3 $200;
  set final4;
  	col1=trt1;
	col2=trt2;
	col3=trt3;
  keep txt1 txt2 col2 col3;
run;

data v_tabdat.&dsname;
   set final4;
run;

proc compare b=tabdat.t_I_14_2_3 c= work.final4 list;
run;

%chklog_inter;


