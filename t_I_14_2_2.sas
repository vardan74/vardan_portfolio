/*Summary of Sum of Diameters for Target Lesions – Phase I*/

dm 'log;clear;'; 
%include "call setup.sas"; 
%let pgmname=t_I_14_2_2.sas; 
%let dsname =t_I_14_2_2;  
dm 'log;clear;'; 
proc delete data = work._all_ ;run ; 
 


data adsl1;
   set ads.adsl(where=(FASFL='Y' and POOLN=1));
   run;

Proc sql noprint;  
     select trt01p,count(USUBJID) into: trt1-,:n1- 
	 from adsl1
     group by trt01p;
quit;

%put &trt1 &trt2  &n1 &n2;

 data Adtm1;
   set ads.Adtm(where=(FASFL='Y' and Pooln=1 and Param='Sum of Diameters for Target Lesions'));
run;
 
data stat1;
  set adtm1(where=(ABLFL='Y'));
run;

proc sort data=stat1;by trt01p avisitn avisit;run;
proc means data=stat1 noprint;
  by trt01p avisitn avisit;
  var aval;
  output out=baseline n=_n mean=_mean std=_std max=_max min=_min median=_median;
run;


data stat2;
  set adtm1(where=(PSTBLFL='Y'));
run;

proc sort data=stat2;by trt01p avisitn avisit;run;
proc means data=stat2 noprint;
  by trt01p avisitn avisit;
  var aval;
  output out=post n=_n mean=_mean std=_std max=_max min=_min median=_median;
run;

/*proc sort data=stat2;by trt01p avisitn avisit;run;*/
proc means data=stat2 noprint;
  by trt01p avisitn avisit;
  var chg;
  output out=postchg n=_n mean=_mean std=_std max=_max min=_min median=_median;
run;
/*proc sort data=stat2;by trt01p avisitn avisit;run;*/
proc means data=stat2 noprint;
  by trt01p avisitn avisit;
  var pchg;
  output out=postpchg n=_n mean=_mean std=_std max=_max min=_min median=_median;
run;


data all;
   set baseline(in=a) post(in=b) postchg(in=c) postpchg(in=d);
   by trt01p avisitn avisit;
   if a then ord=1;
      else if b then ord=2;
	  else if c then ord=3;
	  else if d then ord=4;

run;
proc sort data=all;by ord avisitn avisit;run;
proc transpose data=all out=all1;
  by ord avisitn avisit;
  id trt01p;
  var _n _mean _std _max _min _median;
run;

data all2;
   set all1;
   length stat $30.;
   if ord ne 4 then do;
	   if _NAME_='_N' then do;
	     stat='n';
		 if not missing(COHORT_1__200MG_MG010___200MG_SO) then Grp1=strip(put(COHORT_1__200MG_MG010___200MG_SO,5.));
		   else Grp1='-';
		 if not missing(COHORT_2__400MG_MG010___200MG_SO) then Grp2=strip(put(COHORT_2__400MG_MG010___200MG_SO,5.));
		    else Grp2='-';
		  Grp3='-';
		 statord=1;
	   end;
	   if _NAME_='_MEAN' then do;
	     stat='Mean';
		 if not missing(COHORT_1__200MG_MG010___200MG_SO) then Grp1=strip(put(COHORT_1__200MG_MG010___200MG_SO,5.1));
		   else Grp1='-';
		 if not missing(COHORT_2__400MG_MG010___200MG_SO) then Grp2=strip(put(COHORT_2__400MG_MG010___200MG_SO,5.1));
		    else Grp2='-';
			Grp3='-';
		 statord=2;
	   end;

	   if _NAME_='_STD' then do;
	     stat='SD';
		 if not missing(COHORT_1__200MG_MG010___200MG_SO) then Grp1=strip(put(COHORT_1__200MG_MG010___200MG_SO,5.1));
		   else Grp1='-';
		 if not missing(COHORT_2__400MG_MG010___200MG_SO) then Grp2=strip(put(COHORT_2__400MG_MG010___200MG_SO,5.1));
		    else Grp2='-';
			Grp3='-';
		 statord=3;
	   end;
	      if _NAME_='_MAX' then do;
	     stat='Maximum';
		 if not missing(COHORT_1__200MG_MG010___200MG_SO) then Grp1=strip(put(COHORT_1__200MG_MG010___200MG_SO,5.));
		   else Grp1='-';
		 if not missing(COHORT_2__400MG_MG010___200MG_SO) then Grp2=strip(put(COHORT_2__400MG_MG010___200MG_SO,5.));
		    else Grp2='-';
			Grp3='-';
		 statord=6;
	   end;
	   if _NAME_='_MIN' then do;
	     stat='Minimum';
		 if not missing(COHORT_1__200MG_MG010___200MG_SO) then Grp1=strip(put(COHORT_1__200MG_MG010___200MG_SO,5.));
		   else Grp1='-';
		 if not missing(COHORT_2__400MG_MG010___200MG_SO) then Grp2=strip(put(COHORT_2__400MG_MG010___200MG_SO,5.));
		    else Grp2='-';
			Grp3='-';
		  statord=5;
       end;
		   if _NAME_='_MEDIAN' then do;
		     stat='Median';
			 if not missing(COHORT_1__200MG_MG010___200MG_SO) then Grp1=strip(put(COHORT_1__200MG_MG010___200MG_SO,5.1));
			   else Grp1='-';
			 if not missing(COHORT_2__400MG_MG010___200MG_SO) then Grp2=strip(put(COHORT_2__400MG_MG010___200MG_SO,5.1));
			    else Grp2='-';
				Grp3='-';
			  statord=4;
		   end;
  end;


  else if ord=4 then do;
	      if _NAME_='_N' then do;
	     stat='n';
		 if not missing(COHORT_1__200MG_MG010___200MG_SO) then Grp1=strip(put(COHORT_1__200MG_MG010___200MG_SO,5.));
		   else Grp1='-';
		 if not missing(COHORT_2__400MG_MG010___200MG_SO) then Grp2=strip(put(COHORT_2__400MG_MG010___200MG_SO,5.));
		    else Grp2='-';
		  Grp3='-';
		 statord=1;
	   end;
	   if _NAME_='_MEAN' then do;
	     stat='Mean';
		 if not missing(COHORT_1__200MG_MG010___200MG_SO) then Grp1=strip(put(COHORT_1__200MG_MG010___200MG_SO,5.2));
		   else Grp1='-';
		 if not missing(COHORT_2__400MG_MG010___200MG_SO) then Grp2=strip(put(COHORT_2__400MG_MG010___200MG_SO,5.2));
		    else Grp2='-';
			Grp3='-';
		 statord=2;
	   end;

	   if _NAME_='_STD' then do;
	     stat='SD';
		 if not missing(COHORT_1__200MG_MG010___200MG_SO) then Grp1=strip(put(COHORT_1__200MG_MG010___200MG_SO,5.2));
		   else Grp1='-';
		 if not missing(COHORT_2__400MG_MG010___200MG_SO) then Grp2=strip(put(COHORT_2__400MG_MG010___200MG_SO,5.2));
		    else Grp2='-';
			Grp3='-';
		 statord=3;
	   end;
	      if _NAME_='_MAX' then do;
	     stat='Maximum';
		 if not missing(COHORT_1__200MG_MG010___200MG_SO) then Grp1=strip(put(COHORT_1__200MG_MG010___200MG_SO,5.1));
		   else Grp1='-';
		 if not missing(COHORT_2__400MG_MG010___200MG_SO) then Grp2=strip(put(COHORT_2__400MG_MG010___200MG_SO,5.1));
		    else Grp2='-';
			Grp3='-';
		 statord=6;
	   end;
	   if _NAME_='_MIN' then do;
	     stat='Minimum';
		 if not missing(COHORT_1__200MG_MG010___200MG_SO) then Grp1=strip(put(COHORT_1__200MG_MG010___200MG_SO,5.1));
		   else Grp1='-';
		 if not missing(COHORT_2__400MG_MG010___200MG_SO) then Grp2=strip(put(COHORT_2__400MG_MG010___200MG_SO,5.1));
		    else Grp2='-';
			Grp3='-';
		  statord=5;
	   end;
	   if _NAME_='_MEDIAN' then do;
	     stat='Median';
		 if not missing(COHORT_1__200MG_MG010___200MG_SO) then Grp1=strip(put(COHORT_1__200MG_MG010___200MG_SO,5.2));
		   else Grp1='-';
		 if not missing(COHORT_2__400MG_MG010___200MG_SO) then Grp2=strip(put(COHORT_2__400MG_MG010___200MG_SO,5.2));
		    else Grp2='-';
			Grp3='-';
		  statord=4;
	   end;
 end;

run;


data all3;
   set all2;
  length col1 $100.;
   if ord=1 then col1='Baseline';
   if ord=2 then col1=scan(strip(avisit),1,'/');
   if ord=3 then col1=scan(strip(avisit),1,'/') || ' ' || 'Change from Baseline';
   if ord=4 then col1=scan(strip(avisit),1,'/') || ' ' || 'Percentage Change from Baseline (%)';

    
run;

data final;
   set all3;

col2=stat;
col3=grp3;
col4=grp1;
col5=grp2;
keep col1-col5 ord avisitn  statord;
run;

proc sort data=final;by  avisitn ord statord;run;


%let count_per_page=24;

data final1;
set final;
by  avisitn ord statord;
	retain page 1 line 0;
	set final;
	by  avisitn ord statord;
	if line=&count_per_page  /*and  last.avisittn and statord eq 6*/ then do;
		line=0;
		page=page+1;
	end;
	else line=line+1;
	
run;

 
options papersize=letter orientation=landscape nodate nonumber center missing=" " nobyline; 
ods escapechar="@"; 

ods listing close; 
 
 %mstrtrtf(pgmname=&pgmname, pgmid=1);  
 

 data tabdat.&dsname.;
	set final1 ;
	keep col1 col2 col4 col5;
 run;

 proc report data=final1 nowd headskip headline split='~'  missing wrap spacing=4;
  column   page    col1 col2 col4 col5 /*_11*/;
    define page / order noprint;
	
	
	define col1 / order order=data "Sum of Diameters of All Target Lesions (mm) @nVisit" style(column)=[ width=37%  just=l asis=on]  style(header)=[just=l  asis=on];
    define col2 / "Statistics" style(column)=[ width=8%  just=l asis=on]  style(header)=[just=l  asis=on];
	*define col3 / "Cohort -1: 100mg MG010 + 200mg sorafenib ~ (N=0)" style(column)=[ width=15%  just=c asis=on]  style(header)=[just=c  asis=on];
    define col4 / "200mg MG010 (QD) +~200mg Sorafenib (QD)~(N=&n1.)" style(column)=[ width=27%  just=c asis=on]  style(header)=[just=c  asis=on];
    define col5 / "200mg MG010 (BID) +~200mg Sorafenib (QD)~(N=&n2.)" style(column)=[ width=27%  just=c asis=on]  style(header)=[just=c  asis=on];

compute before page / style=[just=l];
/*			line " ";*/
		endcomp;
compute before _PAGE_ / style={just=center borderbottomwidth=1pt bordertopcolor=white};
			
			line "&t1.";
			line "&t2.";
			line "&t3.";
			line " ";
		endcomp;


		compute after col1 / style=[just=l];
			line " ";
		endcomp;

		compute after _PAGE_ / style={just=left bordertopwidth=1pt borderbottomcolor=white};
/*			line " ";*/
			line "&s1.";
			line "&f1.";
			line "&f2.";
			line "&f3.";
			line "&f4.";
			line "&f5.";
		endcomp;

break after  page/page ; 
run;
ods rtf close;
ods listing;
