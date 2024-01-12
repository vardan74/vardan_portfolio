/*Summary of Hematology Shifts from Baseline – Phase I*/

dm 'log;clear;'; 
%include "call setup.sas"; 
%let pgmname=t_I_14_3_4_1_2.sas; 
%let dsname =t_I_14_3_4_1_2;  
dm 'log;clear;'; 
proc delete data = work._all_ ;run ; 
 
proc format;
   invalue avisit
    'Day 7/Cycle 1'=1007
	'Day 14/Cycle 1'=1014
	'Day 28/Cycle 1'=1028
	'Day 56/Cycle 2'=1056
	'Early Termination'=1900;
      
run;

data adsl1;
   set ads.adsl(where=(SAFFL='Y' and POOLN=1));
   run;


data adlb1;
   set ads.adlb(where=(parcat='HEMATOLOGY' and avisit ne 'Unscheduled' and (ABLFL='Y' or PSTBLFL='Y') and SAFFL='Y' and POOLN=1));
run; 
 
proc sort data=adlb1 out=adlb2;
  by TRT01A;
run;

data adlb2_;
  set adlb2;
   where   ABLFL='Y' or PSTBLFL='Y';
run;

data adlb2_;
  set adlb2_;
  if  missing(BNRIND) then BNRIND="NORMAL"; 
  if  missing(ANRIND) then ANRIND="NORMAL";
run;

Proc sql ;
     create table bign as
     select param,TRT01A, count(distinct USUBJID) as bign, strip(TRT01A) || ' (N=' || strip(put(calculated bign,5.)) || ')' as grp 
	   
	    from adlb2_
		group by param,TRT01A;
quit;


/*data bign;*/
/*	length trt01A $200;*/
/*   set bign;*/
/*   output;*/
/*  */
/*   if _N_ =1 then do;*/
/*   */
/*     trt01A='Cohort -1: 100mg MG010 + 200mg sorafenib';*/
/*	 bign=0;*/
/*	 GRP='Cohort -1: 100mg MG010 + 200mg sorafenib (N=0)';*/
/*	 output;*/
/*end;	*/
/**/
/*run;*/

proc freq data=adlb2_ (where=(PSTBLFL='Y'))noprint;
by TRT01A;
   
  tables PARAM*avisitn*avisit*BNRIND*ANRIND/out=shift_cnt(where=(not missing(BNRIND) and not missing(ANRIND))) nopercent nocum list;
run;

proc sort data=shift_cnt; by  PARAM trt01a avisitn avisit;run;

proc sort data=shift_cnt out=dummy(keep= trt01a param) nodupkey; by  trt01a PARAM;run;

data dummy1;
   set dummy;
   length trt01a A BNRIND avisit $200.;
   count=.;
    do trt01a = 'Cohort 1: 200mg MG010 + 200mg sorafenib','Cohort 2: 400mg MG010 + 200mg sorafenib';
      do avisit='Day 7/Cycle 1','Day 14/Cycle 1','Day 28/Cycle 1','Day 56/Cycle 2','Early Termination';
		  do BNRIND='LOW', 'NORMAL', 'HIGH';
		       do A='LOW', 'NORMAL', 'HIGH';
			   
		           output;

		       end;
		   end;
	  end;
 	end;

/* drop ANRIND percent;*/
 rename A=ANRIND;
 run;

 data dummy1;
    set dummy1;
	avisitn=input(avisit,avisit.);
run;



 proc sort data=shift_cnt;by  PARAM trt01a avisitn avisit ANRIND;run;
 proc sort data=dummy1;by  PARAM trt01a avisitn avisit ANRIND;run;


proc sort data=dummy1  nodupkey; by  PARAM trt01a avisitn avisit ANRIND BNRIND ;run;


 proc freq data=shift_cnt noprint;
   
  tables PARAM*trt01a*avisitn*avisit*ANRIND/out=shift_cnt_denom nopercent nocum list;
   WEIGHT count;
 run;

 data shift_cnt1(drop= percent);
    merge dummy1(in=a) shift_cnt(in=b);
	 by  PARAM trt01a avisitn avisit  ANRIND BNRIND;
	 if a;
	 if b and a then point=1;
	    else point=-1;
 run;


data shift1;
merge shift_cnt1(in=a) shift_cnt_denom(rename=(count=denom));
  by  PARAM trt01a avisitn avisit  ANRIND /*avisitn avisit*/;
if a;
run;


proc sort data=shift1;by param TRT01A;run;
proc sort data=bign;by param TRT01A;run;

data shift1_;
	merge shift1 bign(in=a);
	by param TRT01A;
	if a;
run;

data shift1_;
   set shift1_;
   if missing(grp) then grp=strip(TRT01A) || ' (N=0' || ')';
proc sort;
	by param TRT01A avisitn;
run;

data visn;
	set shift1_;
	by param TRT01A avisitn;
	if first.avisitn then visn = count;
	else visn + count;
	if last.avisitn;
	keep param TRT01A avisitn visn;
run;

data shift1_;
	merge shift1_ visn;
	by param TRT01A avisitn;
run;

data shift2(rename=(count1=count denom1=denom));
  set shift1_;
  if point eq 1 and visn ne 0  then do;
	  pct=round(count/visn*100,0.1);
	  if pct eq 100 then do;
		  cnt_pct=strip(put(count,best.)) || ' ' || '(' || strip(put(pct,5.)) || ')';
		  count1=strip(put(count,best.));
		  denom1=strip(put(denom,best.));
	  end;
	  else do;
		  cnt_pct=strip(put(count,best.)) || ' ' || '(' || strip(put(pct,5.1)) || ')';
		  count1=strip(put(count,best.));
		  denom1=strip(put(denom,best.));
	  end;
  end;
  
  drop count denom;
run;

proc sort data=shift2; by PARAM  grp avisitn avisit ANRIND ;run;

proc transpose data=shift2 out=shift3(drop= _NAME_ );
  by PARAM  grp avisitn avisit ANRIND;
  ID BNRIND;
  var CNT_PCT;
run;
/*proc sort data=shift3 nodupkey;by  PARAM  grp avisitn avisit ANRIND;run;*/

/*creating baseline counts*/

data adlb3;
   set adlb2;
   if  missing(BNRIND) then BNRIND="NORMAL"; 
run;

proc freq data=adlb3(where=(ABLFL='Y')) noprint;
by trt01A;
  tables PARAM*BNRIND/out=baseline(where=(not missing(BNRIND))) nopercent nocum list;
  tables PARAM/out=basedenom(rename=(count=basedenom)) nopercent nocum list;
run;
 

data baseline_(rename=(count1=count));
	  length avisit $200.;
   set baseline;
   avisit='Baseline [a]';
   count1=strip(put(count,best.));
   drop count;
run;
proc sort data=baseline_;
  by  PARAM trt01a  avisit;
run;


proc transpose data=baseline_ out=baseline1(drop= _NAME_);
  by PARAM trt01a  avisit ;
  ID BNRIND;
  var count;
run;
proc sort data=baseline1;by param trt01a;run;
proc sort data=basedenom;by param trt01a;run;
/*data baseline1;*/
/*  set baseline1;*/
/*	by param;*/
/*	    if trt01a ne 'Cohort -1: 100mg MG010 + 200mg sorafenib' then do;*/
/*		    output;*/
/*			trt01A='Cohort -1: 100mg MG010 + 200mg sorafenib';*/
/*		    output;*/
/*			point1=99;	*/
/*			call missing(normal,high,low,basedenom);*/
/*		end;*/
/*		else if trt01a ne 'Cohort 2: 400mg MG010 + 200mg sorafenib' then do;*/
/*		    output;*/
/*			trt01A='Cohort 2: 400mg MG010 + 200mg sorafenib';*/
/*		    output;*/
/*			call missing(high,normal,low,basedenom);*/
/*			point1=99;	*/
/*		end;*/
/*		else if trt01a ne 'Cohort 1: 200mg MG010 + 200mg sorafenib' then do;*/
/*		    output;*/
/*			trt01A='Cohort 1: 200mg MG010 + 200mg sorafenib';*/
/*		    output;*/
/*			call missing(high,normal,low,basedenom);*/
/*			point1=99;	*/
/*		end;*/
/*run;*/

proc sort data=baseline1 out=basenodup nodupkey;by param;run;
data dummybase;
	length trt01a $200;
   set basenodup;
   
    by  PARAM ;
    do trt01a = /*'Cohort -1: 100mg MG010 + 200mg sorafenib',*/'Cohort 1: 200mg MG010 + 200mg sorafenib','Cohort 2: 400mg MG010 + 200mg sorafenib';
      output;
 	end;
 run;
	data dummybase;
	  set  dummybase;
	  call missing(low,normal,high);
	run;



proc sort data=baseline1; by param trt01a;run;
proc sort data=dummybase;by param trt01a;run;
data baseline2;
  merge dummybase baseline1(in=a) ;
   by param trt01a;
   if a;
run;


data baseline3;
  merge baseline2(in=a) basedenom(drop=percent);
   by param trt01a;
   if a;
run;

proc sort data=baseline3;by param trt01a;run;
proc sort data=bign;by param trt01a;run;
data baseline4;
 merge bign baseline3(in=a) ;
 by param trt01a;
 if a;
run;



data baseline5;
   set baseline4;
   if not missing(basedenom) then do;
	    if not missing(High) then high_=put(input(High,best5.)/basedenom*100,5.1);
		if not missing(low) then  low_ = put(input(low,best5.)/basedenom*100,5.1);
		if not missing(Normal) then normal_= put(input(Normal,best5.)/basedenom*100,5.1);

   end;
run;

data baseline5;
   set baseline5;
   		if high_='100.0' then high_='100';
		if low_='100.0' then low_='100';
		if normal_='100.0' then normal_='100';
run;

data baseline6;
   set baseline5;
    if not missing(High) then highp=strip(high) || ' (' || strip(high_) || ')';
	if not missing(low) then  lowp=strip(low) || ' (' || strip(low_) || ')';
	if not missing(Normal) then normalp=strip(normal) || ' (' || strip(normal_) || ')';
	drop high low normal high_ low_ normal_ ;
	
run;


data baseline6;
  set baseline6;
  if missing(grp) then grp=strip(TRT01A) || ' (N=0' || ')';
  rename highp=high lowp=low normalp=normal;
run;


/*****************************************************************/

data denom1;
   set shift2(drop=percent bign pct CNT_PCT count trt01a);
    if not missing(denom);
   proc sort ;by param grp avisitn avisit anrind;
run;
proc sort data=shift3; by param grp avisitn avisit anrind;run;

proc sort data=denom1 nodupkey;by param grp avisitn avisit anrind;run;
data shift3_;
  merge shift3(in=a) denom1;
  by param grp avisitn avisit anrind;
  if a ;
run;



  proc sort data=shift3_; by PARAM grp;run;
  proc sort data=baseline6; by PARAM grp;run;

data all;
   set baseline6(in=a) shift3_(in=b);
   by   PARAM grp;
   if a then ord=1;
   if b then ord=2;
run;



proc sort data=all;by param grp;run;


data all1;
   set all;
   length col1-col8 $200.;
    if ord=1  then do;
/*      denom=strip(put(sum(input(NORMAL,??best.),input(HIGH,??best.),input(LOW,??best.)),best.));*/
	 denom=strip(put(basedenom,5.));
      
	end;

   	col1=param;
	col2=grp;
	col3=scan(avisit,1,'/');
	col4=propcase(ANRIND);

	col5=denom;
	col6=LOW;
	col7=NORMAL;
	col8=HIGH;
	    if missing(col6) then col6='-';
		  
		if missing(col7) then col7='-';
		
		if missing(col8) then col8='-';
		
		if missing(col5) then col5='-';

	keep col1-col8 ord   avisitn;
run;

data all1;
  set all1;

  if col5^= "-" or col6^="-" or  col7^= "-" or col8^= "-" then do;
        col5 = tranwrd(col5, "-", "0");
        col6 = tranwrd(col6, "-", "0");
        col7 = tranwrd(col7, "-", "0");
		col8 = tranwrd(col8, "-", "0");
    end;
run;

data all2;
  set all1;
   col2 = tranwrd(col2, "Cohort 1: 200mg MG010 + 200mg sorafenib", "200mg MG010 (QD) + 200mg Sorafenib (QD)");
   col2 = tranwrd(col2, "Cohort 2: 400mg MG010 + 200mg sorafenib", "200mg MG010 (BID) + 200mg Sorafenib (QD)");
   if col4='Low' then ord2=1;
   else if col4='Normal' then ord2=2;
   else if col4='High' then ord2=3;
	if find(col2,'(QD)') then ord1 = 1;
	if find(col2,'(BID)') then ord1 = 2;
run;

proc sort data=all2;by col1 ord1 avisitn ord2;run;

%let count_per_page=7;

data all2_;
	retain pagenum 0 line 0;
	set all2;
	by col1 ord1 avisitn ord2;
	if first.avisitn or first.col1 then line+1;
	if line>&count_per_page  or ord=1 then do;
		
		pagenum+1;
		line=1;
	
	end;

run;


 options papersize=letter orientation=landscape nodate nonumber center missing=" " nobyline; 
ods escapechar="@"; 
ods listing close; 


%mstrtrtf(pgmname=&pgmname, pgmid=1);  


 data tabdat.&dsname.;
	set all2 ;
	keep col1-col8;
 run;

/* proc compare b=tabdat.&dsname. c=v_tabdat.qc_t_I_14_3_4_1_2;*/
/* run;*/

proc report data=all2_ nowd headskip headline split='~'  missing wrap spacing=4 style(header)={verticalalign=middle} style = [outputwidth = 99.99%];
  column col1 col2 pagenum avisitn col3-col5 ('Baseline~_____________________________' col6-col8)  ;
 
  define pagenum/order noprint;
  define avisitn/ order noprint;  
  define col1 / order noprint;

  define col2/ order order = data "Treatment Group" style(column)={width=30% just =l} style(header)={just =left};  
  define col3/ order "Visit" style(column)={width=16% just =l asis=on} style(header)={just =left};
  define col4/ "Post-baseline~ Result Classification" style(column)={width=15% just =l asis=on};
  define col5/ "n" style(column)={width=12% just =c asis=on};
  define col6/ "Low~ n (%)" style(column)={width=7% just =c asis=on};
  define col7/ "Normal~  n (%)" style(column)={width=7% just =c asis=on};
  define col8/ "High~  n (%)" style(column)={width=6% just =c asis=on};

   
 compute before _PAGE_ / style=[just=left];			
			line "(*ESC*)S={just=center borderbottomwidth=1pt bordertopcolor=white}&t1.";
			line " &t2.";
			line " &t3.";

			line " (*ESC*)R/RTF'\par\ql ' Group: " col1 $50.;
			
endcomp;
	
 	compute after _PAGE_ / style={just=left bordertopwidth=1pt borderbottomcolor=white};
			line " ";
			line "&f1.";
			line "&f2.";
			line "&f3.";
		endcomp; 


	compute after avisitn;
			line " ";
		endcomp;

break after pagenum/page ;

run;
ods rtf close;
ods listing;

%chklog_inter;
