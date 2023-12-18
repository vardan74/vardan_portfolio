%macro _shift_sum (datain=,
                  subset=,
                  ptid=USUBJID,
                  popds=,
                  popfl=,
                  Trt=,
                  byvar=,
                  var=,
                  value=,
                  baseline=,
                  postbaseline=,
				  basevis=,
				  postvis=,
                  order=,
                  miss=N,
  				  title1=,
          	      title2=,
          		  title3=,
				  title4=,
          	      title5=,
          		  title6=,
            	  ftnote1=,
            	  ftnote2=,
            	  ftnote3=,
            	  ftnote4=,
                  outp=,
                  fext=rtf,
                  debug=y)/minoperator  mindelimiter=',';
/*soh***************************************************************************
MACRO NAME     : _shift_sum.sas
DESCRIPTION    : Creates shift tables 
CALLED MACROS  : %_varexist, %_duplicate,%_vartype, %_obs_count
INPUT          : DATAIN
OUTPUT         : TABLE
ASSUMPTIONS    :
--------------------------------------------------------------------------------

MACRO TEMPORARY OBJECT PREFIX:

PARAMETERS:
  datain=        :(required)analysis dataset which should be summarized.
  subset=        :(optional)filtering condition for the analysis dataset (&datain)
  popds=         :subject level dataset, used for displaying population size.
				  If left blank, analysis dataset will be used.
  ptid=          :(required)variable that is used to merge subject level and analysis 
                  datasets as default
  popfl=         :(required)filtering condition for the subject level dataset (&popds).
  byvar=         :(optional) Only one value of byvar is needed.Example (parcat1,sex).
  Trt=           :(required)this parameter is for calculating the number of participants in the 
				   analysis set in each group.Example (trt01a)
  var=           :(required) this variable is need for calculating baseline and postbaseline numbers
				  of participants for parameter variable.Example (paramn,paramcd)
  value=         : (required) user specify the values of parameter variable
  baseline=      :(required) User specify variable of baseline which must contain in the dataset.
  postbaseline=  :(required) User specify variable of postbaseline which must contain in the dataset.
  basevis=		 :(required) User specify variable fixing first baseline visit (i.e. avisitn=0).	
  postvis=       :(required) User specify variable putting post visits (i.e. avisitn>0).
  order=         :(required) User specify order e.g.(GRADE 0|GRADE 1|GRADE 2| GRADE 3|GRADE 4).
  miss= N        :(optional) the table can display the Missing row and Missing sections 
				  if the option is used.By default the value is assign N.
  outp=          :(required)output path, including the name of the TLF.
  fext= rtf      :(required) extension of the output.
  titleX=        :titles of TLF, where X is a number from 1 to 6
  ftnoteX=       :footnotes of TLFs, where X a number from 1 to 4
  debug=N        :(optional)used for debugging purposes. By default, any dataset created
                  during the call of the macro will be deleted. 
                  Value other than n or N will reverse this action.

  USAGE NOTES:


--------------------------------------------------------------------------------
HISTORY:
Ver# Author            Date    Code History Description
---- ----------------- ------- -------------------------------------------------
1    Vardan Isakhanyan 27dec21 Original version of the code
**eoh**************************************************************************/

/*%include "/GMA_Stat/utility/SOTU/_varexist.sas";*/
/*%include "/GMA_Stat/utility/SOTU/_sotu_pr_tandf.sas";*/
/*%include "/GMA_Stat/utility/SOTU/_duplicate.sas";*/

  %include "X:\BioMetrics\SAS programming\SOTU\macros\prod\_duplicate.sas";
  %include "X:\BioMetrics\SAS programming\SOTU\macros\prod\_varexist.sas";
  %include "X:\BioMetrics\SAS programming\SOTU\macros\prod\_vartype.sas";
  %include "X:\BioMetrics\SAS programming\SOTU\macros\prod\_obs_count.sas";
  %include "X:\BioMetrics\SAS programming\SOTU\macros\prod\_sotu_pr_tandf.sas";
/**/
/*  libname shift "X:\BioMetrics\SAS programming\SOTU\data\2_4_CROSS";*/
  libname shift "X:\BioMetrics\SAS programming\SOTU\data\M15554";
/*  libname shift "X:\BioMetrics\SAS programming\SOTU\data\M12914";*/

%local dsid  byvarn label vis basevisits visval val rc paramn_m baseline_m postbaseline_m &Trt._m avisitn_m byvar_m chk 
       varval textord num ordname exist chkname param_value cnt;

/********Check all requiered variables*************/
 %if "&datain" eq "" or "&ptid" eq "" or "&popds" eq "" or  "&Trt" eq "" or "&fext" eq "" or "&order" eq ""
      or "&basevis" eq "" or "&postvis" eq "" or "&var" eq "" or "&value" eq "" or "&baseline" 
      eq "" or  "&postbaseline" eq ""  %then %do;
      %put ERROR: You must input all required varables;
    %return;
 %end;

/*Remove temporary datasets associated with this macro*/
proc datasets lib=work nolist ;
  delete __be_: ;
run ;
 %let vis=%scan(&basevis,1,'=');

/*get SAS system options of the user to restore at the end*/
proc optsave out = __be_preops ;
run ;

OPTIONS VALIDVARNAME = ANY ;
options nodate nonumber mprint mlogic ;

%if %upcase(&debug) ne Y %then %do ;
   options nomprint nomlogic ;
%end ;
/*********************Checking datasets for availables******************************/


  %_varexist(_ck_datain = &datain,_ck_var=&ptid);
    %if &exist = 0 %then %do;
      %goto exit;
    %end;

  %_varexist(_ck_datain = &datain,_ck_var=Param);
    %if &exist = 0 %then %do;
      %goto exit;
    %end;
  %_varexist(_ck_datain = &datain,_ck_var=avisit);
    %if &exist = 0 %then %do;
      %goto exit;
    %end;

  %_varexist(_ck_datain = &popds,_ck_var=&ptid);
    %if &exist = 0 %then %do;
      %goto exit;
    %end;


  %_duplicate(_ck_dup_datain = &popds, _ck_dup_by =&ptid, _ck_filter=&popfl);
   %if &exist = 0 %then %do;
    %goto exit;
   %end;
    
   

	%if not(%eval(%upcase(&miss) in(Y, N))) %then %do;
		%put ERROR: (&sysmacroname) miss = &miss: miss must be one of the listed values (Y, N), written in upper or lower case;
		%put ERROR: (&sysmacroname) The macro ended abnormally.;
		%goto exit;
	%end;


/*prepare subject level dataset, generate total column if requested, apply numeric format if numeric version of trt variable doesn't exist */
data __be_adsl ;
set &popds ;
 %if &popfl ne %then %do ;
   where &popfl ;
 %end ;
run ;

proc sort  data=__be_adsl;
 by &ptid ;
run ;

/*prepare analysis dataset, generrate total column if requested, apply numeric format if numeric version of trt variable doesn't exist */
data __be_anl0 ;
 set &datain ;
 %if &subset ne %then %do ;
  where &subset ;
 %end ;
run ;

proc sort  data=__be_anl0;
 by &ptid ;
run ;


/**************Merge the subject level and analysis datasets for further processing**********************/
data __be_anl1 ;
  merge __be_anl0 (in=a) __be_adsl (in=b) ;
  by &ptid ;
  if a and b ;
  proc sort ;
  by &ptid;
run ;

data __be_adlb;
  set __be_anl1;
run;


%let dsid = %sysfunc(open(__be_adlb));
%if %sysfunc(vartype(&dsid, %sysfunc(varnum(&dsid, &var.)))) eq C %then %do ;
	  data _null_;
			length z $200 ;
			z = ' ' ;
			x = "&value.";
				do i = 1 to countw(x);
					zz = "'" !! strip(upcase(scan(x, i ))) !! "'";
					z = cat(' ', strip(zz),z) ;
					call symputx('vars',z,'L' ) ;
				end;
		run;
  %end;
  %else %do;
   %let vars=&value;
  %end;
 %put &=vars;
%let rc = %sysfunc(close(&dsid));
/****************************Checking for  variables availables in the dataset***************************/
		/*Checking basevis visit available  in the dataset */
		  %let visval=%scan(&basevis,2,'=');
		    Proc sql noprint;
		      select distinct &vis
			  into: basevisits separated by ', '
			  from __be_adlb;
		    quit;
	        %put &=basevisits;
			%put &=visval;

		  %if  not (&visval in (&basevisits)) %then %do;
		    	%put ERROR: &vis=&visval visit not contain in the dataset,give correct value; 
				%return;
		  %end;

	/*Checking baseline or postline values available in the dataset*/
	 Proc sql noprint;
		select distinct quote(trim(&postbaseline.))
		  into :name separated by ', '
		  from __be_anl1
		  where not missing(&postbaseline.);
     quit;
    %put &=name;
    %do textord=1 %to %sysfunc(countw("&order","|"));
			%let chkname=%sysfunc(strip(%scan("&order",&textord,"|")));
			%if not ("&chkname"  in &name.) %then %do;
			   %put ERROR: The dataset does not contain  &chkname value ,please give correct value;
			   %return;
			%end;
	%end;

      %_varexist(_ck_datain=__be_adlb,_ck_var=&var.);     
         %if &exist = 0 %then %do;
          %goto exit;
         %end;
        
       %_varexist(_ck_datain=__be_adlb,_ck_var=&Trt.);
          %if &exist = 0 %then %do;
           %goto exit;
         %end; 

       %_varexist(_ck_datain=__be_adlb,_ck_var=&baseline.);
         %if &exist = 0 %then %do;
          %goto exit;
         %end;

       %_varexist(_ck_datain=__be_adlb,_ck_var=&postbaseline.); 
         %if &exist = 0 %then %do;
          %goto exit;
         %end;

       %_varexist(_ck_datain=__be_adlb,_ck_var=&vis.); 
         %if &exist = 0 %then %do;
          %goto exit;
         %end;
      
/**********************Checking  byvar parameter values availables.It must contain one value*************/
	%if &byvar. ne %then %do;
		%if %sysfunc(countw(&byvar.," ")) eq 1 %then %do;
			%_varexist(_ck_datain=__be_adlb,_ck_var=&byvar.);
			%if &exist = 0 %then %do;
				%goto exit;
			%end;
		%end;
		%else %do;
			%put ERROR: byvar parameter must contain only one value; 
			%return;
		%end;
	%end;
 
/*Create Dummy dataset inserting rows for those USUBJID which dont have  appropriate visits ,baseline values ,etc*/

proc sort data=__be_adlb(where=(&var. in (&vars))) out= __be_uniq_subj(keep=&ptid &trt &byvar &var param &baseline) nodupkey ;
  by &ptid &trt &byvar &var. param &baseline;
  where not missing(&trt);
run;


proc sql noprint;
  select distinct &vis.
  into :visit separated by ', ' 
  from __be_adlb
  where not missing(&vis.);
quit;
  %put &=visit;

 data __be_usubjid;
  set __be_uniq_subj;
  by &ptid &trt &byvar &var param;
	  do &vis=&visit;
          output;
      end;
 run;

  proc sort data=__be_USUBJID;by &trt &ptid  &byvar &var &vis &baseline ;run;
  proc sort data= __be_adlb;by &trt &ptid  &byvar &var &vis &baseline;run;

  data __be_union;
   merge __be_adlb(in=a) __be_usubjid(in=b);
   by  &trt &ptid  &byvar &var &vis &baseline ;
   if b;
  run; 
    
 data __be_adlb;
   set __be_union;
 run;
    /*Checking  whether byvar variable have numeric equivalent for further sorting apply */
	%if &byvar. ne %then %do;
         %let dsid = %sysfunc(open(__be_adlb));
	     %let byval = %sysfunc(varnum(&dsid,&byvar.n));
	     %let rc = %sysfunc(close(&dsid));
         %if &byval > 0 %then %do;
            %let byvarn=Y;
         %end; 
		 %else %do;
		    %let byvarn=N;
		 %end;
     %end;
/*************************************Checking input variables for character type*******************************************/
    %_vartype(_ck_datain=__be_adlb, _ck_var=Trt, _ck_type= C);
      %if &exist = 0 %then %do;
         %put ERROR- &Trt. variable must be character type; 
         %goto exit;
      %end;

/*Checking type of variable.If variable have character type and there is equivalent of numeric type in the dataset*/ 
 /*so need to take and use numeric type of variable,otherwise need to create new numeric type of variable for further using*/

     %_vartype(_ck_datain=__be_adlb, _ck_var=var);
      %if &exist = C %then %do;

		 %let dsid = %sysfunc(open(__be_adlb));
	     %let val = %sysfunc(varnum(&dsid,&var.n));
	     %let rc = %sysfunc(close(&dsid));
         %if &val > 0 %then %do;
           %let var=&var.n; 
         %end; 
         %else %do;
           proc sql;
             create table __be_num_version as
               select distinct &var., 1 as &var._num 
                 from __be_adlb
                 where not missing(&var.)
                 order by &var.;
           quit;

          data __be_num_version;
             set __be_num_version;
             &var._n+&var._num;
             drop &var._num;
           run;
			
          
          proc sort  data=__be_adlb;by &var.;run;
          proc sort data=__be_num_version;by &var.;run;

          data __be_adlb;
            merge __be_adlb(in=ind_adlb) __be_num_version(in=num_v);
            by &var.;
            if ind_adlb and num_v;
          run;
          
			%let oldvalue=&value;
            
           proc sql noprint;
             select &var._n, &var.
               into: param_value separated by ',' , :ch_param_value separated by ', ' 
               from __be_num_version
               where &var. in (&vars);
           quit;
           %let value=%quote(&param_value);
           %let var=&var._n;
         %end;

    %put &=param_value;
	%put &=ch_param_value;
    
  /***************************************Checking param values in the dataset*********************************/ 

	%do c1=1 %to %sysfunc(countw(&oldvalue,","));
	    %let nm=%scan(&oldvalue,&c1,",");

		 %put &=nm;
		  %if  not (&nm in (&ch_param_value)) %then %do;
		    %put ERROR: &nm value need to spelling and corret while it is not compatible in the dataset values;
	  		%return;
		  %end;

	%end;
%end;       	  
    proc sql noprint;
      select distinct(&var)
       into: varval separated by ','
       from __be_adlb;
    quit;
   %put &=varval &=value;
   %do z=1 %to %sysfunc(countw(&value));
      %let chk=%scan(&value,&z,",");
	  %put &=chk;
       %if not (&chk in (&varval.)) %then %do;
        %put ERROR: &chk value is not correct;
        %return;
       %end;
   %end;

/********Calculating subjects counts for summarize for bign counts by TRT, baseline counts,postbaseline counts*********/

Proc sql noprint ;
    select &Trt,count (distinct &ptid) as N
    into :trt1-, :bign1-
    from &popds
     where not missing(&Trt.)  %if &popfl ne %then %do ;
       and &popfl %end ;
     group by &Trt;
     

  %if %upcase(&miss)=Y %then %do;
     create table __be_basecount as 
      select &Trt, %if &byvar. ne %then %do; &byvar.,%end; &var,&baseline,count(distinct &ptid) as N1
      from __be_adlb
      where not missing(&Trt.) and &var in (&value.) and &basevis
       group by &Trt, 
		%if &byvar. ne %then %do; 
           &byvar.,
        %end; &var,&baseline ;
  %end;
  %else %if %upcase(&miss)=N %then %do;
	   Proc sql noprint ;
		create table __be_basecount1 as 
	      select &Trt,%if &byvar. ne %then %do; &byvar.,%end; &var., &vis., &baseline.,&postbaseline., count(distinct &ptid) as n1
	         from __be_adlb
	         where not missing(&Trt) and  &var. in (&value.) 
	         group by &Trt,&var.,%if &byvar. ne %then %do; &byvar.,%end; &vis., &baseline.,&postbaseline.;
	   quit;


	data __be_basecount11;
		set __be_basecount1(where=(&postbaseline. ne ''));
	run;

	proc freq data=__be_basecount11 noprint;
	  tables &Trt* &var* %if &byvar. ne %then %do; &byvar.* %end; &vis.* &baseline./ out=__be_basecount(rename=(count=N1)) ;
	  weight n1;
	run;
  %end;
    
quit;

proc sql noprint;
         create table __be_postcount as 
          select &Trt,%if &byvar. ne %then %do; &byvar.,%end; &var,&vis,&baseline,&postbaseline,count(distinct &ptid) as n
          from __be_adlb
          where not missing(&Trt.) and  &var in (&value.) and  &postvis 
         %if  %upcase(&miss) ne Y %then %do; and not missing(&postbaseline) and not missing(&baseline)%end;
         group by &Trt, %if &byvar. ne %then %do; &byvar.,%end; &var,&vis,&baseline,&postbaseline ;
quit;
/*****************************Checking for observations counts after applying filters***********************************/
   %_obs_count(_ck_obs_datain =__be_basecount);
       %if &exist = 0 %then %do;
	   %put ERROR- for selected parameters there are not observations.;
         %goto exit;
      %end;


/********Calculating subjects counts for summarize for bign counts by TRT, baseline counts,postbaseline counts*/ 
 /*taking into account missing values***********************************************************/
  data __be_basecount;
     length &baseline.1 $200.;
      set __be_basecount;
    
      if &baseline.='' then &baseline.1='Missing';
        else &baseline.1=&baseline.;
      drop &baseline.;
      rename &baseline.1=&baseline.;
     proc sort;by &byvar. &Trt. &var. %if %upcase(&miss)=N %then %do; &vis.%end; &baseline.;
  run;

	data __be_postcount;
		length &postbaseline.1 &baseline.1 $200.;
		set __be_postcount;

		if &baseline.='' then &baseline.1='Missing';
		else &baseline.1=&baseline.;

		if &postbaseline.='' then &postbaseline.1='Missing';
		else &postbaseline.1=&postbaseline.;

		drop &postbaseline. &baseline.;
		rename &postbaseline.1=&postbaseline. &baseline.1=&baseline.;

		proc sort;by &byvar. &Trt. &var. %if %upcase(&miss)=N %then %do; &vis. %end;  &baseline.;
	run;

/************************Merging  baseline and postbaseline  datasets************************/
  data __be_baseandpost;
   merge __be_basecount(in=a ) __be_postcount(in=b );
   by &byvar. &trt. &var. %if %upcase(&miss)=N %then %do; &vis. %end; &baseline.;
   if b ;
 run;



/**************************Converting numeric counts to character type***************************/
 data __be_baseandpost1;
     set __be_baseandpost;
     length count $200.;
     count=strip((put(n ,5.0))|| " (" || strip(put(round(n/N1*100,0.1),5.1)) || ")");
     keep &byvar. &Trt. &var. &vis. &baseline. &postbaseline. count;
  run;

  proc sort data=__be_baseandpost1;
    by &byvar. &Trt. &var. &vis. &baseline.;
  run;

  /****Creating unique values by baseline,TRT,visits,parameters,byvar in the separate datasets****/
 proc sort data=__be_anl1 out=__be_&baseline._unic(keep=&baseline) nodupkey;
    by &baseline;
    %if %upcase(&miss) ne Y %then %do; where &baseline ne " "; %end; 
 run;

 proc sort data=__be_anl1 out=__be_&Trt._unic(keep=&Trt) nodupkey;
   where not missing(&Trt.);
   by &Trt;
 run;

 proc sort data=__be_anl1(where=(not missing(avisit))) out=__be_visits_unic(keep=&vis avisit) nodupkey;
   by &vis;
 run;

 proc sort data=__be_adlb out=__be_params_unic(keep=&var param) nodupkey;
   by &var;
 run;
/********************Creating baseord  variable for byvar ordering*******************************/
 %if &byvar. ne %then %do;
    proc sort data=__be_anl1 out=__be_&byvar._unic(keep=&byvar) nodupkey;
     by &byvar;
    run;

    data __be_&byvar._unic;
     length &byvar. $200.;
     set __be_&byvar._unic;
     baseord=-10;
    run;
 %end;
/********************Creating groupord  variable for TRT ordering*******************************/
  data __be_&Trt._unic;
   set __be_&Trt._unic;
   by &Trt; 
   groupord+1;
  run;
/*****Assigning 'Missing' if there are missing values in the __be_&baseline._unic dataset********/
data __be_&baseline._unic;
  set __be_&baseline._unic;
  if &baseline. eq '' then &baseline.='Missing';
run;
/*******Creating macro variables for unique visits,for unique TRT,for unique parameter names*****/
/****for unique baseline,for unique byvar values************************************************/

%if &byvar. ne %then %do;
	proc sort data=__be_adlb out=__be_test nodupkey;
		by &byvar.;
	run;
%end;
proc sql noprint;
    select distinct &baseline
      into :&baseline._M separated by ', '
      from __be_&baseline._unic;
 
    select distinct &Trt
      into :&Trt._M separated by ', '
      from __be_adlb
      where not missing(&Trt.) ;

    select distinct &vis
       into :avisitn_M separated by ', '
       from __be_adlb
       where not missing(&vis);
  
    select distinct &var
      into :paramn_M separated by ', '
      from __be_adlb
      where not missing(&var.);



	%if &byvar. ne %then %do;
		select &byvar 
			into :&byvar._M separated by '** '
			from __be_test
			where not missing(&byvar);
		%if &byvarn. eq Y %then %do;
			select &byvar.n 
				into :byvarn_M separated by '** '
				from __be_test
				where not missing(&byvar.n);
		%end;
	%end;
  
 
    select distinct (&var),count(distinct &var)
      into : chkval separated by ' ', :chkcnt trimmed
    from __be_baseandpost1;

quit;

%if &byvar. ne %then %do;
	Proc sql noprint;
	  select distinct (&var),&byvar
	    into :var_value1-, :name_byvar1-
	    from __be_baseandpost1;
	quit;
%end;
%else %do;
  Proc sql noprint;
	  select distinct (&var)
	    into :var_value1-
	    from __be_baseandpost1;
	quit;
%end;
%let cnt=&sqlobs;


/*******Creating order for postbaseline values and checking values in order parameter************/  
/****for exact accordance  in the dataset********************************************************/
  
	proc format;
       invalue postlineord
		%do textord=1 %to %sysfunc(countw("&order","|"));
			%let chkname=%sysfunc(strip(%scan("&order",&textord,"|")));
			"&chkname"= %sysevalf(0.5+ &textord *0.01)
		%end;
		;
	run;
  
%put &=avisitn_M;

/*************************Create dummy dataset including visits*************************************/
data __be_dummy;
  length  %if &byvar. ne %then %do; &byvar. $200. %end; count $200. &baseline. $200. &postbaseline. $200.;
  %if &byvar. ne %then %do;
    do k=1 to countw("&&&byvar._M","**");
     &byvar= strip(scan("&&&byvar._M",k,"**"));
	 %if &byvarn eq Y %then %do; 
	 	&byvar.n= strip(scan("&byvarn_M",k,"**"));
	 %end;
  %end;
     do a=1 to countw("&&&Trt._M",",");
      do &Trt.=strip(scan("&&&Trt._M",a,","));
        do &var=&paramn_M;
          do &vis=&avisitn_M;
            do i=1 to countw("&&&baseline._M",",");
              &baseline= strip(scan("&&&baseline._M",i,","));
              do j=1 to countw("&&&baseline._M",",");
                   &postbaseline=strip(scan("&&&baseline._M",j,","));
                   count='0';
                   output;
              end;
            end;
          end;
        end;
      end;
    end;
  %if &byvar. ne %then %do;
   end;
  %end;
   drop i j  a;
run;

proc sort data=__be_dummy out=__be_basedummy nodupkey;
  by &byvar &Trt &var &vis &baseline;
run;
/***********Assigning values in ordering variables************/
data __be_dummy1;
   set __be_basedummy __be_dummy;
   by &byvar &Trt &var &vis &baseline;
    baseord=1; 
   if first.&baseline then  baseord=0.1; 
   if baseord=0.1 then &postbaseline='';
     if &baseline="Missing" then &baseline._ord=2;
		 else if &baseline='' then &baseline._ord=0;
		   else &baseline._ord=1;
run;
/*********Filtering dummy dataset with existing values**********/
data __be_dummy2;
  set __be_dummy1;
  where %do t=1 %to &cnt;
		  (&var= &&var_value&t 
	        %if &byvar. ne %then %do;
             and &byvar="&&name_byvar&t")  
			%end; 
			%else %do;
			  )
			%end;
			%if &t ne &cnt %then %do;
			or
			%end;
  		%end;;
run;

proc sort data=__be_dummy2 out=__be_unic_visit_param nodupkey;
    by &byvar &Trt &var &vis;
run;

proc sort data=__be_dummy2;
 by &byvar &Trt &var &vis &baseline._ord &baseline;
run; 

data __be_dummy3;
   set __be_unic_visit_param  __be_dummy2;

   by &byvar &Trt &var &vis;
   if first.&vis then do;
      &baseline='';
      baseord=0;
   end;
   if &postbaseline='Missing' then baseord=2;
run;

proc sort data=__be_dummy3;by &Trt;run;
data __be_dummy3;
   merge   __be_dummy3(in=in1) __be_&Trt._unic;
   by &Trt;
   if in1;
run;

data __be_dummy4;
   set %if &byvar. ne %then %do; __be_&byvar._unic %end; __be_dummy3;
   %if &byvar. ne %then %do; proc sort;by &byvar.%end;;
run;

proc sort data=__be_dummy4;
 by &byvar &Trt &var &vis  &baseline;
run;
/*******************************************************************/
data __be_basecount;
  set __be_basecount;
  length count1 $200.;
   count1=strip(put(N1,best.));
   baseord=0.1;
  drop N1;
  proc sort;by &byvar &Trt &var  baseord &baseline;
run;

proc sort data=work.__be_baseandpost1;by &byvar &Trt &var &vis &baseline &postbaseline;run;


/*******Merging dummy dataset with statistical dataset called __be_baseandpost1***************/
data __be_combine;
  merge __be_dummy4(in=ind) __be_baseandpost1;
  by &byvar &Trt &var &vis &baseline &postbaseline;
  if ind ;
  proc sort;by &byvar &Trt &var &vis ;
run;

data __be_combine1;
  set __be_combine(where=(&var. in (&chkval.)));
  by &byvar &Trt &var &vis ;
  if first.&vis then do;
    &baseline='';
    &postbaseline='';
  end;
run;

proc sort data=__be_combine1;
  by  &byvar &Trt &var  baseord &baseline;
run;

data __be_combine2;
  merge __be_combine1(in=ind1) __be_basecount;
  by  &byvar &Trt &var  baseord &baseline;
  if ind1;
  proc sort;by &byvar &Trt &var &vis baseord &baseline &postbaseline ;
run;

 data __be_combine2;
   set  __be_combine2;
   if not missing(count1) then do;
     count=count1;
   end;
 run;
/**********Creating formats from dataset for visits and params***********/

data __be_visits_unic_format (keep=start label fmtname type);
   length  label $200 ;
   set __be_visits_unic ;
   start = &vis;
   label = avisit;
   fmtname = 'Cvisit' ;
   type = 'N' ;
  output ;

  proc sort ;
  by fmtname start label ;
run ;

proc format  cntlin= __be_visits_unic_format ;
run ;

data __be_params_unic_format (keep=start label fmtname type);
  length  label $200 ;
  set __be_params_unic ;
   start = &var;
   label = param;
   fmtname = 'Cparam' ;
   type = 'N' ;
  output ;

  proc sort ;
  by fmtname start label ;
run ;

proc format  cntlin= __be_params_unic_format ;
run ;
/***************************************************************/
proc sort data=__be_combine2;
 by  &byvar %if &byvarn eq Y %then %do; &byvar.n %end; &var &vis &baseline._ord &baseline &postbaseline baseord;
run;

proc transpose data=__be_combine2 out=__be_tr_comb(drop=_NAME_) let prefix= G_;
  by  &byvar %if &byvarn eq Y %then %do; &byvar.n %end; &var &vis &baseline._ord &baseline &postbaseline baseord;
  id  groupord;
  var count;
run;

data __be_tr_combine;
 set __be_tr_comb;
		baselineord=input(&baseline.,??postlineord.);
	 if not missing(&baseline._ord) then baselineord1=&baseline._ord+baselineord;
	 if &baseline="Missing" then baselineord1=&baseline._ord;
	 drop baselineord;

run;
proc sort data=__be_tr_combine;by %if &byvarn eq Y %then %do; &byvar.n %end; &byvar &var &vis &baseline._ord baselineord1 &baseline baseord;run;

data __be_tr_combine;
  set __be_tr_combine;
  by %if &byvarn eq Y %then %do; &byvar.n %end; &byvar &var &vis &baseline._ord baselineord1 &baseline baseord ;
  if last.&baseline. and baseord ^=0 then section=last.&baseline.; /*This condition expression need for appearence byvar value in the middle of page*/
  if &postvis.;
run;


data __be_tr_combine;
  set __be_tr_combine;  
  pg=ceil(_n_/20);/*Assigning number of rows*/
run;

data __be_tr_combine1(rename=(value=pg));
   set __be_tr_combine;

   by  %if &byvarn eq Y %then %do;&byvar.n %end;&byvar &var &vis &baseline._ord baselineord1 &baseline baseord  ;
     retain value 1 ;
      if baseord=0 then pg+1;
      if baseord=0.1 then do;
        value=pg;
      end;
     
   if not missing(&postbaseline.) and &postbaseline. ne "Missing" then do;
     baseord=input(&postbaseline.,postlineord.);
   end;
     baselineord=input(&baseline.,??postlineord.);
	 if not missing(&baseline._ord) then baselineord1=&baseline._ord+baselineord;
	 if &baseline="Missing" then baselineord1=&baseline._ord;
   drop pg baselineord;
run;

proc sort data = __be_tr_combine1 out = __be_tr_combine2; 
   by  %if &byvarn eq Y %then %do; &byvar.n %end; &byvar &var &vis pg;
run;

data __be_tr_combine2;
  set __be_tr_combine2;
  by  %if &byvarn eq Y %then %do;&byvar.n %end; &byvar &var &vis pg;
  if baseord ne -10  then do;
    if baseord=0 and last.pg then pg + 1; /*Assigning values to pg variable for breaking pages in proc report*/
  end;
run;

proc sort data = __be_tr_combine2 out = __be_tr_combine3; 
   by   pg;
run;

data __be_tr_combine3a;
 length Param_Name $200.;
  set __be_tr_combine3;
  by  pg;
  params=put(&var, Cparam.);
  if baseord eq -10 and last.pg and _N_ ne 1 then do;
     pg + 1;
  end;
  if first.pg  then  Param_Name='    ' || params;
run;

proc sort data=__be_tr_combine3a;by pg %if &byvarn eq Y %then %do; &byvar.n %end; &byvar &var &vis &baseline._ord baselineord1 &baseline baseord;run;

/*********Creating dataset with byvar values for inserting its into main dataset***********/
  %if &byvar ne %then %do;
		data __be_&byvar;
			set __be_tr_combine3a;
	   		 if baseord ne -10 and not missing(Param_Name);
		    baseord=-10;
			array grp {*} G_:;
			do i=1 to dim(grp);
	     		grp{i}='';
	    	end;
			drop i;
	    run;

	    data __be_tr_combine3;
	     set  __be_tr_combine3a  __be_&byvar;
	    run;
	  proc sort data=__be_tr_combine3;by pg %if &byvarn eq Y %then %do; &byvar.n %end; &byvar &var &vis &baseline._ord baselineord1 &baseline baseord;run;

        data __be_tr_combine3;
	      set __be_tr_combine3;
		  by pg %if &byvarn eq Y %then %do; &byvar.n %end; &byvar &var;
		      output;
		    if _N_ ne 1 and first.&byvar  and ^first.pg and ^last.pg  then do;
	           baseord=-10; output;
	        end;

			/*insert visits in the first of pages*/
			if _N_ ne 1 and first.pg and BTOXGR ne "" then do;
	          baseord=0;output;
			end;
	    run;

  %end; 
  %else %do;
	  data __be_tr_combine3_;
	    set __be_tr_combine3a;
		by pg %if &byvarn eq Y %then %do; &byvar.n %end; &byvar &var;
		      output;
		    
			/*insert visits in the first of pages*/
			if _N_ ne 1 and first.pg and baseord ne 0 then do;
	          baseord=0;output;
			end;
	  
	  run;

	  proc sort data=__be_tr_combine3_;by pg  &byvar &var &vis &baseline._ord baselineord1 baseord ;run;  

	  /*If no By-Variable used in the macro and only one parameter found in the dataset, need to remove the first column in outputs*/
		Proc sql noprint;
		  select count(distinct(&var))
		    into : num trimmed
		    from __be_tr_combine3_;
		quit;
		%put &=num;
  %end;


data __be_Final;
  length  Name visits params $200.;
 set %if &byvar eq %then %do;
      __be_tr_combine3_ %end; 
     %else %do;
       __be_tr_combine3 
     %end;;
  by pg %if &byvarn eq Y %then %do;&byvar.n %end; &byvar &var;
  array group {*} G_: &byvar. ;
   visits=put(&vis, CVISIT.);

    if first.&var then Param_Name='    ' || params;
    if baseord=0.1 then do;
      Name='    '||strip(&baseline.) ||', N1';
	 
    end;     
        %if &byvar. ne %then %do; 
			else if baseord=-10  then Param_Name=&byvar. ;
			 
		%end;
         else   Name='        '|| strip(&postbaseline.);
    if /*&baseline eq ''*/baseord=0 then do;
      Name= visits;
     do i=1 to dim(group);
      group{i}='';
     end;
    end;
/*	%if &byvar. ne %then %do; */
	   if baseord=0.1 then call missing(Param_name);
/*	%end;*/
/********** Creating rows between sections in the dataset for separating purpose **********/
  if baseord=-10  then do;
     name='';
  end;
 %if %UPCASE(&miss) = Y %then %do;
   output;
	  if baseord=2 then do;
        Name='';
	     do i=1 to dim(group);
	      group{i}='';
	     end;
        output;
      end;
  %end;
  %else %do;
   output;
	  if section=1 then do;
        Name='';section=.;baseord=0.99;
	     do i=1 to dim(group);
	      group{i}='';
	     end;
     	output;
      end;
  %end;
  drop i visits params &baseline &postbaseline &byvar.;
run;

/*If no byvar and param_Name have one value then droping byvar column from the Final dataset*/
%if &byvar. eq and &num=1 %then %do;
 data __be_Final(drop=Param_Name);
   set __be_Final;
%end;
proc sort data=__be_Final;by pg %if &byvarn eq Y %then %do; &byvar.n %end; &var &vis &baseline._ord baselineord1 baseord ;run;

proc contents data=__be_Final out=__be_cont(keep=name) noprint;
run;

/*********Creating grp macro variable which contains G_1, G_2, G_3 variables and etc.************/
proc sql noprint;
  select name  into: grp separated by ' '
  from __be_cont
  where name like "G_%";
quit;

options ORIENTATION=landscape;
options nodate nobyline nonumber;
ods escapechar="^";
ods listing close;

  title ;
  footnote ;
%if %upcase(&fext) = RTF %then %do;
   ods &fext. file = "&outp..&fext" style = report;
 %end;
  %else %if %upcase(&fext) = PDF %then %do;
   ods &fext. file = "&outp..&fext" style = report uniform bookmarkgen=no;
  %end;

/*********Creating macro variable called label for putting the value in Proc Report**********/
		%if &byvar ne %then %do;
	        data _null_;
			   set __be_anl1 (obs=1);
			   labeltext= vlabel(&byvar);
			   call symputx('label',labeltext,'L');
			run;
		%end;
		%else %do;
		   %let label=%str();
		%end;
		
 
 proc report data = __be_final   nowd spacing=3 center headline headskip   split = "~" formchar(2) = "_" missing style = [just=c outputwidth=99.999%]
   style(header)={topmargin=10 bottommargin=10 asis=on};

    column   pg  %if &byvar. ne or &num. gt 1 %then %do; Param_Name %end;  Name &var. &vis baselineord1 baseord &grp;
    
    define pg/ order order=data noprint;
    
    define &var./     noprint;
    define &vis/      noprint;
	define baselineord1/   noprint;
    define baseord/   noprint; 
    %if &byvar. ne or &num. gt 1 %then %do; 

      define Param_Name/  display style(header)={just=left}  "&label~    Parameter (unit)"  style(column)={width=15% just =left asis=on} id; 
     %end;
      define Name / display  style(header)={just=left} "Analysis Visits~    Baseline Value~        Postbaseline Value" style(column)={width=18% just =left asis=on} id;
    
     %do w=1 %to %sysfunc(countw(&grp.));
      define %scan(&grp.,&w) / display "&&trt&w ~(N = &&bign&w) ~ n(%)" style(column)={width=%eval(66/%sysfunc(countw(&grp.)))% just =c} %if &w=3 %then %do; page %end;; /*Putting values from grp macro variable into define statement using loop*/
     %end;

   break after pg/page;

   compute after pg;
    line " ";
   endcomp;
   compute before pg;
    line " ";
   endcomp;
   

    %local _sotu_pr_tandf_problem;
         %_sotu_pr_tandf;

    %if &_sotu_pr_tandf_problem = Y %then %do;
          %let syscc = 100 ;
          %goto exit ;
    %end;
 run;

 ods &fext. close;
 ods listing;

 %exit:	
   title ;
   footnote ;
%mend _shift_sum ;
