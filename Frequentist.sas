/* ---------------------*/
/*Input & Modify dataset*/
/* ---------------------*/
options validvarname=any;
proc import datafile="/home/mil18b0/STA5934 (Long. Analysis)/NepalData.txt" 
out=nepal
dbms=tab
replace;
delimiter=",";
run;

data nepal;
set nepal (rename =
(id = id
" sex"N = sex
" wt"N = weight
" ht"N = height
" arm"N = arm
" bf"N = bf
" day"N = day
" month"N = month
" year"N = year
" mage"N = mage
" lit"N = lit
" died"N = died
" alive"N = alive
" t2"N = t2
" age4"N = age4
" age3"N = age3
" age2"N = age2
" age"N = age
));	
run;

Data want;
Do repeat = 1 to 200;
	Do visit = 1 to 5;
		Output;
	End;
End;
Drop repeat;
Run;

data nepal;
merge nepal want;
if bf=0 then bf_bin = 0;
if bf="." then bf_bin=".";
if bf=1 or bf=2 then bf_bin=1;
if weight=99.9 or weight=88.8 then weight=".";
drop age2 age3 age4 t2;
run;

/* -----------------------------------------------*/
/*Fit general model & Perform exploratory analysis*/
/* -----------------------------------------------*/

proc glm data=nepal plots=residuals;
model weight = visit bf_bin bf_bin*visit age / solution;
output out=residuals
r=yresid;
run;

proc export data=residuals 
			file="/home/mil18b0/STA5934 (Long. Analysis)/Nepal_res.txt"
			dbms=tab;
run;

proc sgplot data=nepal;
vline visit / response=weight stat=mean group=bf_bin lineattrs=(thickness=1) markers datalabel;
run;

proc sgplot data=nepal;
vline visit / response=age stat=mean group=bf_bin lineattrs=(thickness=1) markers datalabel;
run;

proc sgplot data=residuals;
vline visit / response=yresid stat=mean group=bf_bin lineattrs=(thickness=1) markers datalabel;
run;

proc sgpanel NOAUTOLEGEND data=nepal;
panelby bf_bin;
series X=visit Y=weight / group=id LINEATTRS = (THICKNESS=1);
run;
proc sgpanel NOAUTOLEGEND data=residuals;
panelby bf_bin;
series X=visit Y=yresid / group=id LINEATTRS = (THICKNESS=1);
run;

proc sgplot data=nepal;
histogram weight;
run;

/*Plot variogram in R*/


/* -----------*/
/*GEE Analysis*/
/* -----------*/

proc genmod data=nepal plots=all;
class id;
model weight = visit bf_bin bf_bin*visit age / dist=normal;
repeated subject=id / type=ar covb corrw;
run;


/* -----------*/
/*MLE Analysis*/
/* -----------*/

proc glimmix data=nepal plots=residualpanel METHOD=laplace ;
class id;
model weight = visit bf_bin bf_bin*visit age / solution dist=gaussian cl ;
random int / subject=id type=ar(1);
run;

