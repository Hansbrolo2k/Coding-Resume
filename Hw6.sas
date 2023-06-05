/*This code from Advanced biostatistics demonstrates my ability to conduct parametric and non-parametric 
hypothesis tests with computation of exact p-values*/

*Computes a Monte Carlo randomization p-value.  The data for this example is the DCCT data from Table 6.6 in Rosenberger and Lachin;
data dcct;
 input id chol tx;
cards;
1 1 1
2 2 0
3 3 0
4 4 0
5 5 1
6 6 1
7 7 1
8 8 1
9 9 1
10 10 1
;
proc npar1way wilcoxon;
 var chol;
 class tx;
run;
*Here I am computing the Wilcoxon test based on the data;
proc rank out=ranked;
 ranks simrank;
 var chol;
proc means data=ranked noprint;
 var simrank;
 output out=b mean=meanrank n=numobs;
data two;
 set ranked;
 if _N_=1 then set b;
 cenrank=simrank-meanrank;
 keep cenrank tx;
proc iml;
 use two;
 read all var {cenrank tx};
 close two;
 sobs=cenrank`*tx; *This is the observed Wilcoxon statistic;
 print sobs;
 numobs=10; *Total sample size;
 t=1:numobs;
 m=20000; *Number of Monte Carlo sequences generated;
 counter=0;
 do j=1 to m;
*/Here is the randomization procedure.  In this example it is BCD p=2/3 */;
     na=0; nb=0; suma=0; sumb=0;
  do i=1 to numobs;
   x=ranuni(26398);
   if na=nb then phi=1/2;
   if na<nb then phi=2/3;
   if na>nb then phi=1/3;
   if x < phi then do;
   	t[i]=1;
    na=na+1;
	*suma=suma+chol[i]; *Here I am adding the cholesterol values on tx A;
   end;
   else do;
   t[i]=0;
    nb=nb+1;
	*sumb=sumb+chol[i]; *Here I am adding the cholesterol values on tx B;	
   end;
  *This is the end of the randomization procedure;

   *Here we do not put an OUTPUT statement because we do not want to output all 100,000 statistics;
  end;
  s=cenrank`*t`; *This is the Wilcoxon test for the generated sequence;
  if abs(s)>=abs(sobs) then counter=counter+1; *Increment the counter if the test statistic is equal to or exeeds the observed statistic;
 end;
  p=counter/m; *p-value is the proportion of generated sequences for which the test statistic >= to observed;
  print p;
 quit;
 
 
 
/*t-test */
*Computes a Monte Carlo randomization p-value.  The data for this example is the DCCT data from Table 6.6 in Rosenberger and Lachin;
data dcct;
 input id chol tx;
cards;
1 1 1
2 2 0
3 3 0
4 4 0
5 5 1
6 6 1
7 7 1
8 8 1
9 9 1
10 10 1
;
proc ttest;
 var chol;
 class tx;
run;
*Here I am computing the difference in means (analog: t-test) based on the data;
proc sort data=dcct;
 by tx;
 run;
proc means data=dcct noprint;
 var chol;
 by tx;
 output out=b mean=mean;
data newb;
 set b;
 if tx=0 then mean=(-1)*mean; 
proc means data=newb noprint;
 var mean;
 output out=c sum=sum;
data three;
 merge dcct c;
 keep chol tx sum;
proc iml;
 use three;
 read all var {chol tx sum};
 close three;
 sobs=sum[1]; *This is the observed difference in means;
 print sobs;
 numobs=10; *Total sample size;
 m=20000;  *Number of Monte Carlo sequences generated;
 counter=0;
 do j=1 to m;
*Here is the randomization procedure.  In this example it is Efron's BCD with p=2/3;
  na=0; nb=0; suma=0; sumb=0;
  do i=1 to numobs;
   x=ranuni(26398);
   if na=nb then phi=1/2;
   if na<nb then phi=2/3;
   if na>nb then phi=1/3;
   if x < phi then do;
    na=na+1;
	suma=suma+chol[i]; *Here I am adding the cholesterol values on tx A;
   end;
   else do;
    nb=nb+1;
	sumb=sumb+chol[i]; *Here I am adding the cholesterol values on tx B;	
   end;
  end;
  *This is the end of the randomization procedure;
  s=(suma/na)-(sumb/nb); *This is the difference in means for the generated sequence;
  if abs(s)>=abs(sobs) then counter=counter+1; *Increment the counter if the test statistic is equal to or exeeds the observed statistic;
*Here we do not put an OUTPUT statement because we do not want to output all 100,000 statistics;
 end;
  p=counter/m; *p-value is the proportion of generated sequences for which the test statistic >= to observed;
  print p;
 quit;
 
 
 *Computes a Monte Carlo randomization p-value.  The data for this example is the DCCT data from Table 6.6 in Rosenberger and Lachin;
data dcct;
 input id chol tx;
cards;
1 1 1
2 2 0
3 3 0
4 4 0
5 5 1
6 6 1
7 7 1
8 8 1
9 9 1
10 10 1
;
proc npar1way wilcoxon;
 var chol;
 class tx;
run;
*Here I am computing the Wilcoxon test based on the data;
proc rank out=ranked;
 ranks simrank;
 var chol;
proc means data=ranked noprint;
 var simrank;
 output out=b mean=meanrank n=numobs;
data two;
 set ranked;
 if _N_=1 then set b;
 cenrank=simrank-meanrank;
 keep cenrank tx;
proc iml;
 use two;
 read all var {cenrank tx};
 close two;
 sobs=cenrank`*tx; *This is the observed Wilcoxon statistic;
 print sobs;
 numobs=10; *Total sample size;
 t=1:numobs;
 m=20000; *Number of Monte Carlo sequences generated;
 counter=0;
 do j=1 to m;
*/Here is the randomization procedure.  In this example it is BCD p=2/3 */;
     na=0; nb=0; suma=0; sumb=0;
  do i=1 to numobs;
   x=ranuni(26398);
   if na=nb then phi=1/2;
   if na<nb then phi=1/2;
   if na>nb then phi=1/2;
   if x < phi then do;
   	t[i]=1;
    na=na+1;
	*suma=suma+chol[i]; *Here I am adding the cholesterol values on tx A;
   end;
   else do;
   t[i]=0;
    nb=nb+1;
	*sumb=sumb+chol[i]; *Here I am adding the cholesterol values on tx B;	
   end;
  *This is the end of the randomization procedure;

   *Here we do not put an OUTPUT statement because we do not want to output all 100,000 statistics;
  end;
  s=cenrank`*t`; *This is the Wilcoxon test for the generated sequence;
  if abs(s)>=abs(sobs) then counter=counter+1; *Increment the counter if the test statistic is equal to or exeeds the observed statistic;
 end;
  p=counter/m; *p-value is the proportion of generated sequences for which the test statistic >= to observed;
  print p;
 quit;
 
 
 
/*t-test */
*Computes a Monte Carlo randomization p-value.  The data for this example is the DCCT data from Table 6.6 in Rosenberger and Lachin;
data dcct;
 input id chol tx;
cards;
1 1 1
2 2 0
3 3 0
4 4 0
5 5 1
6 6 1
7 7 1
8 8 1
9 9 1
10 10 1
;
proc ttest;
 var chol;
 class tx;
run;
*Here I am computing the difference in means (analog: t-test) based on the data;
proc sort data=dcct;
 by tx;
 run;
proc means data=dcct noprint;
 var chol;
 by tx;
 output out=b mean=mean;
data newb;
 set b;
 if tx=0 then mean=(-1)*mean; 
proc means data=newb noprint;
 var mean;
 output out=c sum=sum;
data three;
 merge dcct c;
 keep chol tx sum;
proc iml;
 use three;
 read all var {chol tx sum};
 close three;
 sobs=sum[1]; *This is the observed difference in means;
 print sobs;
 numobs=10; *Total sample size;
 m=20000;  *Number of Monte Carlo sequences generated;
 counter=0;
 do j=1 to m;
*Here is the randomization procedure.  In this example it is Efron's BCD with p=2/3;
  na=0; nb=0; suma=0; sumb=0;
  do i=1 to numobs;
   x=ranuni(26398);
   if na=nb then phi=1/2;
   if na<nb then phi=1/2;
   if na>nb then phi=1/2;
   if x < phi then do;
    na=na+1;
	suma=suma+chol[i]; *Here I am adding the cholesterol values on tx A;
   end;
   else do;
    nb=nb+1;
	sumb=sumb+chol[i]; *Here I am adding the cholesterol values on tx B;	
   end;
  end;
  *This is the end of the randomization procedure;
  s=(suma/na)-(sumb/nb); *This is the difference in means for the generated sequence;
  if abs(s)>=abs(sobs) then counter=counter+1; *Increment the counter if the test statistic is equal to or exeeds the observed statistic;
*Here we do not put an OUTPUT statement because we do not want to output all 100,000 statistics;
 end;
  p=counter/m; *p-value is the proportion of generated sequences for which the test statistic >= to observed;
  print p;
 quit;