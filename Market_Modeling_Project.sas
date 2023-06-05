/*This code demonstrates my modeling capabilities in SAS showing my linear (regression), logistic (regression), and non-linear (Gauss-Newton algorith) modeling*/

*RUN THIS;
proc import datafile='/home/u45126927/my_courses/STAT 654/MarkEff_Pharma_Data_Raw.xls' 
		out=Pharma DBMS=xls REPLACE;
	sheet="MarkEff_Pharma_Data_Raw";
	namerow=1;
	startrow=2;
	getnames=YES;
run;


*All Linear v1; 
proc REG data = Pharma  ALPHA = 0.05 ;
	model 
		Sales  = Formulary_Status Comp_DTC_TV Nov_Ind Dec_ind TV PDEs DTC_Display UR;

		output out = Pharma_out 
			predicted	= yhat
			residual	= residual
			stdr		= SE_Resid
			LCLM		= LCL_Mean
			UCLM		= UCL_Mean			
			LCL		= LCL_Prediction
			UCL		= UCL_Prediction
	;
run;

proc corr data=Pharma plots=matrix(histogram NVAR=ALL NWITH=ALL); 
	Var Sales month Formulary_Status Comp_DTC_TV Nov_Ind Dec_ind TV PDEs DTC_Display UR;
run;

*Some Log: v2+v3+v4; 
data Pharma_v2;
	set Pharma;
	ln_Sales = log(Sales+1);
	ln_DTC_TV = log(Comp_DTC_TV+1);
	ln_TV = log(TV+1);  
	ln_PDEs = log(PDEs+1);
	ln_DTC_Display = log(DTC_Display+1);
run;

proc REG data = Pharma_v2  ALPHA = 0.05 ;
	model 
		ln_Sales  = Formulary_Status ln_DTC_TV Nov_Ind Dec_ind ln_TV ln_PDEs ln_DTC_Display UR;

		output out = Pharma_out 
			predicted	= yhat
			residual	= residual
			stdr		= SE_Resid
			LCLM		= LCL_Mean
			UCLM		= UCL_Mean			
			LCL		= LCL_Prediction
			UCL		= UCL_Prediction
	;
run;

proc corr data=Pharma_v2 plots=matrix(histogram NVAR=ALL NWITH=ALL); 
	Var ln_Sales Month Formulary_Status ln_DTC_TV Nov_Ind Dec_ind ln_TV ln_PDEs ln_DTC_Display UR; 
run;

*Fixes Unemployment entered heteroskedacticity;
*Still need to fix formulary status, TV and DTC_TV and will almost always have issues with indicators;
data Pharma_v3;
	set Pharma;
	ln_Sales = log(Sales+1);
	ln_DTC_TV = log(Comp_DTC_TV+1);
	ln_TV = log(TV+1);  
	ln_PDEs = log(PDEs+1);
	ln_DTC_Display = log(DTC_Display+1);
	ln_UR = log(UR);
run;

proc REG data = Pharma_v3  ALPHA = 0.05 plots=all;
	model 
		ln_Sales  = Formulary_Status ln_DTC_TV Nov_Ind Dec_ind ln_TV ln_PDEs ln_DTC_Display ln_UR;
		output out = Pharma_out 
			predicted	= yhat
			residual	= residual
			stdr		= SE_Resid
			LCLM		= LCL_Mean
			UCLM		= UCL_Mean			
			LCL		= LCL_Prediction
			UCL		= UCL_Prediction
	;
run;

*Actual vs Predicted: Struggle to sort out early ones and 8-10;
data Pharma_out3; 
	set Pharma_out;
	label std_residual="standardized residuals";
	std_residual=residual / SE_Resid;
	Predicted = exp(yhat)-1;
run;

proc sgplot data=Pharma_out3;
   scatter x=Sales y=Predicted;
run;

data Pharma_v4;
	set Pharma;
	ln_Sales = log(Sales+1);
	ln_DTC_TV = log(Comp_DTC_TV+1);
	ln_TV = log(TV+1);  
	ln_PDEs = log(PDEs+1);
	ln_DTC_Display = log(DTC_Display+1);
run;

proc REG data = Pharma_v4  ALPHA = 0.05 ;
	model 
		Sales  = Formulary_Status ln_DTC_TV Nov_Ind Dec_ind ln_TV ln_PDEs ln_DTC_Display UR;

		output out = Pharma_out 
			predicted	= yhat
			residual	= residual
			stdr		= SE_Resid
			LCLM		= LCL_Mean
			UCLM		= UCL_Mean			
			LCL		= LCL_Prediction
			UCL		= UCL_Prediction
	;
run;

proc corr data=Pharma_v4 plots=matrix(histogram NVAR=ALL NWITH=ALL); 
	Var Sales Month Formulary_Status ln_DTC_TV Nov_Ind Dec_ind ln_TV ln_PDEs ln_DTC_Display UR; 
run;


*Basic NL: v5 - Fail; 


proc NLIN data=Pharma

	/* Print best 10 grid search steps */
	BEST=50

	/* Stop the optimization algorithm if it has not converged before Iteration = 200 */
	MAXITER=3000

	/* Use Gauss-Newton method. The other option is NEWTON */
	METHOD=GAUSS CONVERGE=1.0E-7 LIST

	/* Produce 90% approximate CIs for parameters and and predicted means*/
	ALPHA=0.01

	/*
	OUTEST	= KOKO(where = (_TYPE_ = "COVB"))
	*/;
	
	parms Beta0 = -100 to 100 by 20 Beta1 = .4 Beta2 = 2.5 Beta3 = 1.12 Beta4 = 1.89 Beta5 = 2.5 to 3 by .25 Beta6 = 1.16 Beta7 = .1535 Beta8 = -.4149;

	model Sales = Beta0 + Beta1*Formulary_Status + Comp_DTC_TV**Beta2 + Beta3*Nov_Ind + Beta4*Dec_ind + TV**Beta5 + PDEs**Beta6 + DTC_Display**Beta7 + Beta8*UR;
	/* options LCLM and UCLM request lower and upper CIs for the mean response */
	output out=Table_out p=y_hat r=residual stdr=SE_Resid LCLM=LCL_Mean 
		UCLM=UCL_Mean sse=sse;
run;



*Basic NL: v6 - Works and not terrible. Struggles until 10; 


proc NLIN data=Pharma_v4

	/* Print best 10 grid search steps */
	BEST=50

	/* Stop the optimization algorithm if it has not converged before Iteration = 200 */
	MAXITER=3000

	/* Use Gauss-Newton method. The other option is NEWTON */
	METHOD=GAUSS CONVERGE=1.0E-7 LIST

	/* Produce 90% approximate CIs for parameters and and predicted means*/
	ALPHA=0.01

	/*
	OUTEST	= KOKO(where = (_TYPE_ = "COVB"))
	*/;
	
	parms Beta1 = .4 Beta2 = .35 Beta3 = 1.12 Beta4 = 1.89 Beta5 = 2.5 to 3 by .25 Beta6 = .5 Beta7 = .38 Beta8 = 0;

	model Sales = Beta1*Formulary_Status + Beta2*ln_DTC_TV + Beta3*Nov_Ind + Beta4*Dec_ind + TV**Beta5 + Beta6*ln_PDEs + Beta7*ln_DTC_Display + Beta8*UR;
	/* options LCLM and UCLM request lower and upper CIs for the mean response */
	output out=Pharma_out6 p=y_hat r=residual stdr=SE_Resid LCLM=LCL_Mean 
		UCLM=UCL_Mean sse=sse;
run;

proc sgplot data=Pharma_out6;
   scatter x=Sales y=y_hat;
run;

*Basic NL+ beta9 for TV: v7 - Works and not terrible. Struggles until 10;



proc NLIN data=Pharma_v4

	/* Print best 10 grid search steps */
	BEST=50

	/* Stop the optimization algorithm if it has not converged before Iteration = 200 */
	MAXITER=3000

	/* Use Gauss-Newton method. The other option is NEWTON */
	METHOD=GAUSS CONVERGE=1.0E-7 LIST

	/* Produce 90% approximate CIs for parameters and and predicted means*/
	ALPHA=0.01

	/*
	OUTEST	= KOKO(where = (_TYPE_ = "COVB"))
	*/;
	
	parms Beta1 = .4 Beta2 = .35 Beta3 = 1.12 Beta4 = 1.89 Beta5 = 2.5 to 3 by .25 Beta6 = .5 Beta7 = .38 Beta8 = 0 Beta9=-1 to 100 by 1;

	model Sales = Beta1*Formulary_Status + Beta2*ln_DTC_TV + Beta3*Nov_Ind + Beta4*Dec_ind + Beta9*TV**Beta5 + Beta6*ln_PDEs + Beta7*ln_DTC_Display + Beta8*UR;
	/* options LCLM and UCLM request lower and upper CIs for the mean response */
	output out=Pharma_out7 p=y_hat r=residual stdr=SE_Resid LCLM=LCL_Mean 
		UCLM=UCL_Mean sse=sse;
run;


*Actual vs Predicted: Struggle to sort out 6-10;
data Pharma_out7; 
	set Pharma_out7;
	label std_residual="standardized residuals";
	std_residual=residual / SE_Resid;
	Predicted = y_hat;
run;

proc sgplot data=Pharma_out7;
   scatter x=Sales y=Predicted;
run;

*v8: adding in 2014 variable;

data Pharma_v8;
	set Pharma;
	After2014 = 0;
	ln_Sales = log(Sales+1);
	ln_DTC_TV = log(Comp_DTC_TV+1);
	ln_TV = log(TV+1);  
	ln_PDEs = log(PDEs+1);
	ln_DTC_Display = log(DTC_Display+1);
	if (Month>'01DEC2014'd) then After2014=1; 
run;

proc REG data = Pharma_v8  ALPHA = 0.05 ;
	model 
		Sales  = Formulary_Status ln_DTC_TV Nov_Ind Dec_ind ln_TV ln_PDEs ln_DTC_Display UR After2014;

		output out = Pharma_out 
			predicted	= yhat
			residual	= residual
			stdr		= SE_Resid
			LCLM		= LCL_Mean
			UCLM		= UCL_Mean			
			LCL		= LCL_Prediction
			UCL		= UCL_Prediction
	;
run;

proc corr data=Pharma_v8 plots=matrix(histogram NVAR=ALL NWITH=ALL); 
	Var Sales Formulary_Status ln_DTC_TV Nov_Ind Dec_ind ln_TV ln_PDEs ln_DTC_Display UR After2014; 
run;

*Basic NL + After2014: v9 - Worked; 


proc NLIN data=Pharma_v8

	/* Print best 10 grid search steps */
	BEST=50

	/* Stop the optimization algorithm if it has not converged before Iteration = 200 */
	MAXITER=3000

	/* Use Gauss-Newton method. The other option is NEWTON */
	METHOD=GAUSS CONVERGE=1.0E-7 LIST

	/* Produce 90% approximate CIs for parameters and and predicted means*/
	ALPHA=0.01

	/*
	OUTEST	= KOKO(where = (_TYPE_ = "COVB"))
	*/;
	
	parms Beta1 = .4 Beta2 = .35 Beta3 = 1.12 Beta4 = 1.89 Beta5 = 2.5 to 3 by .25 Beta6 = .5 Beta7 = .38 Beta8 = 0 Beta9=-1 to 100 by 1 Beta10 = 1.57108;

	model Sales = Beta1*Formulary_Status + Beta2*ln_DTC_TV + Beta3*Nov_Ind + Beta4*Dec_ind + Beta9*TV**Beta5 + Beta6*ln_PDEs + Beta7*ln_DTC_Display + Beta8*UR+ Beta10*After2014;
	/* options LCLM and UCLM request lower and upper CIs for the mean response */
	output out=Pharma_out9 p=y_hat r=residual stdr=SE_Resid LCLM=LCL_Mean 
		UCLM=UCL_Mean sse=sse;
run;


*Actual vs Predicted: Much Better!;
data Pharma_out9; 
	set Pharma_out9;
	label std_residual="standardized residuals";
	std_residual=residual / SE_Resid;
	Predicted = y_hat;
run;

proc sgplot data=Pharma_out9;
   scatter x=Sales y=Predicted;
run;

proc sgplot data=Pharma_out9;
   scatter x=Predicted y=residual;
run;



*NL + Gompertz + After2014: v10 - Worked; 


proc NLIN data=Pharma_v8 

	/* Print best 10 grid search steps */
	BEST=50

	/* Stop the optimization algorithm if it has not converged before Iteration = 200 */
	MAXITER=3000

	/* Use Gauss-Newton method. The other option is NEWTON */
	METHOD=GAUSS CONVERGE=1.0E-7 LIST

	/* Produce 90% approximate CIs for parameters and and predicted means*/
	ALPHA=0.01

	/*
	OUTEST	= KOKO(where = (_TYPE_ = "COVB"))
	*/;
	
	parms Beta1 = .4 Beta2 = .35 Beta3 = 1.12 Beta4 = 1.89 Beta5=-1 to 20 by 1 Beta6 = -1 to 2 by .5 Beta7 = -.1 to .1 by .01 Beta8 = .5 Beta9 = .38 Beta10 = 0 Beta11 = 1.57108;

	model Sales = Beta1*Formulary_Status + Beta2*ln_DTC_TV + Beta3*Nov_Ind + Beta4*Dec_ind + Beta5*exp(-1*Beta6*exp(-1*Beta7*TV)) + Beta8*ln_PDEs + Beta9*ln_DTC_Display + Beta10*UR+ Beta11*After2014;
	/* options LCLM and UCLM request lower and upper CIs for the mean response */
	output out=Pharma_out10 p=y_hat r=residual stdr=SE_Resid LCLM=LCL_Mean 
		UCLM=UCL_Mean sse=sse;
run;

*Actual vs Predicted: Much Better!;
data Pharma_out10; 
	set Pharma_out10;
	label std_residual="standardized residuals";
	std_residual=residual / SE_Resid;
	Predicted = y_hat;
run;

proc sgplot data=Pharma_out10;
   scatter x=Sales y=Predicted;
run;

*Residuals by Predicted;
proc sgplot data=Pharma_out10;
   scatter x=Predicted y=residual;
run;

*Residuals by regressors;
proc sgplot data=Pharma_out10;
   scatter x=Formulary_Status y=residual;
run;

proc sgplot data=Pharma_out10;
   scatter x=ln_DTC_TV y=residual;
run;

proc sgplot data=Pharma_out10;
   scatter x=Nov_Ind y=residual;
run;

proc sgplot data=Pharma_out10;
   scatter x=Dec_ind y=residual;
run;

proc sgplot data=Pharma_out10;
   scatter x=TV y=residual;
run;

*PDEs is concerning;
proc sgplot data=Pharma_out10;
   scatter x=ln_PDEs y=residual;
run;

proc sgplot data=Pharma_out10;
   scatter x=ln_DTC_Display y=residual;
run;

proc sgplot data=Pharma_out10;
   scatter x=UR y=residual;
run;

proc sgplot data=Pharma_out10;
   scatter x=After2014 y=residual;
run;

*NL - Fix PDEs + Gompertz + After2014: v11 - Worked;

proc NLIN data=Pharma_v8 

	/* Print best 10 grid search steps */
	BEST=50

	/* Stop the optimization algorithm if it has not converged before Iteration = 200 */
	MAXITER=3000

	/* Use Gauss-Newton method. The other option is NEWTON */
	METHOD=GAUSS CONVERGE=1.0E-7 LIST

	/* Produce 90% approximate CIs for parameters and and predicted means*/
	ALPHA=0.01

	/*
	OUTEST	= KOKO(where = (_TYPE_ = "COVB"))
	*/;
	
	parms Beta1 = .0668 Beta2 = -.0227 Beta3 = .9747 Beta4 = 1.66 Beta5=7.545 Beta6 = .4423 Beta7 = .00918 Beta8 = 0 to 1 by .1 Beta9= .5 to 2.5 by .5 Beta10 = .0606 Beta11 = -.0492 Beta12 = 1.59;

	model Sales = Beta1*Formulary_Status + Beta2*ln_DTC_TV + Beta3*Nov_Ind + Beta4*Dec_ind + Beta5*exp(-1*Beta6*exp(-1*Beta7*TV)) + Beta8*PDEs**Beta9 + Beta10*ln_DTC_Display + Beta11*UR+ Beta12*After2014;
	/* options LCLM and UCLM request lower and upper CIs for the mean response */
	output out=Pharma_out11 p=y_hat r=residual stdr=SE_Resid LCLM=LCL_Mean 
		UCLM=UCL_Mean sse=sse;
run;

*Actual vs Predicted: Much Better!;
data Pharma_out11; 
	set Pharma_out11;
	label std_residual="standardized residuals";
	std_residual=residual / SE_Resid;
	Predicted = y_hat;
run;

proc sgplot data=Pharma_out11;
   scatter x=Sales y=Predicted;
run;

*Residuals by Predicted;
proc sgplot data=Pharma_out11;
   scatter x=Predicted y=residual;
run;

*Residuals by regressors;
proc sgplot data=Pharma_out11;
   scatter x=Formulary_Status y=residual;
run;

proc sgplot data=Pharma_out11;
   scatter x=ln_DTC_TV y=residual;
run;

proc sgplot data=Pharma_out11;
   scatter x=Nov_Ind y=residual;
run;

proc sgplot data=Pharma_out11;
   scatter x=Dec_ind y=residual;
run;

proc sgplot data=Pharma_out11;
   scatter x=TV y=residual;
run;

proc sgplot data=Pharma_out11;
   scatter x=PDEs y=residual;
run;

proc sgplot data=Pharma_out11;
   scatter x=ln_DTC_Display y=residual;
run;

proc sgplot data=Pharma_out11;
   scatter x=UR y=residual;
run;

proc sgplot data=Pharma_out11;
   scatter x=After2014 y=residual;
run;