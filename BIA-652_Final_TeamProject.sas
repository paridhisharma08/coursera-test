*-----------------------------------------------------------------------------------;
* Project        : Multivariate Data Analysis I                           			;
* Developers     : Paridhi Sharma, Saumya Sharma
* Comments       : Final Project                                          			;
*-----------------------------------------------------------------------------------; 

* IMPORTING DATASET FROM EXCEL SHEET TO WORK DIRECTORY;
PROC IMPORT OUT= WORK.Train 
            DATAFILE= "D:\MIS_SecondSemester\BIA-652\default of credit card clients.xls" 
            DBMS=EXCEL REPLACE;
     RANGE="Data$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

* CHANGING NAME OF DEPENDENT VARIABLE;
Data PAYMENT;
set Train;
 default_pay=default_payment_next_month;
run;

* CREATING MACRO TO REMOVE OUTLIERS;
%macro outliers(input=, var= , output=);

/* Calculate the quartiles and inter-quartile range using proc univariate */
proc univariate data=&Input noprint;
var &Var;
output out=temp QRANGE=IQR Q1=First_Qtl Q3=Third_Qtl;
run;

/* Extract the upper and lower limits into macro variables */
data _null_;
set temp;
call symput('QR', IQR);
call symput('Q1', First_Qtl);
call symput('Q3', Third_Qtl);
run;

%let ULimit=%sysevalf(&Q3 + 1.5 * &QR);
%let LLimit=%sysevalf(&Q1 - 1.5 * &QR);

/* Final dataset excluding outliers*/
data &output;
set &input;
%put See the lower and upper limit of acceptable range below: ;
%put Lower limit = &LLimit;
%put Upper limit = &ULimit;
if &Var >= &Llimit and &Var <= &ULimit;
run;
%mend;

*MACRO CALLS TO REMOVE OUTLIERS;
%outliers(Input=payment, Var = LIMIT_BAL, Output= Outdata);
%outliers(Input=Outdata, Var = AGE, Output= Outdata1);
%outliers(Input=Outdata1, Var = BILL_AMT1, Output= Outdata2);
%outliers(Input=Outdata2, Var = BILL_AMT2, Output= Outdata3);
%outliers(Input=Outdata3, Var = BILL_AMT3, Output= Outdata4);
%outliers(Input=Outdata4, Var = BILL_AMT4, Output= Outdata5);
%outliers(Input=Outdata5, Var = BILL_AMT5, Output= Outdata6);
%outliers(Input=Outdata6, Var = BILL_AMT6, Output= Outdata7);
%outliers(Input=Outdata7, Var = PAY_AMT1, Output= Outdata8);
%outliers(Input=Outdata8, Var = PAY_AMT2, Output= Outdata9);
%outliers(Input=Outdata9, Var = PAY_AMT3, Output= Outdata10);
%outliers(Input=Outdata10, Var = PAY_AMT4, Output= Outdata11);
%outliers(Input=Outdata11, Var = PAY_AMT5, Output= Outdata12);
%outliers(Input=Outdata12, Var = PAY_AMT6, Output=payment_new);


* METHOD 1: USING MULTIPLE REGRESSION,LOGISTIC REGRESSION AND DISCRIMINANT ANALYSIS ON PAYMENT DATASET;

ODS PDF FILE='D:\MIS_SecondSemester\BIA-652\Assignments\Output\FINAL_PROJECT_METHOD1.PDF' style= BRICK;

TITLE'METHOD 1: USING NORMAL REGRESSION,LOGISTIC REGRESSION AND DISCRIMINANT ANALYSIS OF ORIGINAL DATASET.';

* REGESSION MODEL USING ALL VARIABLES FROM PAYMENT DATASET;
TITLE'REGESSION MODEL USING ALL VARIABLES FROM PAYMENT DATASET';
proc reg data=payment_new outest=est PLOTS(MAXPOINTS=NONE);
    model default_pay = LIMIT_BAL SEX EDUCATION MARRIAGE AGE PAY_0 PAY_2 PAY_3 PAY_4   
                        PAY_5 PAY_6 BILL_AMT1 BILL_AMT2 BILL_AMT3 BILL_AMT4 BILL_AMT5 
                        BILL_AMT6 PAY_AMT1 PAY_AMT2 PAY_AMT3 PAY_AMT4 PAY_AMT5 PAY_AMT6 / dwProb STB;
    OUTPUT OUT=reg_payment  PREDICTED=reg_default_predict RESIDUAL=d_RES L95M=l_l95m U95M=l_u95m L95=l_l95 U95=l_u95
    rstudent=d_rstudent h=lev cookd=Cookd  dffits=diffit STDP=d_spredicted  STDR=d_s_residual  STUDENT=d_student;
quit; 

* LOGISTIC REGESSION MODEL USING SIGNIFICANT VARIABLES FROM PAYMENT DATASET;
TITLE'LOGISTIC REGESSION MODEL USING SIGNIFICANT VARIABLES FROM PAYMENT DATASET';
proc logistic data=reg_payment descending;
    model default_pay = BILL_AMT1 BILL_AMT5 BILL_AMT6 EDUCATION MARRIAGE PAY_0 PAY_2 PAY_4
                                       PAY_AMT1 PAY_AMT2 PAY_AMT3 PAY_AMT5 / rsq ;
quit;

* PRINCIPAL COMPONENT ANALYSIS FOR PAYMENT DATASET;
TITLE'PRINCIPAL COMPONENT ANALYSIS FOR PAYMENT DATASET';
proc princomp data=payment_new out=pca_payment;
    var LIMIT_BAL SEX EDUCATION MARRIAGE AGE PAY_0 PAY_2 PAY_3 PAY_4   
        PAY_5 PAY_6 BILL_AMT1 BILL_AMT2 BILL_AMT3 BILL_AMT4 BILL_AMT5 
        BILL_AMT6 PAY_AMT1 PAY_AMT2 PAY_AMT3 PAY_AMT4 PAY_AMT5 PAY_AMT6;
run;

* PRINCIPAL COMPONENT ANALYSIS FOR PAYMENT DATASET;
TITLE'LOGISTIC REGRESSION MODEL USING SIGNIFICANT PRINCIPAL COMPONENTS';
proc logistic data=pca_payment descending;
    model default_pay = Prin1 Prin2 Prin3 Prin4 Prin5 Prin6 /  rsq ;
quit;

* STEPDISC PROCEDURE TO IDENTIFY SIGNIFICANT VARIABLES FOR DISCRIMINANT ANALYSIS;
TITLE'STEPDISC PROCEDURE TO IDENTIFY SIGNIFICANT VARIABLES FOR DISCRIMINANT ANALYSIS';
proc stepdisc data=payment_new slstay= 0.05 bsscp tsscp;
      class default_pay;
      var LIMIT_BAL SEX EDUCATION MARRIAGE AGE PAY_0 PAY_2 PAY_3 PAY_4   
          PAY_5 PAY_6 BILL_AMT1 BILL_AMT2 BILL_AMT3 BILL_AMT4 BILL_AMT5 
          BILL_AMT6 PAY_AMT1 PAY_AMT2 PAY_AMT3 PAY_AMT4 PAY_AMT5 PAY_AMT6;
run;

* DISCRIMINANT ANALYSIS USING ALL VARIABLES FROM PAYMENT DATASET;
TITLE'DISCRIMINANT ANALYSIS USING VARIABLES INDENTIFIED FROM STEPDISC PROCEDURE ';
proc discrim data= payment_new distance anova;
    class default_pay;
    var &_STDVAR ;
run;

ODS PDF CLOSE;

ODS PDF FILE='D:\MIS_SecondSemester\BIA-652\Assignments\Output\FINAL_PROJECT_METHOD2.PDF' style= BRICK;
* METHOD 2: USING MULTIPLE REGRESSION, LOGISTIC REGRESSION AND DISCRIMINANT ANALYSIS ON STANDARDIZED PAYMENT DATASET ;

* STANDARDIZING PAYMENT DATASET;
PROC STANDARD DATA=payment_new
             MEAN=0 STD=1 
             OUT=payment_z(rename=(
                        LIMIT_BAL = LIMIT_BAL_z 
                        Age = Age_z
                        BILL_AMT1 = BILL_AMT1_z
                        BILL_AMT2 = BILL_AMT2_z 
                        BILL_AMT3 = BILL_AMT3_z
                        BILL_AMT4 = BILL_AMT4_z
                        BILL_AMT5 = BILL_AMT5_z 
                        BILL_AMT6 = BILL_AMT6_z
                        PAY_AMT1 = PAY_AMT1_z
                        PAY_AMT2 = PAY_AMT2_z
                        PAY_AMT3 = PAY_AMT3_z 
                        PAY_AMT4 = PAY_AMT4_z
                        PAY_AMT5 = PAY_AMT5_z 
                        PAY_AMT6 = PAY_AMT6_z));
  VAR LIMIT_BAL Age BILL_AMT1 BILL_AMT2 BILL_AMT3 BILL_AMT4 BILL_AMT5   
      BILL_AMT6 PAY_AMT1 PAY_AMT2 PAY_AMT3 PAY_AMT4 PAY_AMT5 PAY_AMT6;
RUN;

* MULTIPLE REGRESSION ON STANDARDIZED PAYMENT DATASET;
TITLE'MULTIPLE REGRESSION ON STANDARDIZED PAYMENT DATASET';
proc reg data=payment_z outest=est PLOTS(MAXPOINTS=NONE);
    model default_pay = LIMIT_BAL_z SEX EDUCATION MARRIAGE AGE_z   
                        PAY_0 PAY_2 PAY_3 PAY_4 PAY_5 PAY_6 
                        BILL_AMT1_z BILL_AMT2_z BILL_AMT3_z BILL_AMT4_z 
                        BILL_AMT5_z BILL_AMT6_z PAY_AMT1_z PAY_AMT2_z PAY_AMT3_z   
                        PAY_AMT4_z  PAY_AMT5_z  PAY_AMT6_z / dwProb STB;
    OUTPUT OUT=reg_defout  PREDICTED=reg_defpredict RESIDUAL=d_RES L95M=l_l95m  U95M=l_u95m  L95=l_l95 U95=l_u95
    rstudent=d_rstudent h=lev cookd=Cookd  dffits=diffit
    STDP=d_spredicted  STDR=d_s_residual  STUDENT=d_student;
quit;

* LOGISTIC REGRESSION USING SIGNIFICANT VARIABLES FROM STANDARDIZED DATASET; 
TITLE'LOGISTIC REGRESSION USING SIGNIFICANT VARIABLES FROM STANDARDIZED DATASET.';
proc logistic data=reg_defout descending;
    model default_pay = BILL_AMT1_z BILL_AMT5_z BILL_AMT6_z EDUCATION MARRIAGE PAY_0 PAY_2 PAY_4
                        PAY_AMT1_z PAY_AMT2_z PAY_AMT3_z PAY_AMT5_z/ rsq ;
quit;

* PRINCIPAL COMPONENT ANALYSIS OF STANDERDIZED PAYMENT DATASET;
TITLE'PRINCIPAL COMPONENT ANALYSIS OF STANDERDIZED PAYMENT DATASET';
proc princomp data=payment_z out=pca_payment1;
    var LIMIT_BAL_z SEX  EDUCATION MARRIAGE AGE_z PAY_0 PAY_2 PAY_3 
        PAY_4 PAY_5 PAY_6 BILL_AMT1_z BILL_AMT2_z BILL_AMT3_z BILL_AMT4_z 
        BILL_AMT5_z BILL_AMT6_z PAY_AMT1_z PAY_AMT2_z PAY_AMT3_z PAY_AMT4_z  
        PAY_AMT5_z  PAY_AMT6_z;
run;

* LOGISTIC REGRESSION USING SIGNIFICANT PRINCIPAL COMPONENTS;
TITLE'LOGISTIC REGRESSION USING SIGNIFICANT PRINCIPAL COMPONENTS';
proc logistic data=pca_payment1 descending;
    model default_pay = Prin1 Prin2 Prin3 Prin4 Prin5 Prin6/ rsq ;
quit;

* STEPDISC PROCEDURE TO IDENTIFY SIGNIFICANT VARIABLES FOR DISCRIMINANT ANALYSIS;
TITLE'STEPDISC PROCEDURE TO IDENTIFY SIGNIFICANT VARIABLES FOR DISCRIMINANT ANALYSIS';
proc stepdisc data=payment_Z slstay= 0.05 bsscp tsscp;
      class default_pay;
      var LIMIT_BAL_z SEX  EDUCATION MARRIAGE AGE_z PAY_0 PAY_2 PAY_3 
          PAY_4 PAY_5 PAY_6 BILL_AMT1_z BILL_AMT2_z BILL_AMT3_z BILL_AMT4_z 
          BILL_AMT5_z BILL_AMT6_z PAY_AMT1_z PAY_AMT2_z PAY_AMT3_z   PAY_AMT4_z  
          PAY_AMT5_z  PAY_AMT6_z;
run;

* DISCRIMINANT ANALYSIS OF STANDARDIZED PAYMENT DATASET;
TITLE'DISCRIMINANT ANALYSIS OF STANDARDIZED PAYMENT DATASET';
proc discrim data= payment_z distance anova;
    class default_pay;
    var &_STDVAR;
run;
ODS PDF CLOSE;

ODS PDF FILE='D:\MIS_SecondSemester\BIA-652\Assignments\Output\FINAL_PROJECT_METHOD3.PDF' style= BRICK;

* METHOD 3: MULTIPLE REGRESSION, LOGISTIC REGRESSION AND DISCRIMINANT ANALYSIS USING SQUARE AND LOGARITHMIC TRANFORMATION OF STANDARDIZED PAYMENT DATASET;

* SQUARE TRANFORMATION;
data payment_trans;
    set payment_z;
    sq_limit_bal = LIMIT_BAL_z**2;
    sq_bill_amt1 = (BILL_AMT1_z)**2;
    sq_bill_amt2 = (BILL_AMT2_z)**2; 
    sq_bill_amt3 = (BILL_AMT3_z)**2;
    sq_bill_amt4 = (BILL_AMT4_z)**2;
    sq_bill_amt5 = (BILL_AMT5_z)**2;
    sq_bill_amt6 = (BILL_AMT6_z)**2;
    sq_pay_amt1 = (PAY_AMT1_z)**2;
    sq_pay_amt2 = (PAY_AMT2_z)**2;
    sq_pay_amt3 = (PAY_AMT3_z)**2 ;
    sq_pay_amt4 = (PAY_AMT4_z)**2;
    sq_pay_amt5 = (PAY_AMT5_z)**2 ;
    sq_pay_amt6 = (PAY_AMT6_z)**2;
run;

* LOGARITHMIC TRANFORMATION;
data payment_transform;
    set payment_trans;
    log_limit_bal = log(sq_limit_bal);
    log_bill_amt1 = log(sq_bill_amt1);
    log_bill_amt2 = log(sq_bill_amt2); 
    log_bill_amt3 = log(sq_bill_amt3);
    log_bill_amt4 = log(sq_bill_amt4);
    log_bill_amt5 = log(sq_bill_amt5) ;
    log_bill_amt6 = log(sq_bill_amt6);
    log_pay_amt1 = log(sq_pay_amt1);
    log_pay_amt2 = log(sq_pay_amt2);
    log_pay_amt3 = log(sq_pay_amt3) ;
    log_pay_amt4 = log(sq_pay_amt4);
    log_pay_amt5 = log(sq_pay_amt5) ;
    log_pay_amt6 = log(sq_pay_amt6);
run;

* MULIPTLE REGRESSION ON TRANSFORMED PAYMENT DATASET;
TITLE'MULIPTLE REGRESSION ON TRANSFORMED PAYMENT DATASET';
proc reg data=payment_transform outest=est PLOTS(MAXPOINTS=NONE);
    model default_pay = SEX EDUCATION MARRIAGE PAY_0 PAY_2 PAY_3 PAY_4 PAY_5 PAY_6 
                        log_limit_bal log_bill_amt1 log_bill_amt2 log_bill_amt3 log_bill_amt4
                        log_bill_amt5 log_bill_amt6 log_pay_amt1 log_pay_amt2 log_pay_amt3 log_pay_amt4 
                        log_pay_amt5 log_pay_amt6/ dwProb STB;
    OUTPUT OUT=reg_def_out  PREDICTED=reg_defpredict RESIDUAL=d_RES L95M=l_l95m  U95M=l_u95m  L95=l_l95 U95=l_u95
    rstudent=d_rstudent h=lev cookd=Cookd  dffits=diffit
    STDP=d_spredicted  STDR=d_s_residual  STUDENT=d_student;
quit;

* LOGISTIC REGRESSION USING SIGNIFICANT VARIABLES FROM TRANSFORMED PAYMENT DATASE; 
TITLE'LOGISTIC REGRESSION USING SIGNIFICANT VARIABLES FROM TRANSFORMED PAYMENT DATASET';
proc logistic data=reg_def_out descending;
    model default_pay = MARRIAGE PAY_0 PAY_2 PAY_6 log_pay_amt1 log_pay_amt2 log_pay_amt3 log_pay_amt4/ rsq ;
quit;

* PRINCIPAL COMPONENT ANALYSIS OF TRANSFORMED PAYMENT DATASET;
TITLE'PRINCIPAL COMPONENT ANALYSIS OF TRANSFORMED PAYMENT DATASET';
proc princomp data=payment_transform out=pca_payment_transform;
    var SEX EDUCATION MARRIAGE PAY_0 PAY_2 PAY_3 PAY_4 PAY_5 PAY_6 
        log_limit_bal log_bill_amt1 log_bill_amt2 log_bill_amt3 log_bill_amt4
        log_bill_amt5 log_bill_amt6 log_pay_amt1 log_pay_amt2 log_pay_amt3 log_pay_amt4 
        log_pay_amt5 log_pay_amt6;
run;

*LOGISTIC REGRESSION USING SIGNIFICANT PRINCIPAL COMPONENTS;
TITLE'LOGISTIC REGRESSION USING SIGNIFICANT PRINCIPAL COMPONENTS';
proc logistic data=pca_payment_transform descending;
    model default_pay = Prin1 Prin2 Prin3 Prin4 Prin5 Prin7 Prin8 Prin9 / rsq ;
quit;

* STEPDISC PROCEDURE TO IDENTIFY SIGNIFICANT VARIABLES FOR DISCRIMINANT ANALYSIS;
TITLE'STEPDISC PROCEDURE TO IDENTIFY SIGNIFICANT VARIABLES FOR DISCRIMINANT ANALYSIS';
proc stepdisc data=payment_transform slstay= 0.05 bsscp tsscp;
      class default_pay;
      var SEX EDUCATION MARRIAGE PAY_0 PAY_2 PAY_3 PAY_4 PAY_5 PAY_6 
          log_limit_bal log_bill_amt1 log_bill_amt2 log_bill_amt3 log_bill_amt4
          log_bill_amt5 log_bill_amt6 log_pay_amt1 log_pay_amt2 log_pay_amt3 log_pay_amt4 
          log_pay_amt5 log_pay_amt6;
RUN;

*DISCRIMINANT ANALYSIS USING ALL VARIABLES FROM TRANSFORMED PAYMENT DATASET;
TITLE'DISCRIMINANT ANALYSIS USING ALL VARIABLES FROM TRANSFORMED PAYMENT DATASET';
proc discrim data= payment_transform distance anova;
    class default_pay;
    var &_stdvar;
run;
ODS PDF CLOSE;




