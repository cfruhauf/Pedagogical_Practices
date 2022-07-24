********************************************************************************
********************************************************************************
**********************************  3 EM LP  ***********************************
********************************************************************************

**** ÍNDICE DE PRÁTICAS PEDAGÓGICAS - GERAL (IPP_G) ****
mca IPP_G_1 IPP_G_2 IPP_G_3 IPP_G_4 IPP_G_5 IPP_G_6 IPP_G_7

*mcaplot, overlay origin normalize(principal)

predict IPP_G


**** ÍNDICE DE PRÁTICAS PEDAGÓGICAS - LÍNGUA PORTUGUESA (IPP_LP) ****
mca IPP_LP_1 IPP_LP_2 IPP_LP_3 IPP_LP_4 IPP_LP_5 IPP_LP_6

*mcaplot, overlay origin normalize(principal)

predict IPP_LP

**** NÍVEL SOCIOECONOMICO 1 (NSE1) ****
mca TV GELADEIRA FREEZER MAQ CARRO COMP BANHO EMPR

*mcaplot, overlay origin normalize(principal)

predict NSE1

*********************************************************************************************************
***** VAMOS INVERTER A ORDEM DE VALOR DOS INDICADORES E COLOCAR OS INDICADORES EM ESCALA DE 0 a 10  *****
*********************************************************************************************************

***************************
***** IPP_G
***************************

sum IPP_G
gen IPP_G_ = (10 - IPP_G)

sum IPP_G_
gen IPP_G_I = ((IPP_G_-(6.946215))/(11.2358-(6.946215)))*10

replace IPP_G_I=round(IPP_G_I, 0.0001)
sum IPP_G_I

***************************
***** IPP_LP
***************************

sum IPP_LP
gen IPP_LP_ = (10 - IPP_LP)

sum IPP_LP_
gen IPP_LP_I = ((IPP_LP_-(8.071779))/(11.23279-(8.071779)))*10

replace IPP_LP_I=round(IPP_LP_I, 0.0001)
sum IPP_LP_I


***************************
***** NSE1
***************************

sum NSE1
gen NSE1_ = (10 - NSE1)

sum NSE1_
gen NSE1_I = ((NSE1_-(3.307561))/(11.45755-(3.307561)))*10

replace NSE1_I=round(NSE1_I, 0.0001)
sum NSE1_I

*********************************************************************************************************
*********************************** VARIAVEIS PARA ESTATISTICAS  ****************************************
*********************************************************************************************************
gen MASCULINO=.
replace MASCULINO=1 if GENERO==0
replace MASCULINO=0 if GENERO==1

gen NAOBRANCO=.
replace NAOBRANCO=1 if RACA==0
replace NAOBRANCO=0 if RACA==1

gen NAOREPROV=.
replace NAOREPROV=1 if REPROV==0
replace NAOREPROV=0 if REPROV==1

gen NAOGOSTALP=.
replace NAOGOSTALP=1 if GOSTA_LP==0
replace NAOGOSTALP=0 if GOSTA_LP==1

gen EXPMENOS5=.
replace EXPMENOS5=1 if EXP_ESCOLA_M5==0
replace EXPMENOS5=0 if EXP_ESCOLA_M5==1

gen EXPDIRMENOSM5=.
replace EXPDIRMENOSM5=1 if EXP_DIR_M5==0
replace EXPDIRMENOSM5=0 if EXP_DIR_M5==1

gen PUBLICA=.
replace PUBLICA=1 if DEPENDENCIA_ADM==0
replace PUBLICA=0 if DEPENDENCIA_ADM==1

gen RURAL=.
replace RURAL=1 if LOCALIZACAO==0
replace RURAL=0 if LOCALIZACAO==1

*********************************************************************************************************
******************************************** STEP WISE  *************************************************
*********************************************************************************************************

      ** MODELO 1 **
** Intercepto aleatorio **

xtmixed LP || ESCOLA: || TURMA: , mle nolog

** Coeficiente intra classe **
estat icc


       ** MODELO 2 **
       ** alunos **
	   
xtmixed LP GENERO RACA ESC_MENOS_8_9_MAE ESC_MENOS_EM_MAE ESC_MENOS_ES_MAE ///
ESC_ES_MAE ESC_MENOS_8_9_PAI ESC_MENOS_EM_PAI ESC_MENOS_ES_PAI ESC_ES_PAI ///
REPROV GOSTA_LP NSE1_I || ESCOLA: || TURMA: , mle nolog


      ** MODELO 3 **
       ** professor **

xtmixed LP GENERO RACA ESC_MENOS_8_9_MAE ESC_MENOS_EM_MAE ESC_MENOS_ES_MAE ///
ESC_ES_MAE ESC_MENOS_8_9_PAI ESC_MENOS_EM_PAI ESC_MENOS_ES_PAI ESC_ES_PAI ///
REPROV GOSTA_LP NSE1_I LATO_PG STRICTO_PG EXP_ESCOLA_M5 RENDA_1E3_PROF ///
RENDA_3E4_PROF RENDA_4E7_PROF RENDA_7_PROF IPP_G_I IPP_LP_I || ESCOLA: || TURMA: , mle nolog


      ** MODELO 4 **
       ** diretor/escola **

xtmixed LP GENERO RACA ESC_MENOS_8_9_MAE ESC_MENOS_EM_MAE ESC_MENOS_ES_MAE ///
ESC_ES_MAE ESC_MENOS_8_9_PAI ESC_MENOS_EM_PAI ESC_MENOS_ES_PAI ESC_ES_PAI ///
REPROV GOSTA_LP NSE1_I LATO_PG STRICTO_PG EXP_ESCOLA_M5 RENDA_1E3_PROF ///
RENDA_3E4_PROF RENDA_4E7_PROF RENDA_7_PROF IPP_G_I IPP_LP_I EXP_DIR_M5 ///
LOCALIZACAO DEPENDENCIA_ADM || ESCOLA: || TURMA: , mle nolog
