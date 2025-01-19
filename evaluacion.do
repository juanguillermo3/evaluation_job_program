
*
*
* program evaluation for the ruta de la empleabilidad
*

/*
* this module implement the program evaluation techniques for la 
* ruta de la empleabilidad  with several estimations based on the
* counterfactual outcomes frameworks. it provides methods for testing
* the identification assumptions and estimating the policy effect as
* the average treetment-effect-of-the-treated.
*/

*
* 0. some set-up for stata
*
macro drop _all
cls
*
* 0.2 routing
*
global APP_HOME="C:\Users\josio\OneDrive - Fedesarrollo\proyecto grade"
global EVALUATION_HOME="C:\Users\josio\OneDrive - Fedesarrollo\proyecto grade\program evaluation"

*
* 1.0 data-environment for the evaluation
*
*
cd "$EVALUATION_HOME"
dir
use "program evaluation dataset.dta", clear
*
* 1.1 quality control information about estimation
*
*
cd "$EVALUATION_HOME"
dir
use "program evaluation dataset.dta", clear
*
*
local D = c(current_date)
local T = c(current_time)
local T = subinstr("`T'",":"," ",.)
di "`D' `T'"
global evaluation_datetime= "`D' `T'"  
cd "$EVALUATION_HOME"
cd "evaluaciones"
mkdir "$evaluation_datetime"
*
*
*https://www.stata.com/statalist/archive/2006-08/msg00548.html

*
* (0) formal definition of treat var
*
*
destring minuto, replace
capture drop treatment_0
capture drop treatment_1
gen treatment_0=cond(minuto>0, 1, 0)
gen treatment_1=cond(minuto>1, 1, 0)
replace  treatment_1=. if minuto==1
table treatment_0
table treatment_1
capture label drop generic_treatment_label
label define generic_treatment_label 0 "control" 1 "treatment"
label values treatment_0 generic_treatment_label
*
cd "$EVALUATION_HOME"
cd "evaluaciones"
cd "$evaluation_datetime"
*
asdoc, text( \n ) fs(10), append save(reporte de evaluacion.doc)
asdoc, text( \n ) fs(10), append save(reporte de evaluacion.doc)
asdoc table treatment_0, append save(reporte de evaluacion.doc)
asdoc table treatment_1, append save(reporte de evaluacion.doc)
*
* (0.2) feature selection on control set as global
*
*
global control_set="edad mujer"
global control_set="$control_set government_program condiciones discapacidades migrante"
global control_set="$control_set edu_*"
global control_set="$control_set lab_*"
asdoc, text( \n ) fs(10), append save(reporte de evaluacion.doc)
asdoc, text( \n ) fs(10), append save(reporte de evaluacion.doc)
asdoc tabstat $control_set, stat(min max count sum), append save(reporte de evaluacion.doc)
*
* (1) 
*
global depurated_control_set=""
*
asdoc, text( \n ) fs(10), append save(reporte de evaluacion.doc)
asdoc, text( \n ) fs(10), append save(reporte de evaluacion.doc)
asdoc, text(drop actions performed by system:) fs(10), append save(reporte de evaluacion.doc)
*
foreach some_controls in $control_set{
	    preserve
		{
		keep `some_controls'
		describe
		foreach  a_control of varlist _all{
		di "`a_control'"
		quietly sum `a_control'
		*di "`r(min)' `r(max)' `r(sum)'"
		if `r(min)'==0 & `r(max)'==1 & `r(sum)'<30 {
		asdoc, text(`a_control' dropped by system because small subsample size) fs(10), append save(reporte de evaluacion.doc)
		local a_control =""
		}
		global depurated_control_set="$depurated_control_set `a_control'"
		}
		}
		restore
}
*
di "$depurated_control_set"
*
* (2) 
*
*
replace alguna_colocacion=0 if alguna_colocacion==.
tab alguna_colocacion
asdoc, text( \n ) fs(10), append save(reporte de evaluacion.doc)
asdoc, text( \n ) fs(10), append save(reporte de evaluacion.doc)
asdoc table alguna_colocacion, append save(reporte de evaluacion.doc)
*
asdoc, text( \n ) fs(10), append save(reporte de evaluacion.doc)
asdoc, text( \n ) fs(10), append save(reporte de evaluacion.doc)
asdoc tabstat alguna_colocacion, by(treatment_0) append save(reporte de evaluacion.doc)
asdoc tabstat alguna_colocacion, by(treatment_1) append save(reporte de evaluacion.doc)


*
* 1.2 propensity score model estimation
*
*

*
*
di "$depurated_control_set"
asdoc, text( \n ) fs(10), append save(reporte de evaluacion.doc)
asdoc, text( \n ) fs(10), append save(reporte de evaluacion.doc)
asdoc kmatch ps treatment_0 $depurated_control_set  /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
		att append save(reporte de evaluacion.doc)
*
asdoc kmatch ps treatment_1 $depurated_control_set  /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
		att append save(reporte de evaluacion.doc)
*
asdoc kmatch ps treatment_1 $depurated_control_set  /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
		att append save(reporte de evaluacion.doc)
*
ereturn list


*
* 2.1 statistical testing of the psm sys
*
*

*
*
* (1) assestment gráfico de balance
* --------
{ 
*  
capture drop _KM_*
*
quietly kmatch ps treatment_1 $depurated_control_set  /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
		att 
*di e(tvar)
}
*
{
*
matrix S=e(_N)
matrix list e(_N)
*
local s=S[1,1]
local _s= (S[1,1]/S[1,3])*100
local lb = "2 o mas sesiones de entrenamiento" 
*
local s: di %5.0f `s'
local _s: di %5.2f `_s'
*
cd "$EVALUATION_HOME"
cd "evaluaciones"
cd "$evaluation_datetime"
kmatch box, title( `lb' - `s' - `_s'% ) noout
*
graph export "soporte comun `v'.pdf", replace 
graph save "soporte comun `v'.gph", replace 
}
*
*
* (2) assestment gráfico de soporte común
* --------
{ 
*  
capture drop _KM_*
*
quietly kmatch ps treatment_0 $depurated_control_set  /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
		att 
*di e(tvar)
}
*
{
local lb = "2 o mas sesiones de entrenamiento" 
kmatch summarize  $depurated_control_set
return list
mat Raw = r(M)
mat Matched = r(M)
coefplot matrix(Raw[,3]) matrix(Matched[,6]), ///
title(`lb') ///
subtitle(Diferencia estandarizadas ex-ante y post-hoc ) ///
xline(0) 
*
graph export "balance de muestra `v'.pdf", replace 
graph save "balance de muestra `v'.gph", replace 
}

*
* Automating reports generation from stata
* https://journals.sagepub.com/doi/pdf/10.1177/1536867X1301300104
