
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
* (0.1) crear una nueva evaluacion
*
*
cd "$EVALUATION_HOME"
cd "evaluaciones"
cd "$evaluation_datetime"

*
* (1) definicion formal de control tratamiento
*
*

*
cd "$EVALUATION_HOME"
dir
use "program evaluation dataset.dta", clear

*
label define generic_treatment_label 0 "control" 1 "treatment"

*
gen _entrevista=cond(entrevista!=., entrevista, 0)
gen _competencias_transversale=cond(competencias_transversale!=., competencias_transversale, 0)
gen _busqueda_de_empleo=cond(busqueda_de_empleo!=., busqueda_de_empleo, 0)
capture drop _cualquier_actividad
gen _cualquier_actividad=_entrevista+ _competencias_transversale+_busqueda_de_empleo
*
hist _cualquier_actividad, frequency


*

gen cualquier_actividad_1= cond(_cualquier_actividad>0,1,0)
gen cualquier_actividad_2= cond(_cualquier_actividad>1,1,0)
*
capture drop entrevista_*
gen entrevista_1=cond( entrevista!=.,cond(entrevista>=1,1,0),.)
gen entrevista_2=cond( entrevista!=.,cond(entrevista>=2,1,0),.)
*
capture drop competencias_transversale_*
gen competencias_transversale_1=cond( competencias_transversale!=.,cond(competencias_transversale>=1,1,0),.)
gen competencias_transversale_2=cond( competencias_transversale!=.,cond(competencias_transversale>=2,1,0),.)
*
capture drop busqueda_de_empleo_*
gen busqueda_de_empleo_1=cond( busqueda_de_empleo!=.,cond(busqueda_de_empleo>=1,1,0),.)
gen busqueda_de_empleo_2=cond( busqueda_de_empleo!=.,cond(busqueda_de_empleo>=2,1,0),.)
*
tabstat cualquier_actividad_* entrevista_* competencias_transversale_* busqueda_de_empleo_*, stats(sum count) columns(s)
* 



*
* (0.2) feature selection on control set as global
*
*

*
asdoc tabstat edad mujer migrante armed_conflict ethnic_group diverse_functionality, stats(mean sum count) columns(v) abb(40)
*
asdoc tabstat no_informado sin_bachillerato bachillerato_o_tecnico universitario_o_superior, stats(mean sum count) columns(v) abb(40)
*
asdoc tabstat experiencia certificacion idioma_extranjero competencias_digitales software empleo_al_registro, stats(mean sum count) columns(v) abb(40)



global control_set="edad mujer migrante armed_conflict ethnic_group diverse_functionality"
global control_set="$control_set sin_bachillerato bachillerato_o_tecnico universitario_o_superior"
global control_set="$control_set experiencia certificacion idioma_extranjero competencias_digitales software empleo_al_registro"
*global control_set="$control_set lab_*"
*global control_set="$control_set sal_* civil_* anos_experiencia"
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

*
tab alguna_colocacion
asdoc, text( \n ) fs(10), append save(reporte de evaluacion.doc)
asdoc, text( \n ) fs(10), append save(reporte de evaluacion.doc)
asdoc table alguna_colocacion, append save(reporte de evaluacion.doc)
*
foreach some_treatment in $treatment_set{
	di "`some_treatment'"
	*
	asdoc, text( \n ) fs(10), append save(reporte de evaluacion.doc)
	asdoc, text( \n ) fs(10), append save(reporte de evaluacion.doc)
	asdoc tabstat alguna_colocacion, by(`some_treatment') append save(reporte de evaluacion.doc)
}



*
* 1.1 Performing the program evaluation
*
*

*
*
di "$depurated_control_set"

*
*
global control_set="edad mujer"
global control_set="$control_set government_program condiciones discapacidades migrante"
global control_set="$control_set edu_*"
*global control_set="$control_set lab_*"
*global control_set="$control_set sal_* anos_experiencia"
*global control_set="$control_set sal_* civil_* anos_experiencia"
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
asdoc kmatch ps competencias_transversale $depurated_control_set /// treatment var, covariates to be matched/balanced
		(alguna_colocacion)
*
{
local lb = "2 o mas actividades de la ruta de empleabilidad" 
kmatch summarize  edad mujer migrante   armed_conflict    ethnic_group 
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
*
{
local lb = "2 o mas actividades de la ruta de empleabilidad" 
kmatch summarize  bachillerato_o_tecnico sin_bachillerato  universitario_o_superior  no_informado
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
{
local lb = "2 o mas actividades de la ruta de empleabilidad" 
kmatch summarize  empleo_al_registro experiencia certificacion  competencias_digitales software  idioma_extranjero
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
{
local lb = "2 o mas actividades de la ruta de empleabilidad" 
kmatch summarize  edad mujer migrante armed_conflict laboral_policies ethnic_group
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
kmatch ps  cualquier_actividad_1 $depurated_control_set /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
		att
*
kmatch ps  cualquier_actividad_2 $depurated_control_set /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
		att
		
		
		
*
asdoc kmatch ps  entrevista_1 $depurated_control_set /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
		att
*
asdoc kmatch ps  entrevista_2 $depurated_control_set /// treatment var, covariates to be matched/balanced
		(alguna_colocacion),	///
		att
		
		
*
*
kmatch ps  competencias_transversale_1 $depurated_control_set /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
		att
*
kmatch ps  competencias_transversale_2 $depurated_control_set /// treatment var, covariates to be matched/balanced
		(alguna_colocacion)	, ///
		att
		
kmatch ps  busqueda_de_empleo_1 $depurated_control_set /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
		att



*
kmatch ps  cualquier_actividad_1 $depurated_control_set /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
		att over(mujer)
*
kmatch ps entrevista_1 $depurated_control_set /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
		att over(mujer)
*
kmatch ps competencias_transversale_1 $depurated_control_set /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
		over(mujer)
*
kmatch ps busqueda_de_empleo_1 $depurated_control_set /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
		over(mujer)
		
		
*
kmatch ps  competencias_transversale $depurated_control_set /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
		att over(competencias_digitales)
*
*
kmatch ps  competencias_transversale $depurated_control_set /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
		att over(software)

		
{ 
*  
capture drop _KM_*
*
quietly kmatch ps cualquier_actividad_2 $depurated_control_set  /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
		att 
*di e(tvar)
}

{
*
matrix S=e(_N)
matrix list e(_N)
*
local s=S[1,1]
local _s= (S[1,1]/S[1,3])*100
local lb = "2 o mas actividades de la ruta de empleabilidad"
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
kmatch ps  cualquier_actividad_1 $depurated_control_set /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
		over(mujer) 
*
asdoc kmatch ps  entrevista_1 $depurated_control_set /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
		over(mujer)
*
asdoc kmatch ps  competencias_transversale_1 $depurated_control_set /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
		over(mujer)		
*	
*
*
asdoc kmatch ps   busqueda_de_empleo_1 $depurated_control_set /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
		over(mujer)	

asdoc kmatch ps   busqueda_de_empleo_1 $depurated_control_set /// treatment var, covariates to be matched/balanced
		(alguna_colocacion)
		
		

		
		
asdoc kmatch ps  entrevista_1 $depurated_control_set /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
		over(mujer) ///
		att append save(reporte de evaluacion.doc)
		
*
asdoc kmatch ps  entrevista_1 $depurated_control_set /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
		over(mujer) ///
		att append save(reporte de evaluacion.doc)
		
*
asdoc kmatch ps competencias_transversale_1 $depurated_control_set /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
		over(mujer) ///
		att append save(reporte de evaluacion.doc)
*
asdoc kmatch ps busqueda_de_empleo_1 $depurated_control_set /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
		over(mujer) ///
		att append save(reporte de evaluacion.doc)
		
*
asdoc kmatch ps  entrevista_2 $depurated_control_set /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
		att append save(reporte de evaluacion.doc)
*
asdoc kmatch ps competencias_transversale_2 $depurated_control_set /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
				over(mujer) ///
		att append save(reporte de evaluacion.doc)
*
asdoc kmatch ps busqueda_de_empleo_2 $depurated_control_set /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
		over(mujer) ///
		att append save(reporte de evaluacion.doc)
		




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
quietly kmatch ps competencias_transversale_1 $depurated_control_set  /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
		att 
*di e(tvar)
}
*

*
*
* (2) assestment gráfico de soporte común
* --------
{ 
*  
capture drop _KM_*
*
quietly kmatch ps entrevista_1 $depurated_control_set  /// treatment var, covariates to be matched/balanced
		(alguna_colocacion), ///
		att 
*di e(tvar)
}



*
{
local lb = "2 o mas actividades de la ruta de empleabilidad"  
kmatch summarize sal_menos_de_1_smmlv sal_1_smmlv sal_1_a_2_smmlv sal_2_a_4_smmlv sal_4_a_6_smmlv sal_6_a_9_smmlv
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
