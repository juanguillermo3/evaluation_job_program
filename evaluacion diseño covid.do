*
* main psm estimator for the  marginal effect of intermediation
*

/*

*/


*
* sys setup
*

macro drop _all
cls

* fecha
* routing
*

global APP_HOME="C:\Users\josio\OneDrive - Fedesarrollo\proyecto grade"
global EVALUATION_HOME="C:\Users\josio\OneDrive - Fedesarrollo\proyecto grade\repositorios de datos\labelled sample"
global RESULTADOS_HOME="C:\Users\josio\OneDrive - Fedesarrollo\proyecto grade\resultados evaluacion"

*
* data (evaluation matrix)
*

cd "$EVALUATION_HOME"
dir
import excel "covid design sample by paula.xlsx", first clear
count


*
* control set
*
gen mujer=cond(genero=="mujer",1,0)
tab estudios, gen(est_)
tab laboral_states, gen(lab_)
*
global dem="edad mujer est_* es_migrante lab_*"
quietly tabstat $dem
*
global hv="experiencia certificacion idioma_extranjero competencias_digitales software"
quietly tabstat $hv
*
tab ano_registro, gen(year_)
global other="previous_activities year_*"
quietly tabstat $other

*
* moderators
*

*
*
capture drop _estudios
capture drop _laboral_states
capture drop grupo_edad 
capture drop _grupo_edad
*
encode estudios , gen(_estudios)
encode laboral_states, gen(_laboral_states)
egen grupo_edad=cut(edad), at(10(10)120) label
replace grupo_edad=6 if edad>=70


*
* evaluation on placements
*

*
reg phase_placements dd_post_covid dd_Treatment_1 dd_Post_CovidXTreatment_1 $dem $hv $other
reg phase_placements dd_post_covid dd_Treatment_2 dd_Post_CovidXTreatment_2 $dem $hv $other ///
if sexo =="mujer" 

*
reg phase_placements dd_post_covid dd_Treatment_2 dd_Post_CovidXTreatment_2 $dem $hv $other ///
if sexo =="hombre" 
reg phase_placements dd_post_covid dd_Treatment_2 dd_Post_CovidXTreatment_2 $dem $hv $other ///
if sexo =="mujer" 
*

*
reg phase_placements dd_post_covid dd_Treatment_3 dd_Post_CovidXTreatment_3 $dem $hv $other if ///
sexo =="mujer" 
*
reg phase_placements dd_post_covid dd_Treatment_3 dd_Post_CovidXTreatment_3 $dem $hv $other if ///
sexo =="hombre"
*

*
* evaluation on partial outcomes
*

local partial_outcome="phase_placements"
*di "`partial_outcome' results.doc"
*
dir
capture erase "`partial_outcome' results.doc"
capture erase "`partial_outcome' results.txt"
*
*
foreach sex in "mujer" "hombre" {
	foreach treatment_intensity of numlist 1/3 {
	capture drop dd_Post_CovidXTreatment
	gen dd_Post_CovidXTreatment=dd_Post_CovidXTreatment_`treatment_intensity'
	quietly reg `partial_outcome' dd_post_covid dd_Treatment_`treatment_intensity' dd_Post_CovidXTreatment $dem $hv $other ///
		if sexo =="`sex'"
	outreg2 using "`partial_outcome' results.doc", ///
	title(Resultados sobre `partial_outcome') ctitle("`sex' `treatment_intensity'") ///
	append keep(dd_Post_CovidXTreatment) label pvalue nocons
	*di "`sex'" `treatment_intensity'
	}
}



foreach  partial_outcome of varlist phase_*{
*
dir
capture erase "`partial_outcome' results.doc"
capture erase "`partial_outcome' results.txt"
*
*
foreach sex in "mujer" "hombre" {
	foreach treatment_intensity of numlist 1/3 {
	capture drop dd_Post_CovidXTreatment
	gen dd_Post_CovidXTreatment=dd_Post_CovidXTreatment_`treatment_intensity'
	quietly reg `partial_outcome' dd_post_covid dd_Treatment_`treatment_intensity' dd_Post_CovidXTreatment $dem $hv $other ///
		if sexo =="`sex'"
	outreg2 using "`partial_outcome' results.doc", ///
	title(Resultados `partial_outcome') ctitle("`sex' `treatment_intensity'") ///
	append keep(dd_Post_CovidXTreatment) label pvalue nocons 
	*di "`sex'" `treatment_intensity'
	}
}
}

reg phase_postulaciones dd_post_covid dd_Treatment_`treatment_intensity' dd_Post_CovidXTreatment $dem $hv $other


encode gender, gen(_gender)

*
kmatch ps dd_post_covid $dem $hv $other ///
(phase_postulaciones), /// 
ematch(mujer es_migrante _laboral_states activ_cualquiera) ///
over(_gender) ///
att
*
kmatch ps dd_post_covid $dem $hv $other ///
(phase_auto_postulaciones), /// 
ematch(mujer es_migrante _laboral_states activ_cualquiera) ///
over(_gender) ///
att
*
kmatch ps dd_post_covid $dem $hv $other ///
(phase_remisiones), /// 
ematch(mujer es_migrante _laboral_states activ_cualquiera) ///
over(_gender) ///
att
*


local partial_outcome="phase_remisiones"
*di "`partial_outcome' results.doc"
*
dir
capture erase "`partial_outcome' results.doc"
capture erase "`partial_outcome' results.txt"
*
*
foreach sex in "mujer" "hombre" {
	foreach treatment_intensity of numlist 1/3 {
	capture drop dd_Post_CovidXTreatment
	gen dd_Post_CovidXTreatment=dd_Post_CovidXTreatment_`treatment_intensity'
	quietly reg `partial_outcome' dd_post_covid dd_Treatment_`treatment_intensity' dd_Post_CovidXTreatment $dem $hv $other ///
		if sexo =="`sex'"
	outreg2 using "`partial_outcome' results.doc", ///
	title(Resultados sobre `partial_outcome') ctitle("`sex' `treatment_intensity'") ///
	append keep(dd_Post_CovidXTreatment) label pvalue nocons
	*di "`sex'" `treatment_intensity'
	}
}








local partial_outcome="phase_postulaciones"
*di "`partial_outcome' results.doc"
*
dir
capture erase "`partial_outcome' results.doc"
capture erase "`partial_outcome' results.txt"
*
*
foreach sex in "mujer" "hombre" {
	foreach treatment_intensity of numlist 1/3 {
	capture drop dd_Post_CovidXTreatment
	gen dd_Post_CovidXTreatment=dd_Post_CovidXTreatment_`treatment_intensity'
	quietly reg phase_auto_postulaciones dd_post_covid dd_Treatment_`treatment_intensity' dd_Post_CovidXTreatment $dem $hv $other ///
		if sexo =="`sex'"
	outreg2 using "`partial_outcome' results.doc", title(Resultados sobre `partial_outcome') ctitle("`sex' `treatment_intensity'") append keep(dd_Post_CovidXTreatment)
	*di "`sex'" `treatment_intensity'
	}
}

*


