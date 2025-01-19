
*
*
* program evaluation for intermediation services
*

/*

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
global EVALUATION_HOME="C:\Users\josio\OneDrive - Fedesarrollo\proyecto grade\repositorios de datos\matriz evaluacion"
global RESULTADOS_HOME="C:\Users\josio\OneDrive - Fedesarrollo\proyecto grade\resultados evaluacion"

*
* 1. load evaluation matriz
*
*

cd "$EVALUATION_HOME"
dir
use "matriz de evaluacion.dta", clear

*
*

*
* dont
*
foreach var in activ_* {
	di "`var'"
}
*
* do
*
foreach var of varlist activ_* {
	di  "`var'"
}


*
capture drop *_1
capture drop *_2
capture drop *_3 
*
foreach var of varlist activ_* {
	di  "`var'"
	gen `var'_1=cond(`var'>=1,1,.)
	replace `var'_1=0 if grupo_control==1
	gen `var'_2=cond(`var'>=2,1,.)
	replace `var'_2=0 if grupo_control==1
	gen `var'_3=cond(`var'>=3,1,.)
	replace `var'_3=0 if grupo_control==1
*
table `var' `var'_1
table `var' `var'_2
table `var' `var'_3
}


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

encode covid_design, gen(_covid_design)
*
*
capture drop   colocacion
gen colocacion=cond(num_contrataciones>0,1,0)
gen pre_colocacion=cond(pre_num_contrataciones>0,1,0)
gen post_colocacion=cond(post_num_contrataciones>0,1,0)

*
global dem="edad mujer est_* es_migrante lab_*"
global hv="experiencia certificacion idioma_extranjero competencias_digitales software"

*
kmatch ps  activ_cualquiera_3 $dem $hv  ///
(colocacion), ///
ematch(mujer es_migrante) ///
over(mujer) ///
att
*
kmatch ps  activ_cualquiera_3 $dem $hv  ///
(pre_colocacion), ///
ematch(mujer es_migrante) ///
over(mujer) ///
att
*
kmatch ps  activ_cualquiera_3 $dem $hv  ///
(post_colocacion), ///
ematch(mujer es_migrante) ///
over(mujer) ///
att
*
tostring mujer, gen(s_mujer)
gen sample=s_mujer+covid_design
encode sample, gen(_sample)
*
kmatch ps  activ_cualquiera_1 $dem $hv  ///
(post_colocacion), ///
ematch(mujer es_migrante) ///
over(_sample) ///
att




