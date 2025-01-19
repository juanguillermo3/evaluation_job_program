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

*
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
import excel "final_evalaution_sample.xlsx", first clear
count

*
* 0.1. control set
*

program feature_engineering

	*
	capture drop mujer est_* lab_*
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
	capture drop year_* 
	capture drop post_covid_* 
	capture drop ln_momento_registro*
	*
	tostring year, replace
	tab year, gen(year_)
	tab actividad_post_covid, gen(period_)
	gen ln_momento_registro=log(momento_registro)
	*
	global other="ln_momento_registro year_* period_1"
	quietly tabstat $other
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
	table _estudios
	
	*
    capture drop  colocacion
	gen colocacion=cond(num_contrataciones>0,1,0)
end
feature_engineering

global treatments="al_menos_*"






matrix R= .\.\.
*
foreach treatment_col of varlist  $treatments  {

    *
	di "`treatment_col'"
	*
	kmatch ps  `treatment_col' $dem $hv $other ///
	(colocacion), ///
	ematch(mujer es_migrante _laboral_states) ///
	att
	*
	matrix r = r(table)
	matrix s= e(_N)
	matrix r=r[1,1]\r[4,1]\s[1,1]
	matrix R=R,r
	matrix list R
	*
	kmatch ps `treatment_col' $dem $hv $other ///
	(colocacion), ///
	ematch(mujer es_migrante _laboral_states) ///
	over(mujer) ///
	att
	*
	matrix r = r(table)
	matrix s= e(_N)
	matrix r=r[1,1...]\r[4,1...]\s[1,1],s[2,1]
	matrix R=R,r
	matrix list R
}
*
kmatch ps  al_menos_1 $dem $hv $other ///
(colocacion), ///
ematch(mujer es_migrante _laboral_states) ///
o
att

matrix list e(_N)

table  sexo al_menos_2 actividad_post_covid


*
capture program drop slice_results
program slice_results
	matrix R = r(table)
	matrix R = R[1,1...] \ R[4,1...]
	matrix list R
end
slice_results
*
capture program drop  complement_results
program complement_results
    matrix samples = e(_N)
	matrix samples = samples[1,1],samples[2,1]
	matrix R = R \ samples
	matrix list R
end
complement_results

slice_results
complement_results


*
* 1. main results
*

*
kmatch ps  al_menos_3 $dem $hv $other ///
(colocacion), ///
ematch(mujer es_migrante _laboral_states) ///
over(mujer) ///
att
*
capture program drop slice_results
program slice_results
	matrix R = r(table)
	matrix R = R[1,1...] \ R[4,1...]
	matrix list R
end
slice_results
*
capture program drop  complement_results
program complement_results
    matrix samples = e(_N)
	matrix samples = samples[1,1],samples[2,1]
	matrix R = R \ samples
	matrix list R
end
complement_results
*

*
* 2. frequency
*

*
* 2.1 moderated
*

*
gen generoxeduc=genero+estudios 
encode generoxeduc, gen(_generoxeduc)

*
*
kmatch ps  al_menos_2 $dem $hv $other ///
(colocacion), ///
ematch(mujer es_migrante _laboral_states) ///
over(_generoxeduc ) ///
att
*
matrix samples = e(_N)
matrix samples=samples'
matrix R = r(table)
matrix R = R[1,1...] \ R[4,1...]\ samples[1,1...]
matrix list R
*
cd "$RESULTADOS_HOME"
putexcel set "results.xlsx", sheet("het, education") modify
putexcel  A2=matrix(R),  rownames 
*
*


gen phase_placements=pre_alguna_contratacion+post_alguna_contratacion
*
*

drop _actividad_post_covid
gen _actividad_post_covid=
 cond(actividad_post_covid=="post-covid", 1, 0)


*
capture drop pre_covid
gen pre_covid=.
replace pre_covid=0 if actividad_post_covid=="post-covid"
replace pre_covid=1 if actividad_post_covid=="pre-covid"

*
kmatch ps  pre_covid $dem $hv ///
  (num_contrataciones) if mujer==1, ///
  ematch( mujer es_migrante _laboral_states _estudios) ///
  over(al_menos_3) ///
  att
*
kmatch ps  pre_covid $dem $hv ///
  (num_contrataciones) if mujer==0, ///
  ematch( mujer es_migrante _laboral_states _estudios) ///
  over(al_menos_3) ///
  att
  


*
* 3.0 evaluation of covid design
*

  
*
* 3.1 control set
*

*
cd "$EVALUATION_HOME"
dir
import excel "covid_paula_design_eval_sample.xlsx", first clear
count


*
capture drop mujer est_* lab_*
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
capture drop year_* 
capture drop post_covid_* 
capture drop ln_momento_registro*
*
tostring year, replace
tab year, gen(year_)
tab actividad_post_covid, gen(period_)
gen ln_momento_registro=log(momento_registro)
*
global other="ln_momento_registro year_* period_1"
quietly tabstat $other
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
table _estudios

*
replace phase_placements=0 if phase_placements==.
gen _phase_placements=cond(phase_placements>0, 1,0)



*
quietly reg _phase_placements dd_post_covid dd_Treatment_1 dd_Post_CovidXTreatment_1 $dem $hv, noc
*
matrix R=r(table)
matrix R=R[1, 3]\R[4, 3]\R[7, 3]
matrix list R
*
quietly reg _phase_placements dd_post_covid dd_Treatment_1 dd_Post_CovidXTreatment_1  $dem $hv if sexo =="hombre" ///
, noc
acum
*
quietly reg _phase_placements dd_post_covid dd_Treatment_1 dd_Post_CovidXTreatment_1  $dem $hv if sexo =="mujer" ///
, noc
*
program drop acum
program acum
	matrix r=r(table)
	matrix r=r[1, 3]\r[4, 3]\r[7, 3]
	matrix R=R,r
end
acum


*
quietly reg _phase_placements dd_post_covid dd_Treatment_2 dd_Post_CovidXTreatment_2 $dem $hv , noc
acum
*
quietly  reg _phase_placements dd_post_covid dd_Treatment_2 dd_Post_CovidXTreatment_2  $dem $hv if sexo =="hombre" ///
, noc
acum
*
quietly reg _phase_placements dd_post_covid dd_Treatment_2 dd_Post_CovidXTreatment_2  $dem $hv if sexo =="mujer" ///
, noc
acum


*
quietly reg _phase_placements dd_post_covid dd_Treatment_3 dd_Post_CovidXTreatment_3 $dem $hv , noc
acum
*
quietly  reg _phase_placements dd_post_covid dd_Treatment_3 dd_Post_CovidXTreatment_3  $dem $hv if sexo =="hombre" ///
, noc
acum
*
quietly reg _phase_placements dd_post_covid dd_Treatment_3 dd_Post_CovidXTreatment_3  $dem $hv if sexo =="mujer" ///
, noc
acum

