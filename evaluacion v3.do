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
import excel "labelled sample.xlsx", first clear
count

*
* 0.1. emembling binary treatment vars
*

* dont
*
foreach var in activ_* {
	di "`var'"
}
* do
*
foreach var of varlist activ_* {
	di  "`var'"
}

*
* creating treatment vars with a variable threshold
*

*
* (cuidado con la critica de lina)
*

foreach var of varlist activ_* {
	di  "`var'"
		foreach threshold of numlist 1/4 {
		capture drop `var'_`threshold'
		quietly gen `var'_`threshold'=cond(`var'>=`threshold',1,.)
		quietly replace `var'_`threshold'=0 if activ_cualquiera ==0
		}
	tabstat `var'_*, stat(sum n)
	}
*

*
* 0.2 emembling control set
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
* 0.3 ensembling outcomes
*

capture drop  colocacion
gen colocacion=cond(num_contrataciones>0,1,0)
*sum colocacion
*gen pre_colocacion=cond(pre_num_contrataciones>0,1,0)
*gen post_colocacion=cond(post_num_contrataciones>0,1,0)

*
* 0.4 ensembling moderators
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
* 1. main results
*

*
*
kmatch ps  activ_cualquiera_1 $dem $hv $other ///
(colocacion), ///
ematch(mujer es_migrante _laboral_states) ///
att
*
matrix R = r(table)
matrix R = R[1,1...] \ R[4,1...]\ R[7,1...]
*
kmatch ps  activ_cualquiera_2 $dem $hv $other ///
(colocacion), ///
ematch(mujer es_migrante _laboral_states) ///
att
*
matrix new_R=r(table)
matrix new_R = new_R[1,1...] \ new_R[4,1...]\ new_R[7,1...]
matrix R = R\ new_R
*
kmatch ps  activ_cualquiera_3 $dem $hv $other ///
(colocacion), ///
ematch(mujer es_migrante _laboral_states) ///
att
*
matrix new_R=r(table)
matrix new_R = new_R[1,1...] \ new_R[4,1...]\ new_R[7,1...]
matrix R = R\ new_R
*
display "`e(cmdline)'"
*
cd "$RESULTADOS_HOME"
putexcel set "results.xlsx", sheet("main results")replace
putexcel  A2=matrix(R),  rownames 

*
* 1.2 gendered
*

*
*
kmatch ps  activ_cualquiera_1 $dem $hv $other ///
(colocacion), ///
ematch(mujer es_migrante _laboral_states) ///
over(mujer) ///
att
*
matrix R = r(table)
matrix R = R[1,1...] \ R[4,1...]\ R[7,1...]
*
kmatch ps  activ_cualquiera_2 $dem $hv $other ///
(colocacion), ///
ematch(mujer es_migrante _laboral_states) ///
over(mujer) ///
att
*
matrix new_R=r(table)
matrix new_R = new_R[1,1...] \ new_R[4,1...]\ new_R[7,1...]
matrix R = R\ new_R
*
kmatch ps  activ_cualquiera_3 $dem $hv $other ///
(colocacion), ///
ematch(mujer es_migrante _laboral_states) ///
over(mujer) ///
att
*
matrix new_R=r(table)
matrix new_R = new_R[1,1...] \ new_R[4,1...]\ new_R[7,1...]
matrix R = R\ new_R
*
display "`e(cmdline)'"
*
cd "$RESULTADOS_HOME"
putexcel set "results.xlsx", sheet("gender results") modify
putexcel  A2=matrix(R),  rownames 

*
* 1.3 moderated
*

*
*
kmatch ps  activ_cualquiera_3 $dem $hv $other ///
(colocacion), ///
ematch(mujer es_migrante _laboral_states) ///
over( _estudios ) ///
att
*
matrix R = r(table)
matrix R = R[1,1...] \ R[4,1...]\ R[7,1...]
matrix list R
*
cd "$RESULTADOS_HOME"
putexcel set "results.xlsx", sheet("het, education") modify
putexcel  A2=matrix(R),  rownames 
*
*

*
*
kmatch ps  activ_cualquiera_3 $dem $hv $other ///
(colocacion), ///
ematch(mujer es_migrante _laboral_states) ///
over( _laboral_states  ) ///
att
*
matrix R = r(table)
matrix R = R[1,1...] \ R[4,1...]\ R[7,1...]
matrix list R
*
cd "$RESULTADOS_HOME"
putexcel set "results.xlsx", sheet("het, education") modify
putexcel  A2=matrix(R),  rownames 
*
*

*
*
replace grupo_edad=5 if grupo_edad==6
kmatch ps  activ_cualquiera_3 $dem $hv $other ///
(colocacion), ///
ematch(mujer es_migrante _laboral_states) ///
over( grupo_edad ) ///
att
*
matrix R = r(table)
matrix R = R[1,1...] \ R[4,1...]\ R[7,1...]
matrix list R
*
cd "$RESULTADOS_HOME"
putexcel set "results.xlsx", sheet("het, education") modify
putexcel  A2=matrix(R),  rownames 
*
*

*
* 1.4 multiple treatment
*

*
levelsof treatment_case, local(levels) 
local sheet_counter=0
foreach case of local levels {
	preserve
	quietly keep if treatment_case=="`case'"
	count
	if (`r(N)' < 50) {
	restore
	continue
	}
	if (treatment_case=="control") {
	restore
	continue
	}
	restore
	*
	capture drop case_treat
	quietly gen case_treat=.
	quietly replace case_treat=1 if treatment_case=="`case'"
	quietly replace case_treat=0 if treatment_case=="control"
	quietly table case_treat
    *
	di "`case'"
	*
	quietly  kmatch ps  case_treat $dem $hv $other ///
	(colocacion), ///
	ematch(mujer es_migrante _laboral_states) ///
	over(mujer) ///
	att
	*
	matrix R = r(table)
	matrix R = R[1,1...] \ R[4,1...]\ R[7,1...]
	matrix list R
	*
	local sheet_counter=`sheet_counter'+1
	cd "$RESULTADOS_HOME"
	di "`case'"
	putexcel set "results.xlsx", sheet("caso `sheet_counter'") modify
	putexcel  A1="`case'"
	putexcel  A2=matrix(R),  rownames 
	}

*
* 1.6 covid design
*





*
*  other outcomes
*

*
kmatch ps  activ_cualquiera_3 $dem $hv ///
(salarios_altos), ///
ematch(mujer es_migrante) ///
over(mujer) ///
att
*
matrix R = r(table)
*
cd "$RESULTADOS_HOME"
putexcel set "results.xlsx", sheet("out, altos salarios") modify
putexcel  A2=matrix(R),  rownames 
*
kmatch ps  activ_cualquiera_3 $dem $hv ///
(contratos_permanentes), ///
ematch(mujer es_migrante) ///
over(mujer) ///
att
*
cd "$RESULTADOS_HOME"
putexcel set "results.xlsx", sheet("out, permanentes") modify
putexcel  A2=matrix(R),  rownames 
*

*
*  covid design
*

*
kmatch ps  activ_cualquiera_3 $dem $hv  ///
(pre_colocacion), ///
ematch(mujer es_migrante) ///
over(mujer) ///
att
*
matrix R = r(table)
*
kmatch ps  activ_cualquiera_3 $dem $hv  ///
(post_colocacion), ///
ematch(mujer es_migrante) ///
over(mujer) ///
att
*
matrix R = R,r(table)
*
cd "$RESULTADOS_HOME"
putexcel set "results.xlsx", sheet("diseño covid") modify
putexcel  A2=matrix(R),  rownames 

table mujer  activ_cualquiera_3
table estudios   activ_cualquiera_3
table laboral_states  activ_cualquiera_3
table laboral_states  activ_cualquiera_3
table grupo_edad activ_cualquiera_3


preserve
keep if covid_design=="nuevo desempleado"
*
kmatch ps  activ_cualquiera_2 $dem $hv  ///
(post_colocacion), ///
ematch(mujer es_migrante) ///
over(mujer) ///
att
*
restore
