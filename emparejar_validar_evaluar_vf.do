*
*  1. ingesta de datos (tabla personas)
* 

*
cls
clear
global APP_HOME="C:\Users\57320\Desktop\grade_cw"
cd "$APP_HOME"
*
dir
import excel "people_eval_data.xlsx", first clear
*br

* instalacion de librarias

ssc install kmatch

*
*  2. creacion de variables (dumificacion final pre-estimacion)
*

*
capture program drop ensemble_covars 
program define ensemble_covars
	capture drop _*
	tab genero_, gen(_sex_)
	tab edad_, gen(_age_)
	tab es_migrante_, gen(_migrant_) 
	tab estudios_, gen(_educ_) 
	*
	gen _experiencia_=ln(experiencia_+0.01) 
	gen _horas_certificacion_=ln(horas_certificacion_+0.01)
	*
	tab idioma_extranjero_, gen(_sec_lang_)  
	tab competencias_Digitales_, gen(_digital_competences_)  
	tab software_, gen(_software_)  
end
ensemble_covars

*
* colectamos la lista de controles en una unica variable
*

*
global independent_vars="_sex_1 _sex_2 _age_1 _age_2 _age_3 _age_4 _age_5 _age_6 _age_7 _age_8 _age_9 _age_10 _age_11 _migrant_1 _migrant_2 _educ_1 _educ_2 _educ_3 _educ_4 _educ_5 _experiencia_ _horas_certificacion_ _sec_lang_1 _sec_lang_2 _digital_competences_1 _digital_competences_2 _software_1 _software_2"
*   
*     
global independent_vars_labels=""                     
foreach covar of varlist $independent_vars {
	local varlabel : var label `covar'
	global independent_vars_labels="$independent_vars_labels `varlabel'"
}
*
di "$independent_vars_labels"

*
*  3. validacion de algoritmo de emparejamiento
*

*
* corremos un emparejamiento basado en la mayor intensidad de tratamiento
*

*
capture drop  _KM_* 
capture drop  _W_* 
kmatch  ps Min_3 _*, ///
generate wgenerate

*
* diferencia de promedios PRE emparejamiento
*

*
matrix R=.,.,.,.
global pars= "difference p_value lower_limit upper_limit"
*
foreach  outcome of varlist $independent_vars {
	di "`outcome'"
	quietly teffects ra ("`outcome'") (Min_3), ate
	*
	matrix M=r(table)
	matrix M=M[1,1],M[4,1],M[5,1],M[6,1]
	matrix R= R \ M 
}
*
matrix R= R[2...,1...] 
matrix rownames R = $independent_vars"
matrix colnames R = $pars
matrix list R
*
putexcel set "resultados", sheet("promedios_covariables_pre") replace		  
putexcel A1=matrix(R), names nformat(number_d2)

*
* diferencia de promedios POST emparejamiento
*

*
*
matrix R=.,.,.,.
global pars= "difference p_value lower_limit upper_limit"
*
foreach  outcome of varlist $independent_vars  {
	di "`outcome'"
	quietly teffects ra ("`outcome'") (Min_3) [pweight = _W_ATE], ate
	*
	matrix M=r(table)
	matrix M=M[1,1],M[4,1],M[5,1],M[6,1]
	matrix R= R \ M 
}
*
matrix R= R[2...,1...] 
matrix rownames R = $independent_vars
matrix colnames R = $pars
matrix list R
*
putexcel set "resultados", sheet("promedios_covariables_post")  modify  
putexcel A1=matrix(R), names nformat(number_d2)
*
*

*
* grafica de diferencias estandarizadas por covariable
*

*
capture drop  _KM_* 
capture drop  _W_* 
kmatch  ps Min_3 _*, ///
generate wgenerate
{
local lb = "Balance de muestra antes/despues de emparejar"
kmatch summarize  $independent_vars
return list
mat Antes = r(M)
mat Despues = r(M)
coefplot matrix(Antes[,3]) matrix(Despues[,6]), ///
title(Balance y emparejamiento) ///
subtitle(Diferencias antes y despues de emparejar) ///
xline(0)  scale(*.8) ///
caption(Nota: Los puntos son diferencias estandarizadas por cada covariable)
*
graph export "balance de muestra `v'.pdf", replace 
graph save "balance de muestra `v'.gph", replace  
}

*
* grafica de soporte común
*

*
{
*
matrix S=e(_N)
matrix list e(_N)
*

local lb = "Soporte común después de emparejamiento" 
*
local s=S[3,1]
local _s= (S[3,1]/S[3,3])*100
local s: di %5.0f `s'
local _s: di %5.2f `_s'
*
kmatch box, title( `lb' - `s' - `_s'% ) noout
*
graph export "soporte comun `v'.pdf", replace 
graph save "soporte comun `v'.gph", replace 
}

*
*  4. evaluacion
*

*
* ciclo de evaluación: 
* (1) ciclo externo sobre las intensidades de tratamiento
* (2) ciclo interno sobre las submuestras por sexo
* (3) se colecta automaticamente la inferencia estadística sobre los parámetros,
*     así como el tamaño de la muestra emparejada, por parámetro
*

*
matrix R=.,.,.,.,.,.,.
matrix RM=.,.,.,.,.,.,.
matrix RH=.,.,.,.,.,.,.
global pars= "gender difference p_value lower_limit upper_limit n % "
global intensities= "Min_1 Min_2 Min_3"
*
foreach  intensity of varlist $intensities {
    *
	di "`intensity'"
	*
	capture drop  _KM_* 
	capture drop  _W_* 
	quietly kmatch  ps "`intensity'" _*, ///
	generate wgenerate ///
	over(_sex_2)
	*
	matrix S=e(_N)
	*
	local w= S[3,1]
	local m= S[6,1]
	local n= S[3,1]+S[6,1]
	local w_= (S[3,1]/S[3,3])*100
	local m_= (S[6,1]/S[6,3])*100
	local n_= (S[3,1]+S[6,1])/ (S[3,3]+S[6,3]) *100
    *
	quietly teffects ra (colocacion_) ("`intensity'") [pweight = _W_ATE], ate
	matrix M=r(table)
	matrix M= 0, M[1,1],M[4,1],M[5,1],M[6,1],`n',`n_'
	matrix R= R \ M
	*
	quietly teffects ra (colocacion_) ("`intensity'") [pweight = _W_ATE] if genero_=="mujer", ate 
	matrix M=r(table)
	matrix M= 1, M[1,1],M[4,1],M[5,1],M[6,1],`m',`m_'
	matrix RM= RM \ M
	*
	quietly teffects ra (colocacion_) ("`intensity'") [pweight = _W_ATE] if genero_=="hombre", ate 
	matrix M=r(table)
	matrix M= 2, M[1,1],M[4,1],M[5,1],M[6,1],`w',`w_'
	matrix RH= RH \ M
}
*
matrix R= R[2...,1...] 
matrix rownames R = $intensities
matrix colnames R = $pars
*
matrix RM= RM[2...,1...] 
matrix rownames RM = $intensities
matrix colnames RM = $pars
*
matrix RH= RH[2...,1...] 
matrix rownames RH = $intensities
matrix colnames RH = $pars
matrix list RH
*
matrix R=R\RM\RH
matrix list R
*
putexcel set "resultados", sheet("resultados_experimento_1")  modify  
putexcel A1=matrix(R), names nformat(number_d3)

*
*  finaliza evaluacion sobre las personas
*

*
*  1. ingesta de datos (tabla postulaciones)
* 

*
cd "$APP_HOME"
dir
import excel "activities_eval_data.xlsx", first clear
*br
ensemble_covars

*
*  2. creacion de variables (dumificacion final pre-estimacion)
*

*
tab number_of_candidates_, gen(_number_of_candidates_) 
tab covid_phase, gen(_covid_phase_) 
gen _date_=date_
gen colocacion_=alguna_colocacion_

*
*  3. evaluacion
*

*
* ciclo de evaluación: 
* (1) ciclo externo sobre las intensidades de tratamiento
* (2) ciclo interno sobre las submuestras por sexo
* (3) se colecta automaticamente la inferencia estadística sobre los parámetros,
*     así como el tamaño de la muestra emparejada, por parámetro
* (4) una corrida independiente con tratamiento real, y otra con tratamiento ficticio (placebo)
*

*
* (tratamiento real)
*

*
matrix R=.,.,.,.,.,.,.
matrix RM=.,.,.,.,.,.,.
matrix RH=.,.,.,.,.,.,.
global pars= "gender difference p_value lower_limit upper_limit n % "
global intensities= "Min_1 Min_2 Min_3"
*
foreach  intensity of varlist $intensities {
    *
	di "`intensity'"
	*
	capture drop  _KM_* 
	capture drop  _W_* 
	kmatch  ps "`intensity'" _* year_, ///
	ematch(_covid_phase_*) ///
	generate wgenerate ///
	over(_sex_2)
	*
	matrix S=e(_N)
	*
	local w= S[3,1]
	local m= S[6,1]
	local n= S[3,1]+S[6,1]
	local w_= (S[3,1]/S[3,3])*100
	local m_= (S[6,1]/S[6,3])*100
	local n_= (S[3,1]+S[6,1])/ (S[3,3]+S[6,3]) *100
    *
	quietly teffects ra (colocacion_) ("`intensity'") [pweight = _W_ATE], ate
	matrix M=r(table)
	matrix M= 0, M[1,1],M[4,1],M[5,1],M[6,1],`n',`n_'
	matrix R= R \ M
	*
	quietly teffects ra (colocacion_) ("`intensity'") [pweight = _W_ATE] if genero_=="mujer", ate 
	matrix M=r(table)
	matrix M= 1, M[1,1],M[4,1],M[5,1],M[6,1],`m',`m_'
	matrix RM= RM \ M
	*
	quietly teffects ra (colocacion_) ("`intensity'") [pweight = _W_ATE] if genero_=="hombre", ate 
	matrix M=r(table)
	matrix M= 2, M[1,1],M[4,1],M[5,1],M[6,1],`w',`w_'
	matrix RH= RH \ M
}
*
matrix R= R[2...,1...] 
matrix rownames R = $intensities
matrix colnames R = $pars
*
matrix RM= RM[2...,1...] 
matrix rownames RM = $intensities
matrix colnames RM = $pars
*
matrix RH= RH[2...,1...] 
matrix rownames RH = $intensities
matrix colnames RH = $pars
matrix list RH
*
matrix R=R\RM\RH
matrix list R
*
putexcel set "resultados", sheet("resultados_experimento_2.a")  modify  
putexcel A1=matrix(R), names nformat(number_d3)

*
* (tratamiento ficticio)
*

*
matrix R=.,.,.,.,.,.,.
matrix RM=.,.,.,.,.,.,.
matrix RH=.,.,.,.,.,.,.
global pars= "gender difference p_value lower_limit upper_limit n % "
global fake_intensities= "Minf_1 Minf_2 Minf_3"
*
foreach  intensity of varlist $fake_intensities {
    *
	di "`intensity'"
	*
	capture drop  _KM_* 
	capture drop  _W_* 
	kmatch  ps "`intensity'" _* year_, ///
	ematch(_covid_phase_*) ///
	generate wgenerate ///
	over(_sex_2)
	*
	matrix S=e(_N)
	*
	local w= S[3,1]
	local m= S[6,1]
	local n= S[3,1]+S[6,1]
	local w_= (S[3,1]/S[3,3])*100
	local m_= (S[6,1]/S[6,3])*100
	local n_= (S[3,1]+S[6,1])/ (S[3,3]+S[6,3]) *100
    *
	quietly teffects ra (colocacion_) ("`intensity'") [pweight = _W_ATE], ate
	matrix M=r(table)
	matrix M= 0, M[1,1],M[4,1],M[5,1],M[6,1],`n',`n_'
	matrix R= R \ M
	*
	quietly teffects ra (colocacion_) ("`intensity'") [pweight = _W_ATE] if genero_=="mujer", ate 
	matrix M=r(table)
	matrix M= 1, M[1,1],M[4,1],M[5,1],M[6,1],`m',`m_'
	matrix RM= RM \ M
	*
	quietly teffects ra (colocacion_) ("`intensity'") [pweight = _W_ATE] if genero_=="hombre", ate 
	matrix M=r(table)
	matrix M= 2, M[1,1],M[4,1],M[5,1],M[6,1],`w',`w_'
	matrix RH= RH \ M
}
*
matrix R= R[2...,1...] 
matrix rownames R = $intensities
matrix colnames R = $pars
*
matrix RM= RM[2...,1...] 
matrix rownames RM = $intensities
matrix colnames RM = $pars
*
matrix RH= RH[2...,1...] 
matrix rownames RH = $intensities
matrix colnames RH = $pars
matrix list RH
*
matrix R=R\RM\RH
matrix list R
*
putexcel set "resultados", sheet("resultados_experimento_2.b")  modify  
putexcel A1=matrix(R), names nformat(number_d3)


*
*  4. evaluacion del impacto de covid
*

* ciclo de evaluación: 
* (0) post_covid_ pasa a ser el tratamiento de interees
* (1) ciclo externo sobre las intensidades de tratamiento
* (2) ciclo interno sobre las submuestras por sexo
* (3) se colecta automaticamente la inferencia estadística sobre los parámetros,
*     así como el tamaño de la muestra emparejada, por parámetro
* (4) una corrida independiente con tratamiento real, y otra con tratamiento ficticio (placebo)
*


*
matrix R=.,.,.,.,.
matrix RM=.,.,.,.,.
matrix RH=.,.,.,.,.
global pars= "gender difference p_value lower_limit upper_limit"
global intensities= "Min_0 Min_1 Min_2 Min_3"
*
capture drop _post_covid_*
capture drop  _KM_* 
capture drop  _W_* 
kmatch  ps   post_covid_ _sex_* _age_* _migrant_* _educ_* _experiencia_* _horas_certificacion_* _sec_lang_* _digital_competences_* _software_* _number_of_candidates_* , ///
ematch( past_activities) ///
generate wgenerate
*
foreach  intensity of varlist $intensities {
    *
	di "`intensity'"

    *
	quietly teffects ra (colocacion_) (post_covid_ ) [pweight = _W_ATE] if `intensity'==1, ate
	matrix M=r(table)
	matrix M= 0, M[1,1],M[4,1],M[5,1],M[6,1]
	matrix R= R \ M
	*
	quietly teffects ra (colocacion_) (post_covid_ ) [pweight = _W_ATE] if genero_=="mujer" & `intensity'==1, ate 
	matrix M=r(table)
	matrix M= 1, M[1,1],M[4,1],M[5,1],M[6,1]
	matrix RM= RM \ M
	*
	quietly teffects ra (colocacion_) (post_covid_ ) [pweight = _W_ATE] if genero_=="hombre" & `intensity'==1, ate 
	matrix M=r(table)
	matrix M= 2, M[1,1],M[4,1],M[5,1],M[6,1]
	matrix RH= RH \ M
}
*
matrix R= R[2...,1...] 
matrix rownames R = $intensities
matrix colnames R = $pars
*
matrix RM= RM[2...,1...] 
matrix rownames RM = $intensities
matrix colnames RM = $pars
*
matrix RH= RH[2...,1...] 
matrix rownames RH = $intensities
matrix colnames RH = $pars
matrix list RH
*
matrix R=R\RM\RH
matrix list R

*
putexcel set "resultados", sheet("resultados_experimento_3")  modify  
putexcel A1=matrix(R), names nformat(number_d2)
