
*
* main psm estimators for the  marginal effect of intermediation
*

*
* sys setup
*

macro drop _all
cls

*
* routing
*

global EVALUATION_HOME="C:\Users\josio\OneDrive - Fedesarrollo\grade_resultados_evaluacion"

*
* data (evaluation matrix)
*

cd "$EVALUATION_HOME"
dir
import delimited "Final_Evaluation_data.csv", encoding("UTF-8") clear
count

*
* feature engineering on covariates
*

*
capture drop __*
foreach covariate of varlist *_ {
	di "`covariate'"
	quietly tab `covariate', gen(__`covariate')
}
*
capture drop Post_date
encode post_date, gen(Post_date)
*
capture drop es_mujer*
gen es_mujer=cond(__genero_2==1,"si","no")
encode es_mujer, gen(es_mujer_)
*
capture drop CovidXGender* 
gen CovidXGender=post_date+es_mujer
encode CovidXGender, gen(CovidXGender_)

*
* charting treatment endogeneity
*

label define estudios_levs  1 "Hasta_Secundaria_Basica"  2  "Educación_Tecnológica"  3 "Bachillerato"  4 "Universitaria/Superior"  5 "Desconocida"
encode estudios_, label(estudios_levs) gen(Estudios_)


ciplot past_activities, by(Estudios_)  xlabel(, labsize(tiny))  saving(f1____)
ciplot past_activities, by(migracion_)  xlabel(, labsize(vsmall)) saving(f2___)
ciplot past_activities, by(idioma_extranjero_)xlabel(, labsize(vsmall))  saving(f5__)
ciplot past_activities, by(competencias_digitales)xlabel(, labsize(vsmall)) saving(f7__)

graph combine f1____.gph f2___.gph f5__.gph f7__.gph, /// 
 title("Actividades promedio por valores de covariable")


*
* ensembling treatment var (and placebos)
*

*
hist past_activities
capture drop main_experiment_*
gen main_experiment_1=.
replace  main_experiment_1=0 if past_activities==0 & experimental_case =="control"
replace  main_experiment_1=1 if past_activities>0 & experimental_case == "treated"
*
capture drop dummy_experiment_*
gen dummy_experiment_1=.
replace  dummy_experiment_1=0 if past_activities==0 & experimental_case =="control"
replace  dummy_experiment_1=1 if future_activities>0 &  experimental_case=="placebo"

*
table main_experiment_1
table dummy_experiment_1


*
* ensembling treatment var
*

*
foreach covariate of varlist _* {
	di "`covariate'"
}
*
foreach covariate of varlist __* {
	di "`covariate'"
}
*
table outcome

*
* main experiment (diagnostics)
*

*
capture drop  _estudios_
destring _ln_number_of_candidates, replace force
kmatch ps  main_experiment_1 __* ///
(outcome), ///
ematch( __genero_* __estudios_* year_*  Post_date)

{
local lb = "Balance de muestra antes/despues de emparejar"
kmatch summarize _*
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
{
*
matrix S=e(_N)
matrix list e(_N)
*
local s=S[1,1]
local _s= (S[1,1]/S[1,3])*100
local lb = "Soporte común después de emparejamiento" 
*
local s: di %5.0f `s'
local _s: di %5.2f `_s'
*
kmatch box, title( `lb' - `s' - `_s'% ) noout
*
graph export "soporte comun `v'.pdf", replace 
graph save "soporte comun `v'.gph", replace 
}


*
kmatch ps  main_experiment_1 __* ///
(outcome), ///
att ///
ematch( __genero_* __estudios_* year_*  Post_date)
*
kmatch ps  dummy_experiment_1 __* ///
(outcome), ///
att ///
ematch( __genero_* __estudios_* year_*  Post_date)

*
* with moderation
*

*
kmatch ps  main_experiment_1 __* ///
(outcome), ///
ematch( __genero_* __estudios_* year_*  Post_date) ///
over(es_mujer_)
*
kmatch ps  main_experiment_1 __* ///
(outcome), ///
ematch( __genero_* __estudios_* year_*  Post_date) ///
over(Post_date)
*
kmatch ps  main_experiment_1 __* ///
(outcome), ///}
att ///
ematch( __genero_* __estudios_* ___migracion_*  Post_date) ///
over(CovidXGender_)



