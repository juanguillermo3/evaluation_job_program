
*
*
* program evaluation for the ruta de la empleabilidad
*

/*

*/

*
* 0. some set-up for stata
*

macro drop _all
cls

*
* 0.1 routing
*

global APP_HOME="C:\Users\josio\OneDrive - Fedesarrollo\proyecto grade"
global ASSESTMENT_HOME="C:\Users\josio\OneDrive - Fedesarrollo\proyecto grade\repositorios de datos\assestment"

*
* 1.0 data-environment for the evaluation
*
*

cd "$ASSESTMENT_HOME"
dir
import excel "tabla_para_validar_balance_de_muestra.xlsx", first clear

*
* 1.1
*

*
label define generic_treatment_label 0 "control" 1 "treatment"
label values al_menos_4 generic_treatment_label

*
* 1.2 feature selection on control set as global
*

*
tab estudios, gen(est_)
tab genero, gen(sexo_)
tab laboral_states, gen(estado_)

*
drop if year<2016
capture drop year_* 
capture drop post_covid_* 
capture drop ln_momento_registro*
tostring year, replace
tab year, gen(year_)
tab actividad_post_covid, gen(period_)
gen ln_momento_registro=log(momento_registro)

*
global control_set="edad  sexo_* es_migrante"
global control_set="$control_set year_* period_* ln_momento_registro"
global control_set="$control_set experiencia certificacion idioma_extranjero competencias_digitales software"
global depurated_control_set="$control_set"

*
* 2. propensity score matching
*

*
quietly kmatch ps al_menos_4 $depurated_control_set, ///
ematch(  es_migrante est_*  estado_* period_* year_*) ///
att
 
*
cd "$ASSESTMENT_HOME"
*
{
local lb = "Balance de muestra antes/despues de emparejar"
kmatch summarize  
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

*
* 3. graphical tests
*

*
cd "$ASSESTMENT_HOME"
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
* 4. numerical tests
*


*
cd "$ASSESTMENT_HOME"
dir
*
capture gen muestra_completa="Muestra completa"
*
replace  estudios="desconocida y hasta basica secundaria" if estudios=="desconocida, hasta basica secundaria"

*
global control_set_table="edad es_migrante est_*"
global control_set_table="$control_set_table period_1 estado_1 ln_momento_registro"
global control_set_table="$control_set_table experiencia certificacion idioma_extranjero competencias_digitales software"

*
global subsample_factors="muestra_completa estudios"
global eval_treatment=al_menos_3
*
cd "$ASSESTMENT_HOME"
shell rmdir "diagnostics_output" /s
shell mkdir "diagnostics_output"
cd "diagnostics_output"
*
foreach subsample_factor in $subsample_factors{
    *
	quietly levelsof sexo, local(levs_sexo)
	foreach lev_sexo in `levs_sexo'{
	
	    *
		quietly levelsof `subsample_factor', local(subsamples)
		foreach subsample in `subsamples'{
		
                *
				di "`lev_sexo'"
				di "`subsample_factor'"
				di  "`subsample'"
				
				*
				capture drop _KM_*
				quietly kmatch ps al_menos_3 $depurated_control_set if sexo=="`lev_sexo'" & `subsample_factor'=="`subsample'", ///
				ematch(  es_migrante est_*  estado_* period_* year_*) ///
				generate ///
				att
				capture drop  psm
				gen psm=_KM_ps
				
				*
				matrix S=e(_N)
				local s=S[1,1]
				di "`s'"
				
				*
				quietly reg  al_menos_3 psm $control_set_table if sexo=="`lev_sexo'" & `subsample_factor'=="`subsample'"
				outreg2 using  "`subsample_factor'", append nocons ti(Regresiones auxiliares en `subsample_factor') cti("`lev_sexo'" "`subsample'") label ///
                word excel sortvar(psm) addtext(Matched in treatment, "`s'") 
				}
		}
}

*
* diagnostico de emparejamiento para la comparacion antes/despues
*

table sexo actividad_post_covid  al_menos_3 

*
capture drop pre_covid
gen pre_covid=.
replace pre_covid=0 if actividad_post_covid=="post-covid"
replace pre_covid=1 if actividad_post_covid=="pre-covid"


*
global control_set_pre_post="sexo_2 edad es_migrante est_*"
global control_set_pre_post="$control_set_pre_post estado_1"
global control_set_pre_post="$control_set_pre_post experiencia certificacion idioma_extranjero competencias_digitales software"

*
cd "$ASSESTMENT_HOME"
cd "diagnostics_output"
*
capture rm "before after comparison.rtf"
capture rm "before after comparison.txt"
capture rm "before after comparison.xml"
*
quietly levelsof sexo, local(levs_sexo)
foreach lev_sexo in `levs_sexo'{

	*
	quietly levelsof al_menos_3, local(subsamples)
	foreach subsample in `subsamples'{
	
			*
			di "`lev_sexo'"
			di  "`subsample'"
			
			*
			capture drop _KM_*
			quietly kmatch ps pre_covid $control_set_pre_post if sexo=="`lev_sexo'" & al_menos_3==`subsample', ///
			ematch(  es_migrante est_*  estado_* year_*) ///
			generate ///
			att
			capture drop  psm
			gen psm=_KM_ps
			
			*
			matrix S=e(_N)
			local s=S[1,1]
			di "`s'"
			
			*
			quietly reg  pre_covid psm $control_set_pre_post if sexo=="`lev_sexo'" & al_menos_3==`subsample'
			outreg2 using  "before after comparison", append nocons ti(Regresiones auxiliares en al_menos_3) cti("`lev_sexo'" "`subsample'") label ///
			word excel sortvar(psm) addtext(Matched in treatment, "`s'") 
			}
	}

*
global LABELLED_SAMPLE_HOME="C:/Users/josio/OneDrive - Fedesarrollo/proyecto grade/repositorios de datos/labelled sample"
cd "$LABELLED_SAMPLE_HOME"
dir
import excel "final_evalaution_sample.xlsx", first clear

			
*
* 1.2 feature selection on control set as global
*

*
tab estudios, gen(est_)
tab genero, gen(sexo_)
tab laboral_states, gen(estado_)

*
drop if year<2016
capture drop year_* 
capture drop post_covid_* 
capture drop ln_momento_registro*
tostring year, replace
tab year, gen(year_)
tab actividad_post_covid, gen(period_)
gen ln_momento_registro=log(momento_registro)

*
global control_set="edad  sexo_* es_migrante"
global control_set="$control_set year_* period_* ln_momento_registro"
global control_set="$control_set experiencia certificacion idioma_extranjero competencias_digitales software"
global depurated_control_set="$control_set"

*
* 2. propensity score matching
*

*
quietly kmatch ps al_menos_3 $depurated_control_set, ///
ematch(  es_migrante est_*  estado_* period_* year_*) ///
att

*
* Automating reports generation from stata
* https://journals.sagepub.com/doi/pdf/10.1177/1536867X1301300104
