*****************************************************************************************************************
* REGRESSIONS
*****************************************************************************************************************
global dir C:\Users\wb519128\Dropbox\Work\Insper\PMRJ

 
//use $dir/data_SIM_2019-01.dta, clear

import delimited $dir/data_SIM_2019.csv, clear
 
 
* Merge com a base de meta placedo

preserve
	tempfile
	import delimited  "$dir/placebo_targets.csv", clear 
	save placebo_targets, replace
restore
 
 drop if missing(aisp) | missing(year) | missing(month)
 merge 1:1 aisp year month using placebo_targets

 
*****************************************************************************************************************
* CREATE TREATMENT VARIABLES 
*****************************************************************************************************************

* Drop existing variables since I cannot recreate the dataset
drop on_target on_target_sr on_target_vr lag1_on_target dist_target_vd  		///
lag12_dist_target_vd dist_target_vr lag12_dist_target_vr dist_target_sr 		///
lag12_dist_target_sr violent_death_sim_cum vehicle_robbery_cum 					///
street_robbery_cum violent_death_sim_cum2 on_target_vd  vehicle_robbery_cum2 	///
street_robbery_cum2  target_vd_cum


* Replace targets with placebos it between 2006 and 1 2009 
replace target_vd = vd_placebo_tar if year <2009 | (year == 2009 & semester == 1)
replace target_vr = vr_placebo_tar if year <2009 | (year == 2009 & semester == 1)
replace target_sr = sr_placebo_tar if year <2009 | (year == 2009 & semester == 1)


foreach x of varlist violent_death_sim target_vd street_robbery target_sr vehicle_robbery target_vr {
	bysort aisp (sem_year month): gen `x'_cum= `x'[_n-1] if month==2 | month==8
	bysort  aisp (sem_year): replace `x'_cum=  `x'[_n-1] +  `x'_cum[_n-1] if month==3 | month==9
	bysort  aisp (sem_year): replace `x'_cum=  `x'[_n-1] +  `x'_cum[_n-1] if month==4 | month==10
	bysort  aisp (sem_year): replace `x'_cum=  `x'[_n-1] +  `x'_cum[_n-1] if month==5 | month==11
	bysort  aisp (sem_year): replace `x'_cum=  `x'[_n-1] +  `x'_cum[_n-1] if month==6 | month==12
}

foreach x of varlist violent_death_sim  street_robbery  vehicle_robbery  {					
	bysort aisp (sem_year month): gen `x'_cum2= `x' if month==1 | month==7
	bysort  aisp (sem_year): replace `x'_cum2=  `x'+  `x'_cum2[_n-1] if month==2 | month==8
	bysort  aisp (sem_year): replace `x'_cum2=  `x' +  `x'_cum2[_n-1] if month==3 | month==9
	bysort  aisp (sem_year): replace `x'_cum2=  `x' +  `x'_cum2[_n-1] if month==4 | month==10
	bysort  aisp (sem_year): replace `x'_cum2=  `x' +  `x'_cum2[_n-1] if month==5 | month==11
	bysort  aisp (sem_year): replace `x'_cum2=  `x' +  `x'_cum2[_n-1] if month==6 | month==12
}


foreach x of varlist target_vd  target_sr  target_vr {
	egen `x'_sem=sum(`x') , by(aisp sem_year) 
}

* Actually create treatment vars
gen 	on_target_vd=(violent_death_sim_cum<=target_vd_cum) 
replace on_target_vd=(violent_death_sim_cum<=target_vd_cum*1.1) if year>=2013
replace on_target_vd=. if cycle==1

gen 	on_target_sr=(street_robbery_cum<=target_sr_cum)
replace on_target_sr=(street_robbery_cum<=target_sr_cum*1.1) if year>=2013

gen 	on_target_vr=(vehicle_robbery_cum<=target_vr_cum)
replace on_target_vr=(vehicle_robbery_cum<=target_vr_cum*1.1) if year>=2013

gen 	on_target= (on_target_vd==1 & on_target_sr==1 & on_target_vr==1)

sort aisp month_year 
gen lag1_on_target=on_target[_n-1]


* Distance variable
gen dist_target_vd=violent_death_sim_cum /target_vd_sem -1 
bysort aisp (month_year): gen lag12_dist_target_vd=dist_target_vd[_n-12]

gen dist_target_vr=(vehicle_robbery_cum)/(target_vr_sem ) -1 
bysort aisp (month_year): gen lag12_dist_target_vr=dist_target_vr[_n-12]

gen dist_target_sr=street_robbery_cum /target_sr_sem -1 
bysort aisp (month_year): gen lag12_dist_target_sr=dist_target_sr[_n-12]



foreach x of varlist target_vd  {
label var `x' "violent death target (month)"
label var `x'_sem "violent death target (semester)"
label var `x'_cum "violent death target (cumulative until t-1)"
label var on_`x' "indicator for on target until t-1)"
label var dist_`x' "distance to the target (=0 on target, >0 above target)"

}


label var cycle "Indicator for the month"



 

*****************************************************************************************************************
* REGRESSIONS
***************************************************************************************************************** 
 
 order aisp year month semester month_year sem_year cycle violent_death homicide violent_death_sim  police_killing body_found robbery theft street_robbery vehicle_robbery vehicle_theft cargo_robbery other_robberies drug_seizure gun_seizure arrest juvenile_arrest target_vd target_sr target_vr 

 
* Quasi-placebo regressions
// From second semester of 2015 to 2018, the city stopped paying the prizes
 
preserve
	keep if (year > 2015 | (year == 2015 & month > 5))

	
	foreach y of varlist  violent_death_sim  vehicle_robbery  street_robbery homicide dpolice_killing {
	xi: xtreg `y'  on_target  n_precinct population i.month i.year ,  fe 
		sum `y' 
		eret2 scalar mean_y=r(mean)
		eret2 scalar adj_R2=e(r2_a)
		outreg2 using Results\placebo_tab3.xls, keep(on_target) dec(3) nocons  aster(se) e(mean_y adj_R2 )
	xi: xtreg  `y'  on_target n_precinct population i.month i.year i.id_cmt,  fe 
		sum `y' 
		eret2 scalar mean_y=r(mean)
		eret2 scalar adj_R2=e(r2_a)	
		outreg2 using Results\placebo_tab3.xls, keep(on_target) dec(3) nocons  aster(se) e(mean_y adj_R2 )
	}
	
	
restore 
 

 
* Placebo target regressions 
preserve
 
	keep if year > 2006 & year < 2009 


	xtreg violent_death_sim  on_target n_precinct population i.month i.year,  fe 
	xtreg violent_death_sim  on_target n_precinct population i.month i.year i.id_cmt
	 
	*Placebo Table 2

	foreach y of varlist  violent_death_sim  vehicle_robbery  street_robbery homicide dpolice_killing {
	xi: xtreg `y'  on_target  n_precinct population i.month i.year ,  fe 
		sum `y' 
		eret2 scalar mean_y=r(mean)
		eret2 scalar adj_R2=e(r2_a)
		outreg2 using Results\placebo_tab3.xls, keep(on_target) dec(3) nocons  aster(se) e(mean_y adj_R2 )
	xi: xtreg  `y'  on_target n_precinct population i.month i.year i.id_cmt,  fe 
		sum `y' 
		eret2 scalar mean_y=r(mean)
		eret2 scalar adj_R2=e(r2_a)	
		outreg2 using Results\placebo_tab3.xls, keep(on_target) dec(3) nocons  aster(se) e(mean_y adj_R2 )
	}

restore 

