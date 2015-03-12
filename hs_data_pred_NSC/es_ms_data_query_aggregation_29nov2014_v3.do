set more off
log close _all
clear all

/* updates
2014.08.14	- added indicator of the severity of the SPED primary area:

2014.08.11	- updated AP, IB, gifted to only count if passed course
			- add retained since 2004 indicator (Y, N)
			- started adding grad tests passed indicator (2010-11 9th - 12th graders)
			- not including AP exam scores until can find IB exams 
			- !!still need EOCTs passed and typical pass year (9th, 10th, 11th, 12th)


2014.08.08	- added AP, IB, gifted course completion indicators
			- added failures in core subjects
			- added PSAT (whether took it and whether scores predict college success)
			- added student address based context information
*/


/*	SEE ${path}\RBES\WSA 2.0\
			student.success.factor\data\metadata\census_data\
			variables_in_model.txt //for census measures included
*/

/*******************************************************************************
	- This file extracts covariate data to be used as predictors within R for 
		estimating probabilities of success within HS grade levels
	- Plan: Use 2010-11 data to predict 2011-12 then check results in 2012-13
	----------------------------------------
	
	OUTCOME:
	- On-time graduation [(date entered 9th + 4 years) or 
						  (date entered GCPS + (12 - grade entered) years)]
	- SAT (520 on CR & M) or 
	- ACT score (>=22 in M & R & >= 18 in E)
	- HS GPA >=83
	
	
	STUDENT-LEVEL INDICATORS:
	*background/"fixed"
	- age (overage) 
	- FRL eligibility;
	- gender 
	- mobility
	- race/ethnicity
	
	*malleable
	- achievement
		- CRCTs
			- from cohort 8th grade year 2008 (not SS b/c diff scales)
		- EOCTs
			-test results
			-course passage (from Student Detail in MYs)
		- Gateways (only avail for 2012, 2013)
		-GPA
		-courses completed (3 - ELA, M, SC; 2 - SS; 17 credits)
	- attendance
	- discipline (incidents + dispositions)
	- engagement perceptions (SEI)
	- LEP services
	- SPED services
	
	SCHOOL-LEVEL INDICATORS:
	- %FRL
	- %SPED
	- %LEP
	- %Enrolled days absent
	- Disciplinary Index
	- SEI annual average (F + S) (MS, HS only)
	//- Census - maternal (paternal?) ed level, income, community ed level
	
		
	-Created by:		2013.12.19 by James Appleton
	-Last revised: 		2014.11.12 by James Appleton
	
*******************************************************************************/
	global		twelfth		"0"		// set to 1 if modeling 11th grd to 12th grd
************************************* globals *********************************
infix str path 1-500 using "c:\current_path.txt", clear
local path = trim(path)
global path = "`path'"
global ssipath = "`path'\RBES\WSA 2.0\student.success.factor"
drop path
cd "${ssipath}"

	global		yrsbk		"3"			// years back from end yr of current 
											* academic year; e.g., 3 in 14-15
											* year sets grad year as 2015-3 = 2012
	
	global		nxtyr		"13"		// year beyond evaluated
	global		evalyr		"12"		// evaluated year
	global		histyr1		"11"		// historic year 1
	global		histyr2		"10"		// historic year 2
	global 		histyr3		"09"		// historic year 3
	global		histyr4		"08"		// historic year 4
	global		histyr5		"07"		// historic year 5 - needed for CRCTs
	
* 	globals to control functions when wanting to not overwrite on re-runs
	global		save		"save"
	global		export		"export"
	global		graph		"graph"
	global		outsheet	"outsheet"
	global		hist		"hist"
	global		tab			"tab"
********************************************************************************

/*******************************************************************************
	-Calculate the school day values for test windows; 
		first day of window is used; ignore days on break
********************************************************************************/
global crct 	=	"20${evalyr}-04-01"		// 1) all have same gadoe test window
global wrtg8 	=	"20${evalyr}-01-23"		// 2) 8th grade writing
global eoctf	=	"20${evalyr}-11-26"		// 3) eocts fall
global eoctsp	=	"20${evalyr}-04-22"		// 4) eocts spring - use summer?
global gwysc	=	"20${evalyr}-04-27"		// 5) science gateway (LA and content)
global gwyss	=	"20${evalyr}-04-27"		// 6) social studies gateway(LA and 
											//		content)dates are given as 
											//		separate on testing calendar
global sem1		=  90						// set length of first semester

****** Switches ****************************************************************
	global		extract			"0"		// extracts separate tables from ODS; 
										// everything is saved for future use 
										// so set to "0"
	global		calc			"0"
	global 		merge_all		"1"		// merges ODS-queried attendance, 
										// behavioral, CRCT data for ms analyses
	global		scoring			"0"
********************************************************************************

/* clear screen for 20,000 lines
program define cls2
        forvalues n=1/2000 {
        display ""
        local ++n
        }
end
exit
*/

log using code/logfiles/merge_all_es_ms.smcl, replace

use data/prep/es_ms_model, clear
	
	tempfile modeling
	save `modeling', replace
	

	drop 	schoolyear_${histyr1}${evalyr} fullname_${histyr1}${evalyr} /// 
			gender_${histyr1}${evalyr} birthdate_${histyr1}${evalyr} ///
			ethnicity_${histyr1}${evalyr} primarylanguage_${histyr1}${evalyr} ///
			ageseptember1st_${histyr1}${evalyr} freereducedlunch_${histyr1}${evalyr} ///
			lep_${histyr1}${evalyr} ell_${histyr1}${evalyr} ///
			specialeducationprimaryarea_${histyr1}${evalyr} /// 
			altschoolindicator_${histyr1}${evalyr} giftedindicator_${histyr1}${evalyr} ///
			repeatinggradeindicator_${histyr1}${evalyr} //daysabsent_${histyr1}${evalyr}
			
	gen pabs_${histyr2}${histyr1} = ///
			daysabsent_${histyr2}${histyr1}/daysenrolled_${histyr2}${histyr1}
	gen pabs_${histyr1}${evalyr} = ///
			daysabsent_${histyr1}${evalyr}/daysenrolled_${histyr1}${evalyr}
	gen percentEnrolledDays_${histyr2}${histyr1} = ///
			daysenrolled_${histyr2}${histyr1}/180
	gen sped_${histyr2}${histyr1} = specialeducation != " "

		order id-specialeducation sped_${histyr2}${histyr1}
	drop 	birthdate_${histyr2}${histyr1} ell_${histyr2}${histyr1} ///
			primarylanguage_${histyr2}${histyr1} altschool ///
			daysenrolled_${histyr2}${histyr1} daysabsent_${histyr2}${histyr1} disc_enr ///
			sequence iss_oss_days num_enr schoolyear grade_${histyr2}${histyr1} ///
			specialeducation
			
	gen female if gender != "" = gender == "Female"

	gen black if ethnicity_${histyr2}${histyr1} != "" = ///
					ethnicity_${histyr2}${histyr1} == "B"
	gen hispanic if ethnicity_${histyr2}${histyr1} != "" = /// 
					ethnicity_${histyr2}${histyr1} == "H"
	gen other if ethnicity_${histyr2}${histyr1} != "" = /// 
					inlist(ethnicity_${histyr2}${histyr1}, "A", "I", "M")
	gen frl_${histyr2}${histyr1} if freereducedlunch_${histyr2}${histyr1} != "" = ///
					inlist(freereducedlunch_${histyr2}${histyr1}, "F", "R")
		rename lep_${histyr2}${histyr1} limitedenglishprof
	gen lep_${histyr2}${histyr1} if limitedenglishprof != "" = /// 
					inlist(limitedenglishprof, "Y", "M")
	gen repgrd_${histyr2}${histyr1} if repgrd != . = repgrd == 1
	gen gft_${histyr2}${histyr1} if gft != . = gft == 1

		drop gender ethnicity freereduced limitedenglish repeating giftedind ///
				date9thgrade graduationdate yearstograd giftedind gft
				
		foreach var of varlist* {
		
			//next yr
			rename `var' `=regexr("`var'", "20${nxtyr}", "_N")'
		
		}
		
		foreach var of varlist* {
		
			//eval yr
			local ev =  "_${histyr1}${evalyr}|_20${evalyr}|_${evalyr}|" + ///
						"${histyr1}${evalyr}|20${histyr1}20${evalyr}|" + ///
						"20${evalyr}|${evalyr}"			
			
			rename `var' `=regexr("`var'", "`ev'", "_E")'
		}
			
		foreach var of varlist* {
		
			//hist yr 1
			local hs1 =  "_${histyr2}${histyr1}|_20${histyr2}20${histyr1}|" + ///
						 "_20${histyr1}|_${histyr1}|${histyr2}${histyr1}|" + ///
						 "20${histyr2}20${histyr1}|20${histyr1}|${histyr1}"			
			
			rename `var' `=regexr("`var'", "`hs1'", "_H1")'
		}
		

		foreach var of varlist* {		
		//hist yr 2		
			rename `var' `=regexr("`var'", "20${histyr2}", "_H2")'
		}
		
		foreach var of varlist* {		
		//hist yr 3	
			rename `var' `=regexr("`var'", "20${histyr3}", "_H3")'
		}
				
preserve 
keep if grade_E ==8
drop if startyear_grade_E ==. | startyear_grade_N == . | fay_year_E != "Y"
		outsheet using data/prep/7th8th_model_only.csv, c replace
restore		

preserve 
keep if grade_E ==7
drop if startyear_grade_E ==. | startyear_grade_N == . | fay_year_E != "Y"	//percentEnrolledDays_1011 < .65 ??regardless of how received them??
		outsheet using data/prep/6th7th_model_only.csv, c replace
restore	


preserve 
keep if grade_E ==6
drop if startyear_grade_E ==. | startyear_grade_N == . | fay_year_E != "Y"	//percentEnrolledDays_1011 < .65 ??regardless of how received them??
		outsheet using data/prep/5th6th_model_only.csv, c replace
restore	

preserve 
keep if grade_E ==5
drop if startyear_grade_E ==. | startyear_grade_N == . | fay_year_E != "Y"	//percentEnrolledDays_1011 < .65 ??regardless of how received them??
		outsheet using data/prep/4th5th_model_only.csv, c replace
restore	

preserve 
keep if grade_E ==4
drop if startyear_grade_E ==. | startyear_grade_N == . | fay_year_E != "Y"	//percentEnrolledDays_1011 < .65 ??regardless of how received them??
		outsheet using data/prep/3rd4th_model_only.csv, c replace
restore	
	
  

/*if $scoring==1 {*/
* SCORED DATASET
*****************
	use data/orig/yr13, nogen keep (1 3) // to confirm attendance for new group
	merge 1:1 id using data/prep/discipline_enroll_${histyr1}_metric_final, nogen keep (1 3)
	merge 1:1 id using data/prep/crct_final_2011, nogen keep (1 3)
	merge 1:1 id using data/prep/mobility${histyr2}${histyr1}_hs, nogen keep (1 3)
	merge 1:1 id using data/prep/crctSuccess1213, nogen keep(1 3)

		drop if ethnicity_${histyr2}${histyr1} == ""

		* finalize dataset
		tab ethnicity, m
		gen blk = 0
		replace blk = 1 if ethnicity=="B"
		gen hsp = 0
		replace hsp = 1 if ethnicity=="H"
		gen wht = 0
		replace wht=1 if ethnicity=="W"
			tab wht ethnicity,m
			tab hsp ethnicity,m
			tab blk ethnicity,m
				drop ethnicity
		
				gen pabs${histyr2}${histyr1}= daysabsent_${histyr2}${histyr1} / daysenrolled_${histyr2}${histyr1}
				tab  specialeducationprimaryarea, m
					gen sped = 0
					replace sped = 1 if specialeducationprimaryarea=="Visual Impairment" | specialeducationprimaryarea=="Blind" | specialeducationprimaryarea=="Hearing Impairment" ///
						| specialeducationprimaryarea=="Orthopedic Impairment" | specialeducationprimaryarea=="Deaf" | specialeducationprimaryarea=="Speech/Language Impairment" 
					replace sped = 2 if specialeducationprimaryarea=="Deaf and Blind" | specialeducationprimaryarea=="Emotional/Behavioral Disorder" | ///
						specialeducationprimaryarea=="Mild Intellectual Disability" | specialeducationprimaryarea=="Other Health Impairment" | ///
						specialeducationprimaryarea=="Specific Learning Disability" | specialeducationprimaryarea=="Autism" 
					replace sped = 3 if specialeducationprimaryarea=="Moderate Intellectual Disability" | specialeducationprimaryarea=="Profund Intellectual Disability" | ///
						specialeducationprimaryarea=="Severe Intellectual Disability" | specialeducationprimaryarea=="Significant Development Delay" | ///
						specialeducationprimaryarea=="Traumatic Brain Injury" 
							tab sped specialeducationprimaryarea,m
								drop specialeducationprimaryarea
							
							tab gender
							gen female = 0
							replace female = 1 if trim(gender)=="Female"
								drop gender
								
									rename altschoolindicator_${histyr2}${histyr1} alt${histyr2}${histyr1}_ind
									rename giftedindicator_${histyr2}${histyr1} gift${histyr2}${histyr1}_ind
									rename repeatinggradeindicator_${histyr2}${histyr1} repgrd${histyr2}${histyr1}_ind
									
										assert freereducedlunch != ""
										gen free = 0
											replace free = 1 if freereducedlunch == "F"
										gen red = 0
											replace red = 1 if freereducedlunch == "R"
											tab free freereducedlunch,m
											tab red freereducedlunch,m
												drop freereducedlunch
												
										assert ell != ""
											rename ell el
										gen ell = 0
											replace ell = 1 if el == "Y"
											tab ell el,m
												drop el
												
										assert lep != ""
											rename lep lep1
										gen lep = 0
											replace lep = 1 if lep1 == "Y" | lep1 == "M"
											tab lep lep1,m
												drop lep1
									

							local hyr1="$histyr1"
							local hyr2="$histyr2"
							foreach var in gift`hyr2'`hyr1' repgrd`hyr2'`hyr1' alt`hyr2'`hyr1' { // uses first part of var name
								tab `var',m
								gen `var' = 0
								replace `var'=1 if `var'_ind=="Y"
								tab `var' `var'_ind,m
									drop `var'_ind
									}

										tab grade,m
										*drop if grade=="PK" | grade=="KK"  // these cannot be matches anyway removed in extract in 2012-13
										destring grade, replace
										rename grade enrgrd
										
												tab ageseptember1st,m
												rename ageseptember1st syage
												rename  schoolyear yr
												rename fullname name
												egen lmrscssz${histyr1}mn=rmean(la${histyr1}z ma${histyr1}z rd${histyr1}z sc${histyr1}z ss${histyr1}z)
												
														replace drate_${histyr2}${histyr1} = 0 if drate_${histyr2}${histyr1}==. & pabs${histyr2}${histyr1}!=.
														replace dsevmx_${histyr2}${histyr1} = 0 if dsevmx_${histyr2}${histyr1}==. & pabs${histyr2}${histyr1}!=.
														replace dsevmn_${histyr2}${histyr1} = 0 if dsevmn_${histyr2}${histyr1}==. & pabs${histyr2}${histyr1}!=. 
														
														 drop enr_days num_enr // drop variables not needed for analysis
														replace give_phx_ind = 0 if give_phx_ind==.
	

	log close

	}
	


