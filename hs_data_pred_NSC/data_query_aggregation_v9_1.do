set more off, permanently
log close _all
clear all

/* updates
2015.04.03	- added 12th predicting 13th (post-sec persist from NSC)

2015.03.13	- changed some census variables to gini and deprivation

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
	- National Student Clearinghouse (NSC) indicated immediate post-secondary
		enrollment and persistence
		
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
	??- On-time graduation [(date entered 9th + 4 years) or 
						  (date entered GCPS + (12 - grade entered) years)]
	- post-secondary ready (psr)
		- SAT (520 on CR & M) or 
		- ACT score (>=22 in M & R & >= 18 in E)
	
	SCHOOL-LEVEL INDICATORS:
	- %FRL
	- %SPED
	- %LEP
	- %Enrolled days absent
	- Disciplinary Index
	- SEI annual average (F + S) (MS, HS only)
	//- Census - community level deprivation & inequality ***Updated- RAR(2015.03.13)
	
		
	-Created by:		2013.12.19 by James Appleton
	
*******************************************************************************/

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

/*clear screen for 20,000 lines
program define cls2
		set more off, permanently
        forvalues n=1/20000 {
        display ""
        local ++n
        }
end
exit
*/

if $extract==1	{
log using code/logfiles/extract.smcl, replace

**************************
*gcps to state schl codes
**************************
	#delimit ;
		odbc load, clear exec	("
									SELECT distinct [LOC]
										  ,[SCH_CODE]
									  WHERE SCH_YR = 20${histyr1}
								")
			dsn(ODS_Prod_RE) user(Research) pass(Research) ;
	#delimit cr

		foreach var of varlist* { 
			destring `var', replace
			rename `var' `=lower("`var'")'
		}			
		rename (sch_code loc) (SchoolNumb loc_${histyr2}${histyr1})
		

		
save data/prep/stateNum_to_gcpsNum_link, replace

*******************************************************************************
* evaluated year data
*******************************************************************************
	#delimit ;
		odbc load, clear exec	("
			SELECT *
			FROM [ResearchAndEvaluation].[dbo].[V_RBES_Special_Entity2]
			WHERE SchoolYear = 20${evalyr}
								")
		dsn(ODS_Prod_RE) user(Research) pass(Research) ;
	#delimit cr

		foreach var of varlist* { 
			destring `var', replace
			rename `var' `=lower("`var'")'
		}

		destring grade, replace force
		drop if grade == .
	
		foreach var of varlist* { 
			rename `var' `var'_${histyr1}${evalyr}
		}

		rename gcps_studentid id
		isid id grade altschoolindicator repeatinggradeindicator
		
		* to check if daysenrolled and daysabsent are plausible values
		assert daysenrolled- daysabsent>=0
${save} data/orig/evalyr_hs, replace
	
	#delimit ;
		odbc load, clear exec	("
			SELECT [Permnum]
				  ,[FAY_year]
				  ,[zoned_school]
				  ,[zoned_school_name]
			FROM  [ResearchAndEvaluation].[dbo].[v_Student_Demography]
			WHERE school_year = 20${evalyr}
								")
		dsn(ODS_Prod_RE) user(Research) pass(Research) ;
	#delimit cr

		foreach var of varlist* { 
			destring `var', replace
			rename `var' `=lower("`var'")'
		}

		foreach var of varlist* { 
			rename `var' `var'_${histyr1}${evalyr}
		}

		rename permnum id
		isid id
		
	tempfile zoned1
	save `zoned1', replace

**************************************************
* historical year data 
*	get school at which student was most enrolled
**************************************************
	* get date to schoolday calendar
	#delimit ;
		odbc load, clear exec	("
			SELECT CAST([DATE_2K] as char(8)) as Date
				  ,[SCHDAY] as SchoolDay
			 FROM [GSDR].[GEMS].[CALENDAR]
			WHERE SCHOOL_YEAR = 20${histyr1}
								")
		dsn(ODS_Prod_MA) user(Research) pass(Research) ;
	#delimit cr

		foreach var of varlist* {
			rename `var' `=lower("`var'")'
		}
		
		gen dt = date(date, "YMD") 
		format dt %td
		drop date
	
		* convert schoolday 0 to the most recent day number
		replace schoolday = . if schoolday == 0
		rename schoolday schlday
		carryforward schlday, gen(schoolday)
			drop schlday
		order dt schoolday
			tempfile cal
			save `cal', replace

	* get enrollment locations and durations from GSDR
	#delimit ;
		odbc load, clear exec	("
			SELECT [SCHOOLNUM]
				  ,[PERMNUM]
				  ,[SEQUENCE]
				  ,[EFFDATE]
				  ,[ENTERCODE]
				  ,[LEAVECODE]
				  ,[ELIGADA]
				  ,[SCHOOLATTN]
				  ,[GRADE]
				  ,[TCHNUM]
				  ,[INSTRSET]
				  ,[TRANSYEAR]
				  ,[STATUS]
			  FROM [GSDR].[GEMS].[SASI_AENR]
			  WHERE TRANSYEAR in (${histyr1} - 1) and
					EFFDATE < 20${histyr1}0530 and
					EFFDATE >= 20${histyr2}0801
								")
		dsn(ODS_Prod_MA) user(Research) pass(Research) ;
	#delimit cr

		foreach var of varlist* {
			destring `var', replace
			rename `var' `=lower("`var'")'
		}
		rename permnum id
		
		* we will use the school the student attended for the longest duration
		tostring effdate, replace
		gen dt = date(effdate, "YMD")
		format dt %td
	
		* link with school calendar to get school days
		merge m:1 dt using `cal', nogen keep(1 3)
			preserve
				use `cal', clear
				keep if schoolday == 1
				drop schoolday
				local first = dt
			restore
			
		* any day before 1 becomes 1 (only up to SQL query fiiltered range)
		replace schoolday = 1 if dt < `first'
		assert schoolday != .

		gen entcd = trim(entercode)
		gen lvcd = trim(leavecode)
		drop if entcd == "" & lvcd == ""
		drop schoolnum sequence eligada grade tchnum instrset transyear ///
				status effdate
		duplicates drop
		bys id (schoolday): gen seq = _n
	
		* we drop if enter date == leave date //affects duration only by 1
			* plus it's unclear whether/how that day should count
		drop if entcd != "" & lvcd != ""
		drop seq
		bys id (schoolday): gen seq = _n
		
		* two sequential enr or lv  w/out approp lv or enr in between
		drop if (mod(seq, 2) == 0 & entcd != "") | /// 
				(mod(seq, 2) == 1 & lvcd != "")
		drop seq
		bys id (schoolday): gen seq = _n

		drop dt entercode leavecode
		rename schoolattn loc
				summarize seq
				local mx = `r(max)'
				di "max sequence is: " `mx'

		reshape wide entcd lvcd loc schoolday, i(id) j(seq)
		egen n = rownonmiss(loc*)
		

		
		* enr only on odd occasions and lv on even but not both so not needed
		drop ent* lv*

		* create separate enrollment durations
			forvalues i = `mx'(-1)1 {
				local j = `i' - 1
				
				* case when last enroll seq is odd (means ended enrolled)
				if (mod(`i', 2) == 1) {
					gen dur`i' = 180 - schoolday`i' + 1 if n == `i' & ///
																mod(n, 2) == 1
					if (`i' >= 2) {
						gen school`i' = loc`j' if n == `i' & mod(`i', 2) == 1
					} //END IF >= 2
					else if (`i' == 1) {
						gen school`i' = loc`i' if n == `i' & mod(`i', 2) == 1
					} //END IF == 1
				} //END MOD == 1
				
				* case for each even to odd pair that is populated
				if (mod(`i', 2) == 0 & `i' >= 2) {
					gen dur`i' = schoolday`i' - schoolday`j' + 1 ///
													if loc`i' != . & loc`j' != .
					gen school`i' = loc`j' if loc`i' != . & loc`j' != .
				} //END MOD == 0
			} //END FORVALUES

	* now get summary by school vs. enrollment period
		isid id
		keep id dur10- school1
		reshape long dur school, i(id) j(seq)
		drop if dur == .
		
		* get sum of enrolled days by location
		collapse (sum) dur, by(id school)
	preserve
		collapse (sum) enr_days = dur (count) num_enr = school, by(id)
		gen mob${histyr2}${histyr1} = num_enr/enr_days
		* save in place of mobility from Chris Lanzi attendance query
		save data\prep\sasi_mobility${histyr2}${histyr1}_hs, replace
	restore
		bys id: egen mx = max(dur)

		* randomly remove one if same days at more than one school
		bys id: gen rand = runiform()
		keep if dur == mx
		drop mx
		duplicates tag id, gen(dup)
		bys id: egen mx = max(rand)
		keep if rand == mx | dup == 0
			drop mx dup rand
			rename school loc
			isid id

		foreach var of varlist loc dur { 
			rename `var' `var'_${histyr2}${histyr1}
		}
					
		isid id
		merge 1:1 id using `zoned1', nogen keep(1 3)
${save} data/orig/fay_evalyr_hs, replace

* get last withdrawal for removing transfer outs
*************************************************
#delimit ;
		odbc load, clear exec	("
			SELECT 			 [PERMNUM]
							,[SCHOOL]
							,[LEAVECODE]
							,[LEAVEDATE]
			FROM [GSDR].[GEMS].[SDRF_HIST]
			WHERE 	LEAVEDATE is not NULL and 
					LEAVECODE != '' and 
					LEN(PERMNUM) >= 7 and
					LEAVEDATE <= 20${evalyr}0801 and
					LEAVEDATE >= 20${histyr1}0801
										")
		dsn(ODS_Prod_MA) user(Research) pass(Research) ;
	#delimit cr
	
	

		foreach var of varlist* {
			destring `var', replace
			rename `var' `=lower("`var'")'
		}
		rename permnum id
		
		bys id (leavedate): gen _cnt = _n
		bys id: egen _mx = max(_cnt)
		keep if _mx == _cnt
		drop _*
		rename leavecode lastwithdrawal
		
		gen transferout = inlist(lastwithdrawal, 	"1", "4", "D", "H", "J", ///
													"K", "T", "X", "Y")
			replace transferout = 1 if lastwithdrawal == "Z"
		
		/* codes used for transfers out, emigrate out, or die: (1, 4, D, H, J, 
																K, T, X, Y, Z)
		 from http://www.gadoe.org/Curriculum-Instruction-and-Assessment/
				Accountability/Documents/Cohort%20Graduation%20Rate%20
				Calculators%2011.08.13.xls
				
				SB10 Transfer to Public School (1)
				Transfer: DJJ (4)
				Death (D)
				Transfer: Homeschool (H)
				Transfer: Out of Country (J)
				Transfer: Private School (K)
				Transfer: Other System (T)
				Transfer: Another State (X)
				SB10 Transfer to State Schools (Y)
				SB10 Transfer to Private School (Z)			
		*/
		
* get diploma type //think DIPLOMA_TYPE1 is the type not expected type 
	* B(Both college prep and vocational), C(College prep), 
	* V(Vocational), G(General Diploma) are considered graduates
		* (see pp. 19-20 of GADOE 2012_SR_Data Element Detail_06.04_12.doc)
	* kept most recent value to avoid duplicates (only 26 duplicates)
	preserve
		#delimit ;
		odbc load, clear exec	("
									SELECT distinct  [SCHOOL_YEAR]
													,[PERMNUM]
													,[DIPLOMA_TYPE1]
									FROM [GSDR].[GEMS].[SDRC_HIST]
									WHERE 	DIPLOMA_TYPE1 != '' 
								")
		dsn(ODS_Prod_MA) user(Research) pass(Research) ;
	#delimit cr

		foreach var of varlist* {
			destring `var', replace
			rename `var' `=lower("`var'")'
		}
		rename permnum id
		
		duplicates drop id diploma_type1, force
		
		bys id (school_year): gen _cnt = _n
		bys id: egen _mx = max(_cnt)
		keep if _mx == _cnt
		drop _* //school_year
		isid id
			gen grad_diploma_type = inlist(diploma_type1, "B", "C", "G", "V")
			save data/prep/diploma_types, replace
	restore
	
	merge 1:1 id using data/prep/diploma_types, nogen keep(1 3)
	gen grad = lastwithdrawal == "G" & grad_diploma_type == 1 & transferout == 0
	drop if transferout == 1
	
	save data/prep/grad, replace		
		
* other historical data besides enrollment
*******************************************
	#delimit ;
		odbc load, clear exec	("
			SELECT 	GCPS_StudentId, 
					SchoolYear,
					RepeatingGradeIndicator,
					GiftedIndicator
			FROM [ResearchAndEvaluation].[dbo].[V_RBES_Special_Entity2]
			WHERE SchoolYear <= 20${histyr1}
								")
		dsn(ODS_Prod_RE) user(Research) pass(Research) ;
	#delimit cr

		foreach var of varlist* { 
			destring `var', replace
			rename `var' `=lower("`var'")'
		}
		rename gcps_studentid id
		gen repeatinggrd = repeatinggradeindicator == "Y"
		gen giftedind = giftedindicator == "Y"
		

	preserve
		keep if schoolyear == 20${histyr1}
		collapse (sum) repeatinggrd giftedind, by(id)
		gen repgrd = repeatinggrd > 0 & giftedind != .
		gen gft = giftedind > 0 & giftedind != .
		drop repeatinggrd giftedind
		tempfile gftret
		save `gftret', replace
	restore
		
		collapse (sum) repeatinggrd giftedind, by(id)
		gen retained_from04 = repeatinggrd > 0 & giftedind != .
		gen gft_from8th_H1 = giftedind > 0 & giftedind != .
		drop repeatinggrd giftedind
		merge 1:1 id using `gftret', nogen keep(1 3)
${save} data/prep/retgft04, replace

	#delimit ;
		odbc load, clear exec	("
			SELECT *
			FROM [ResearchAndEvaluation].[dbo].[V_RBES_Special_Entity2]
			WHERE SchoolYear = 20${histyr1}
								")
		dsn(ODS_Prod_RE) user(Research) pass(Research) ;
	#delimit cr

		foreach var of varlist* { 
			destring `var', replace
			rename `var' `=lower("`var'")'
		}
		
		destring grade, replace force
		drop if grade == .
		
		foreach var of varlist* { 
			rename `var' `var'_${histyr2}${histyr1}
		}

		rename gcps_studentid id
		isid id grade altschoolindicator repeatinggradeindicator
		assert daysenrolled- daysabsent>=0
${save} data/orig/histyr_hs, replace
	
********************************************************************************
* historic year mobility data - ODS - ratio is:
*			(number of distinct school enrollments) / (number of enrolled days)
********************************************************************************

/* Attendance - View by Chris Lanzi for PA work
****************************************************

	*select the date to school calendar linking table

	foreach yr in 20$histyr1 20$evalyr {
		#delimit ;
			odbc load, clear exec	("
				SELECT 	Date, 
						SchoolDay 
				FROM 	Datamart_SASI.dbo.CALENDAR 
				WHERE 	schoolyear = `yr' and 
						EFF_END_DATE is null
									")
				dsn(ODS_Prod_DS) user(Research) pass(Research);
		#delimit cr
${save} data/prep/calendar`yr'hs, replace
	} //END FOREACH
*/
*zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
*TURNED OFF zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
	*select all students in the given sch yr
/*	foreach yr in 2012 2011  { // wait to extract in evening to avoid slowing 
								 // servers (per Chris Lanzi)   
								 // 20${histyr1} 20${histyr2}
	#delimit ;
		odbc load, clear exec	("
			SELECT * 
			FROM Predictive_Analytics.dbo.Attendance 
			WHERE School_Year = `yr'
								")
			dsn(ODS_Dev_PA) user(Research) pass(Research);
	#delimit cr
	// save to desktop b/c ~2G sized files
${save} "C:\Users\e200701890\Desktop\org\attend\attend`yr'", replace 

	} 
*/
*TURNED OFF zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
*zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz

	local paths = "${path}\Jim_Appleton\sort\" ///
					+ "org\attend\attend20${histyr1}"
					
	use "`paths'", clear

/*		preserve
			rename Attendance_Day_Number SchoolDay
			rename Student_School_Number loc
			destring SchoolDay, replace
			destring loc, replace
			sort Student_Permnum  School_Year SchoolDay
			gen distinct_episode = 1
			replace distinct_episode = 0 if Student_Permnum[_n] == ///
					Student_Permnum[_n-1] & loc[_n] == loc[_n-1]
									
				keep if distinct_episode!=0
				duplicates report Student_Permnum loc  // there should be some
				
				* let's see them
				duplicates tag Student_Permnum loc, gen(dup)
				gsort -dup Student_Permnum Attendance_Date
				
				* aggregate
				collapse (count) num_enr=distinct_episode, by(Student_Permnum)
					
			tempfile schls
			save `schls', replace
		restore
						
		preserve
			rename Attendance_Day_Number SchoolDay
			destring SchoolDay, replace
			collapse (count) enr_days=SchoolDay, by (Student_Permnum)
			merge 1:1 Student_Permnum using `schls', nogen keep(1 3)
			rename Student_Permnum id
			gen mob${histyr2}${histyr1}=(num_enr-1)/enr_days //single enrollment
			kdensity mob${histyr2}${histyr1} if mob${histyr2}${histyr1}!=0
			isid id
		restore
	!!CHANGED AT REQUEST OF IMT TO USE GSDR.GEMS.SASI_AENR */
use data\prep\sasi_mobility${histyr2}${histyr1}_hs, clear
	
${save} data\prep\mobility${histyr2}${histyr1}_hs, replace

		
********************************************************************************
*then CRCT data - to determine prior achievement and eval year performance
	* get last CRCT available - could experiment with a summary of past CRCTs
********************************************************************************

	#delimit ;
		odbc load, clear exec	("
			SELECT * 
			FROM 	Assessment.dbo.TEST_STU_CRT 
			WHERE 	EXAM_ADMIN_DATE > 20${histyr5}0801 and 
					EXAM_ADMIN_DATE < 20${evalyr}0801 and SUBJECT!= 'SS'
								")
		dsn(ODS_Prod_Assm) user(Research) pass(Research);
	#delimit cr	
${save} data/orig/crct_hs, replace
			
		foreach v of varlist * {
			rename `v' `=lower("`v'")'
		}

		keep school_yr exam_admin_date loc grade stunumb test_key ftenumb ///
			 tested_irreg_flag subject non_std_admin total_scale_scr
		duplicates drop
		rename school_yr year
		rename exam_admin_date date
		rename stunumb id
		rename total_scale_scr ss_tot
		order  year id loc grade date test_key ftenumb tested_irreg_flag ///
			   subject non_std_admin ss_tot

		*check on irregularities or non-standard administrations
		tab tested_irreg_flag, m
		tab non_std_admin, m 	// for this use we will keep those 
								//flagged by either of these - 
								//better than no achievement proxy

		*remove those with 0 since not possible (is missing?)
		drop if ss_tot==. | ss_tot==0

		*create yr grade subj
		gen tname = "CRCT"
		egen test = concat(year tname grade subject), p(" ")
		drop tname
		tab test, m

		*see if only one year-test-grade-subject comb for each student
		duplicates report id test
		
		* keep only latest year-test-grade-subject combination per year
		bys id test (date): gen n = _n
		tab n, m
		bys id test: egen max_n = max(n)
		keep if n == max_n
		isid id test
			drop n max_n
			
		* keep latest grade subject result 
			* (sort by year-test-grade-subject to choose)
		bys id subject (test): gen n = _n
		tab n, m
		bys id subject: egen max_n = max(n)
		keep if n == max_n
		isid id subject
			drop n max_n
${save} data/prep/crct_extr_hs, replace

		*reshape into student by variables format
		drop loc date test_key ftenumb tested_irreg_flag ///
			 non_std_admin test year grade
		order id subject ss_tot
		reshape wide ss_tot, i(id) j(subject)string
		isid id
${save} data/prep/crct_final_hs, replace

*******************************************************************************************************************
*then pseudo-GPA data from core courses - for use to determine prior achievement as well as eval year performance
	* SchoolYear is fall of academic year (e.g., 2008 = 2008-09 school year) but Maurice adds 1 to make same
	* as ODS_SchoolYear
*******************************************************************************************************************

* USE THIS + XFAT on course numbering to understand which students passed EOC reqs 
														
	#delimit ;
		odbc load, clear exec	("
			SELECT	* 
			FROM	[Predictive_Analytics].[PAVIEW2].[v_Student_Course_History_DETAIL]
			WHERE 	SchoolYear >= 20${histyr5} and
					SchoolYear <= 20${evalyr} and 
					Grade in ('03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
					")
		dsn(ODS_Prod_PA) user(Research) pass(Research);
	#delimit cr
		foreach var of varlist* { 
			destring `var', replace
			rename `var' `=lower("`var'")'
		}
	replace coresubjectcode = "SC" if strpos(longtitle , "Science")
	replace coreind = 1 if coresubjectcode == "SC"
${save} data/orig/gpa_hs, replace
	
*********************************
* promotion data
*********************************
	#delimit ;
		odbc load, clear exec("
			SELECT	 [Permnum]
					,[SchoolYear]
					,[RepeatingGradeInd]
					,[EndYear_Grade]      
					,[StartYear_Grade]
			  FROM 	[Predictive_Analytics].[PAVIEW2].[v_Student_Retention_History]
			WHERE 	SchoolYear in (20${histyr1}, 20${evalyr}, 20${nxtyr}, 2014)")
		dsn(ODS_Prod_PA) user(Research) pass(Research);
	#delimit cr
		foreach var of varlist* { 
			destring `var', replace
			rename `var' `=lower("`var'")'
		}
	rename permnum id
${save} data/orig/promotion_hs, replace

*********************************
* discipline data
*********************************
		local eyr ${evalyr}
		local hyr ${histyr1}
		local hyr2 ${histyr2}

	foreach yr in `eyr' `hyr' `hyr2' {

		#delimit;
			odbc load, clear exec	("
			declare @BeginingDate as date , @EndingDate as date, @SchoolYear int

			SELECT @SchoolYear = 20`yr'

			SELECT @BeginingDate = min([Date]) 
			FROM CALENDAR 
			WHERE EFF_END_DATE IS NULL AND SchoolYear = @SchoolYear AND SchoolDay <> 0

			SELECT @EndingDate = max([Date]) 
			FROM CALENDAR 
			WHERE EFF_END_DATE IS NULL AND SchoolYear = @SchoolYear AND SchoolDay <> 0;

			WITH CTE_REGISTER_DETAILS AS (
				SELECT STUDENT_KEY, COUNT(*) AS EnrolledDays
				FROM REGISTER_DETAILS WITH (NOLOCK)
				WHERE EFF_END_DATE IS NULL AND ODS_SchoolYear = @SchoolYear
				GROUP BY STUDENT_KEY
			)

			SELECT  
				ASCH.SchoolNumber
			,	ASTU.PermNum
			,	ADIS.Sequence AS Incident_ID
			,	ADPO.Sequence
			,	SA.Grade
			,	ADIS.ReportDate
			,	ATBL.DispositionCode
			,	ATBL.DispositionDescription
			,	AADD.Code
			,	AADD.[Description]
			,	ADPO.DispositionDays
			,	ADPO.DispositionStartDate
			,	ADPO.DispositionEndDate
			,	CTE.EnrolledDays
			FROM DISCIPLINE.DISPOSITION AS ADPO WITH (NOLOCK)
			INNER JOIN STUDENT AS ASTU WITH (NOLOCK)
				ON ADPO.STUDENT_KEY = ASTU.STUDENT_KEY 
			INNER JOIN STUDENT_ACADEMICS AS SA WITH (NOLOCK)
				ON ADPO.STUDENT_KEY = SA.STUDENT_KEY 
			INNER JOIN SCHOOL AS ASCH WITH (NOLOCK)
				ON ADPO.SCHOOL_KEY = ASCH.SCHOOL_KEY
			INNER JOIN DISCIPLINE.INCIDENT AS ADIS WITH (NOLOCK)
				ON ADPO.INCIDENT_KEY = ADIS.INCIDENT_KEY 
			INNER JOIN DISCIPLINE.DISPOSITION_CODE AS ATBL WITH (NOLOCK)
				ON ADPO.DISPOSITION_CODE_KEY = ATBL.DISPOSITION_CODE_KEY 
			INNER JOIN DISCIPLINE.INFRACTION_RULES AS AADD WITH (NOLOCK)
				ON ADIS.MAJOR_INFRACTION_RULES_KEY = AADD.INFRACTION_RULES_KEY 
			INNER JOIN CTE_REGISTER_DETAILS AS CTE
				ON ADPO.STUDENT_KEY = CTE.STUDENT_KEY
			WHERE 
				(ADIS.EFF_END_DATE IS NULL) AND 
				(ADIS.ODS_SchoolYear = @SchoolYear) AND 
				(ADIS.ReportDate >= @BeginingDate) AND 
				(ADIS.ReportDate <= @EndingDate) AND 
				(ISNULL(ASTU.PermNum,0) <> 0) AND 
				(EXISTS (SELECT * 	
						 FROM 	STUDENT_DISTRICT_DETAIL 
						 WHERE 	EFF_END_DATE IS NULL AND 
								SchoolYear = @SchoolYear AND 
								STUDENT_KEY = ADPO.STUDENT_KEY)) AND 
				(EXISTS (SELECT * 
						 FROM 	ENROLLMENT 
						 WHERE 	EFF_END_DATE IS NULL AND 
								ODS_SchoolYear = @SchoolYear AND 
								TRANSYEAR = RIGHT(CAST(@SchoolYear - 1 AS varchar),2) AND 
								STUDENT_KEY = ADPO.STUDENT_KEY))
								")
			dsn(ODS_Prod_DS) user(Research) pass(Research);
		#delimit cr

		foreach v of varlist * {
			rename `v' `=lower("`v'")'
		}
${save} data/prep/discipline_enroll_20`yr'_hs, replace			
	} //END FOREACH yr


	
/********************************
*then histyr year Gateway Scores
*********************************

	#delimit;
		odbc load, clear exec	("
			SELECT	* 
			FROM 	ResearchAndEvaluation.dbo.V_HSGateway_Score 
			WHERE 	School_Year = 20${evalyr}")
		dsn(ODS_Prod_RE) user(Research) pass(Research);
	#delimit cr
		foreach var of varlist* { 
			destring `var', replace
			rename `var' `=lower("`var'")'
		}	
		format exam_admin_date %8.0f
		keep if scale_score > 0
		keep if exam_admin_date >= 20130401 & exam_admin_date <= 20130430
		replace test_name = trim(test_name)
		keep if regexm(test_name, "RET") != 1   // use retakes next year?
			rename student_number id
			isid id subject
${save} data/orig/gwy${evalyr}, replace */
	
********************************
*then passed Gateway ever
*********************************			
	// options are (not pass) 'DNA', 'FAIL', ' '; (pass) 'EFF', 'EXC',  'MIN'
	// think L in LAPERF is LA and C in CPERF is for Content

	#delimit ;
		odbc load, clear exec	("
			SELECT [STUNUMB] as id
			  ,[EXAM_ADMIN_DATE] as admin
			  ,[SUBJECT] as subj
			  ,[SCHOOL_YEAR] as year
			  ,[CPERF] as contentPerf
			  ,[LPERF] as langArtsPerf
			  ,CASE WHEN [CPERF] in ('EFF', 'EXC',  'MIN') THEN 1 ELSE 0 END as contentPass
			  ,CASE WHEN [LPERF] in ('EFF', 'EXC',  'MIN') THEN 1 ELSE 0 END as langArtsPass
			FROM [Assessment].[dbo].[TEST_STU_GWY10_Temp]
			WHERE [INVALID_IND] = 'N' --checked and only Y or N
								")
		dsn(ODS_Prod_Assm) user(Research) pass(Research) ;
	#delimit cr
		foreach var of varlist* { 
			destring `var', replace
		}				
${save} data/orig/gwyPassEver, replace 
	
********************************
*then passed HSWT ever
*********************************
	*options here are (not pass) 'DNA', 'FAIL', ' '; (pass) 'EFF', 'EXC',  'MIN'
	*CPERF, LPERF - think L is LA and C is for Content
	*INVALID_IND - checked and only Y or N
	#delimit ;
		odbc load, clear exec("
			SELECT [STUNUMB] as id
			  ,[EXAM_ADMIN_DATE] as admin
			  ,[SUBJECT] as subj
			  ,[SCHOOL_YEAR] as year
			  ,[CPERF] as contentPerf
			  ,[LPERF] as langArtsPerf
			  ,CASE WHEN [CPERF] in ('EFF', 'EXC',  'MIN') THEN 1 ELSE 0 END as contentPass
			  ,CASE WHEN [LPERF] in ('EFF', 'EXC',  'MIN') THEN 1 ELSE 0 END as langArtsPass
			FROM [Assessment].[dbo].[TEST_STU_GWY10_Temp]
			WHERE [INVALID_IND] = 'N' 
							")
		dsn(ODS_Prod_Assm) user(Research) pass(Research) ;
	#delimit cr	
		foreach var of varlist* { 
			destring `var', replace
		}				
	${save} data/orig/hswtPassEver, replace 
																			
**************************************************
* Date entered 9th grade / entered cohort + grade
**************************************************
//get from Datamart SASI; set to 8 to 3 year historical interval. from current date or fixed date


	#delimit ;
		odbc load, clear exec	("
			SELECT	 	 t1.[PermNum]
						,t2.[Date9thGrade]
						,t2.[GraduationDate]
						,CEILING(DATEDIFF(YEAR,t2.Date9thGrade,t2.GraduationDate)) 
											as YearsToGrad
			FROM  		[Datamart_SASI].[dbo].[STUDENT] t1 INNER JOIN
						[Datamart_SASI].[dbo].[STUDENT_DATES] as t2 ON
						t1.STUDENT_KEY = t2.STUDENT_KEY
			WHERE		t2.[Date9thGrade] is not null and 
						t2.[Date9thGrade] > DATEADD(YEAR, -(${yrsbk}+3), CONVERT(datetime,convert(char(8), 20${nxtyr}0801))) and 
						t2.[Date9thGrade] < DATEADD(YEAR, -(${yrsbk}-2), CONVERT(datetime,convert(char(8), 20${nxtyr}0801))) and
						LEN(t1.[PermNum]) >= 7
			ORDER BY 	t2.[Date9thGrade] ASC
								")
								
						

						
						
						
		dsn(ODS_Prod_MA) user(Research) pass(Research);
	#delimit cr

		foreach var of varlist* { 
			destring `var', replace
		rename `var' `=lower("`var'")'
		}
		
		//benefit of doubt approach (latest 9th, earliest grad)
		duplicates drop
		duplicates tag permnum, gen(_dup)
		bys permnum: egen _mn = min(graduationdate)
		drop if _dup > 0 & _mn != . & graduationdate == .
		duplicates drop permnum yearstograd, force
		
		//needs work
		duplicates drop permnum, force
		rename permnum id
		drop _*
${save} data/prep/yearstograd, replace

**************************************************
* SEI
**************************************************

	#delimit ;
		odbc load, clear exec("
			SELECT [loc]
				  ,[season]
				  ,[school_year]
				  ,[student_id]
				  ,[fg]
				  ,[fsl]
				  ,[sei_all]
			  FROM [Assessment].[dbo].[SEI]
			  WHERE season = 'Spring' and
					school_year in (20${histyr3}, 20${histyr2}, 20${histyr1}) or
					season = 'Fall' and school_year in (20${histyr4}, 20${histyr3}, 
					20${histyr2})
							")
		dsn(ODS_Prod_Assm) user(Research) pass(Research);
	#delimit cr
		foreach var of varlist* { 
			destring `var', replace
			rename `var' `=lower("`var'")'
		}
		rename student_id id
		
preserve
	collapse (mean) schl_fg = fg schl_fsl = fsl schl_sei_all = sei_all, by(loc)
	rename loc loc_${histyr2}${histyr1}
	save data/prep/schl_sei, replace
restore
	collapse (mean) fg fsl sei_all, by(id)

${save} data/prep/sei, replace



*zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
*TURNED OFF zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
/*

*NOTE* only has current students; needs db2 control center app + id & pw;
maurice investigating extensibility of this query

**************************************************
* Failed to pass any expected 9-11 EOCT courses
**************************************************
insheet using "data/orig/EOCT_GRAD_REQ.csv", c clear
	rename (v1-v18) (id subtest ninth tenth eleventh twelfth schyr_id ///
						grd data_as_of_dt data_changed_ts eight ///
						grade9entrydt student_last_nm student_first_nm ///
						schoolnum school_nm grade_level orig_grd9th_entrydt)
	foreach v in ninth tenth eleventh {
		replace `v' = "" if `v' == "N/A"
		gen _missing_`v' = 1 if (`v' == "N/C" | `v' == "No") & `v' != ""
		replace _missing_`v' = 0 if (`v' == "Yes" | `v' == "N/E") & `v' != ""
	}
		
	keep if grd == `v' 
	egen missing11th = rowtotal(_*)
	collapse (sum) missing11th (count) N = missing11th, by(id)
	gen percEOCTmiss11th = missing11th/N
				
* convert to form matching MYs view for OAC	
						
* one row per student by school for OAC				
	collapse (min) missing schyr_id grd data_as_of_dt, ///
	by(id student_last_nm student_first_nm schoolnum school_nm)
	gsort school_nm -missing student_last_nm student_first_nm
*/
*TURNED OFF zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
*zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz

*************************************************
* pass Gateway, HSGWST, and GHSGTs if relevant
*************************************************

* maurice checking; contacted michael quast on 

**************************************************
* EOCT Assessment Results
**************************************************
#delimit ;
		odbc load, clear exec("
SELECT [SCHOOL_YR]
      ,[STUNUMB]
      ,[SUBJECT]
      ,[EXAM_ADMIN_DATE]
      ,[GRADE]
      ,[LOC]
      ,[TOTAL_PERF_LEVEL]
      ,[TOTAL_SCALE_SCORE]
FROM [Assessment].[dbo].[TEST_STU_ECT]
WHERE EXAM_ADMIN_DATE >= 20${histyr3}0801 and
	  EXAM_ADMIN_DATE <= 20${evalyr}0731 
")
		dsn(ODS_Prod_Assm) user(Research) pass(Research);
	#delimit cr
	
	${outsheet} using data/orig/act${histyr4}${evalyr}.csv, c replace

	foreach var of varlist* { 
		destring `var', replace
		rename `var' `=lower("`var'")'
		}
			replace subject = trim(subject)

		duplicates drop
		rename school_yr year
		rename exam_admin_date date
		rename stunumb id
		rename total_scale_score ss_tot
		format id %13.0f
		
		*remove those with 0 since not possible (is missing?)
		drop if ss_tot==. | ss_tot==0

		*create yr grade subj
		gen tname = "EOCT"
		egen test = concat(year tname subject), p(" ")
		drop tname
		tab test, m

*see if only one year-test-grade-subject comb for each student
		duplicates report id test
		
* keep only latest year-test-grade-subject combination per year
		bys id test (date): gen n = _n
		tab n, m
		bys id test: egen max_n = max(n)
		keep if n == max_n
		isid id test
			drop n max_n
			
		* keep latest grade subject result 
			* (sort by year-test-grade-subject to choose)
		bys id subject (test): gen n = _n
		tab n, m
		bys id subject: egen max_n = max(n)
		keep if n == max_n
		isid id subject
		drop n max_n
		order id loc year grade test date subject
		sort id year

*********************
* calculate z-scores
*********************
	
	* historic year 2
	***************
		* set path
		local crt_pth = "${path}\RBES\" +  ///
		"Description\Original_DOE-CRCT-EOCT-HSGT_Mns-SDs"
	
	* bring in values from GA DOE
preserve
	foreach su in 9TH AMLIT BIO ECO MAI MAII PHY USH {
		import excel using /// 
		"`crt_pth'\20${histyr2}\State with SD only\EOCT_SP10_`su'_State.xls", ///
			sheet("EOCT_SP10_`su'_State") cellrange(B2:G3) firstrow clear
				keep MeanScaleScore StandardDeviation
				rename(MeanScaleScore StandardDeviation) ///
					  (mn sd) 
			gen subject = "`su'"
			gen year = 20${histyr2}
			tempfile eoct_`su'_ga_$histyr2
			save `eoct_`su'_ga_$histyr2', replace
		}
	use `eoct_9TH_ga_$histyr2', clear
		append using `eoct_AMLIT_ga_$histyr2'
		append using `eoct_BIO_ga_$histyr2'
		append using `eoct_ECO_ga_$histyr2'
		append using `eoct_MAI_ga_$histyr2'
		append using `eoct_MAII_ga_$histyr2'
		append using `eoct_PHY_ga_$histyr2'
		append using `eoct_USH_ga_$histyr2'
		
		replace subject = "MA1" if subject == "MAI"
		replace subject = "MA2" if subject == "MAII"
		replace subject = "AME" if subject == "AMLIT"
		replace subject = "ECO" if subject == "ECON"

	tempfile eoct_mean_sd_$histyr2
	save `eoct_mean_sd_$histyr2', replace
	
restore

* historic year 1
	***************
		* set path
		local crt_pth = "${path}\RBES\" +  ///
		"Description\Original_DOE-CRCT-EOCT-HSGT_Mns-SDs"
		* bring in values from GA DOE
preserve
	foreach su in 9TH AMLIT BIO ECON MAI MAII PHY USH {
		import excel using /// 
		"`crt_pth'\20${histyr1}\EOCT_SP20${histyr1}_State_Summaries\SP${histyr1}_state_`su' with SD.xls", ///
			sheet("SP11_state_`su'") cellrange(A2:I3) firstrow clear
				keep MeanScaleScore StandardDeviation
				rename(MeanScaleScore StandardDeviation) ///
					  (mn sd) 
			gen subject = "`su'"
			gen year = 20${histyr1}
			tempfile eoct_`su'_ga_$histyr1
			save `eoct_`su'_ga_$histyr1', replace
		}
	use `eoct_9TH_ga_$histyr1', clear
		append using `eoct_AMLIT_ga_$histyr1'
		append using `eoct_BIO_ga_$histyr1'
		append using `eoct_ECON_ga_$histyr1'
		append using `eoct_MAI_ga_$histyr1'
		append using `eoct_MAII_ga_$histyr1'
		append using `eoct_PHY_ga_$histyr1'
		append using `eoct_USH_ga_$histyr1'
		
		replace subject = "MA1" if subject == "MAI"
		replace subject = "MA2" if subject == "MAII"
		replace subject = "AME" if subject == "AMLIT"
		replace subject = "ECO" if subject == "ECON"

	tempfile eoct_mean_sd_$histyr1
	save `eoct_mean_sd_$histyr1', replace
	
restore

* evaluation year 1
	***************
		* set path
		local crt_pth = "${path}\RBES\" +  ///
		"Description\Original_DOE-CRCT-EOCT-HSGT_Mns-SDs"
		* bring in values from GA DOE
preserve
	foreach su in 9TH AMLIT BIO ECON MAI MAII PHY USH ALG GEO {
		import excel using /// 
		"`crt_pth'\20${evalyr}\SP${evalyr}_state_`su' with SD.xls", ///
			sheet("SP12_state_`su'") cellrange(A2:I3) firstrow clear
				keep MeanScaleScore StandardDeviation
				rename(MeanScaleScore StandardDeviation) ///
					  (mn sd) 
			gen subject = "`su'"
			gen year = 20${evalyr}
			tempfile eoct_`su'_ga_$evalyr
			save `eoct_`su'_ga_$evalyr', replace
		}
	use `eoct_9TH_ga_$evalyr', clear
		append using `eoct_AMLIT_ga_$evalyr'
		append using `eoct_BIO_ga_$evalyr'
		append using `eoct_ECON_ga_$evalyr'
		append using `eoct_MAI_ga_$evalyr'
		append using `eoct_MAII_ga_$evalyr'
		append using `eoct_PHY_ga_$evalyr'
		append using `eoct_USH_ga_$evalyr'
		append using `eoct_ALG_ga_$evalyr'
		append using `eoct_GEO_ga_$evalyr'		
		
		replace subject = "MA1" if subject == "MAI"
		replace subject = "MA2" if subject == "MAII"
		replace subject = "AME" if subject == "AMLIT"
		replace subject = "ECO" if subject == "ECON"
	tempfile eoct_mean_sd_$evalyr
	save `eoct_mean_sd_$evalyr', replace

restore

preserve

use `eoct_mean_sd_$evalyr', clear
append using `eoct_mean_sd_$histyr1'
append using `eoct_mean_sd_$histyr2'
tempfile save eoct_mean_sd_final
save `eoct_mean_sd_final'

restore
	* merge with current dataset
	merge m:1 subject year using `eoct_mean_sd_final', nogen keep(1 3) ///
								keepus(mn sd)

	* check no missing state data and calculate z-scores
	gen double z = .
	bysort year subject: replace z = (ss_tot - mn)/ sd
					
					* check transformation is linear
					scatter ss z, by(subject year) 
					${graph} export results/graphs/`subj'`yr'z_eth.wmf, replace

		codebook
		summarize
keep id year subject ss_tot z
rename (ss_tot z) (ss_ z_)
reshape wide ss_ z_, i(id year) j(subject) string

save data/prep/eoct${histyr2}${evalyr}, replace

**************************************************
* ACT Assessment Results
**************************************************
	#delimit ;
		odbc load, clear exec("

						SELECT [STUNUMB]
							  ,[TEST_KEY]
							  ,[EXAM_ADMIN_DATE]
							  ,[CORRECTED_REPORT]
							  ,[SUBJECT]
							  ,[SCALE_SCORE]
						  FROM [Assessment].[dbo].[TEST_STU_ACT]
						  WHERE EXAM_ADMIN_DATE >= 20${histyr4}0801 and
								EXAM_ADMIN_DATE <= 20${evalyr}0531 and
								CORRECTED_REPORT = '' and
								SUBJECT in ('EN', 'MA', 'RD', 'SC')
						")
		dsn(ODS_Prod_Assm) user(Research) pass(Research);
	#delimit cr
${outsheet} using data/orig/act${histyr4}${evalyr}.csv, c replace

		foreach var of varlist* { 
			destring `var', replace
			rename `var' `=lower("`var'")'
		}
		replace subject = trim(subject)
		rename (stunumb scale_score) (id score)
		foreach subj in EN MA RD {
			replace subject = "a`subj'" if subject == "`subj'"
		}
			
		*keep most highest ACT result //consistent w/ scores sent to a college
		keep if score != . & score != 0
			keep if score != . & score != 0
			bys id subject (exam_admin_date): gen _cnt = _n
			bys id subject: egen _mx = max(_cnt)
			keep if _mx == _cnt
			drop _* 	

		drop test_key exam_admin_date corrected_report
			reshape wide score, i(id) j(subject) s
${save} data/prep/act${histyr4}${evalyr}, replace

**************************************************
* PSAT Assessment Results - predictor
**************************************************
#delimit ;

odbc load, clear exec("

						SELECT [STUNUMB]
							  ,[GRADE]
							  ,[TEST_KEY]
							  ,[EXAM_ADMIN_DATE]
							  ,[SUBJECT]
							  ,[SUBJECT_SCORE]
						  FROM [Assessment].[dbo].[TEST_STU_PSA]
						  WHERE EXAM_ADMIN_DATE <= 20${histyr1}0531 and
								EXAM_ADMIN_DATE >= 20${histyr5}0801 and
								NONSTANDARD_ADM = '' and
								SUBJECT in ('MA', 'VE', 'WR')
						")
					dsn(ODS_Prod_Assm) user(Research) pass(Research);
					#delimit cr
					
					outsheet using data/orig/psat${histyr2}${histyr1}.csv, c replace
		
		replace SUBJECT = "CR" if SUBJECT == "VE"
	
		foreach var of varlist* { 
		destring `var', replace
		rename `var' `=lower("`var'")'
		}
			replace subject = trim(subject)
			rename (stunumb subject_score) (id psat_score)
			
			*keep 10th PSAT result if exists
				keep if psat_score != . & psat_score != 0 & grade !=. & grade != 0
				gen notTenth = grade != 10
				bys id subject (notTenth exam_admin_date): gen n = _n
				keep if n == 1
				drop n
		
		drop test_key exam_admin_date
			reshape wide psat_score, i(id grade) j(subject) s
			
			* collapse and check if any scores averaged across administrations
				collapse (max) grade notTenth (sum) psat* (count) ///
					psat_scoreCR_nu = psat_scoreCR ///
					psat_scoreMA_nu = psat_scoreMA ///
					psat_scoreWR_nu = psat_scoreWR, by(id)
					
				* check counts of summed values
				assert (psat_scoreCR_nu+psat_scoreMA_nu+psat_scoreWR_nu) <= 3
				drop *nu
		
		* collReady variable based upon collegeBoard grade-specific thresholds
		gen psat_collReady = 0 if 	psat_scoreCR != . & psat_scoreMA != . & ///
									psat_scoreWR != .
		replace psat_collReady = 1 if 	psat_scoreCR != . & psat_scoreCR >= 42 & ///
										psat_scoreMA != . & psat_scoreMA >= 42 & ///
										psat_scoreWR != . & psat_scoreWR >= 42 & ///
										(grade == 9 | grade == 10)
		replace psat_collReady = 1 if 	psat_scoreCR != . & psat_scoreCR >= 45 & ///
										psat_scoreMA != . & psat_scoreMA >= 47 & ///
										psat_scoreWR != . & psat_scoreWR >= 45 & ///
										(grade == 11 | grade == 12)
			drop notTenth
			isid id
		save data/prep/psat${histyr2}${histyr1}, replace

**************************************************
* SAT Assessment Results - OUTCOME variable
**************************************************
/* Notes: 
	-keep most recent SAT result consistent w/ CollegeBoard study
	-


*/
#delimit ;

odbc load, clear exec("

						SELECT [STUNUMB]
							  ,[TEST_KEY]
							  ,[EXAM_ADMIN_DATE]
							  ,[NONSTAND_IND]
							  ,[SUBJECT]
							  ,[SCORE]
						  FROM [Assessment].[dbo].[TEST_STU_SAT]
						  WHERE EXAM_ADMIN_DATE >= 20${histyr4}0801 and
								EXAM_ADMIN_DATE <= 20${evalyr}0531 and
								NONSTAND_IND = '' and
								SUBJECT in ('MA', 'VE')
						")
					dsn(ODS_Prod_Assm) user(Research) pass(Research);
					#delimit cr
					
					outsheet using data/orig/sat${histyr4}${evalyr}.csv, c replace
	
		foreach var of varlist* { 
		destring `var', replace
		rename `var' `=lower("`var'")'
		}
			replace subject = trim(subject)
			rename stunumb id
			
			*keep most recent SAT result
				keep if score != . & score != 0
				bys id subject (exam_admin_date): gen _cnt = _n
				bys id subject: egen _mx = max(_cnt)
				keep if _mx == _cnt
				drop _* 
		
		drop test_key exam_admin_date nonstand_ind
			reshape wide score, i(id) j(subject) s
	
		save data/prep/sat${histyr4}${evalyr}, replace



log close


		}
	

if $calc==1 {

log using code/logfiles/calc.smcl, replace

************************************
* Historic year school-level values
	* uses data prepared for SARs
************************************

* path to SARs data
local p = "S:\Superintendent\Private\Strategy & Performance\ResearchEvaluation\RBES\" + ///
			"School Accountability Rpts\Issued 20" + "${histyr1}" + "-" + "${evalyr}" + ///
			" (For 20" + "${histyr2}" + "-" + "${histyr1}" + ")\Student\data_prep"

preserve

	* school size, % white
	use "`p'\FT002_20${histyr1}", clear
		keep if SchoolYear == 20${histyr1}
	merge m:m SchoolNumb using "${ssipath}\data/prep/stateNum_to_gcpsNum_link", nogen keep(1 3)
		assert loc != .
		isid loc Grade Gender
	collapse (sum) White TotalEnrollment, by(loc)
		gen schl_percWht_H1 = White/TotalEnrollment
	rename TotalEnrollment schlEnr_H1
		drop White
	save data/prep/schl_enr, replace

	* school %esol, %sped
	
	use "`p'\FT004_20${histyr1}", clear
		keep if SchoolYear == 20${histyr1}
	merge m:m SchoolNumb using "${ssipath}\data/prep/stateNum_to_gcpsNum_link", nogen keep(1 3)
		assert loc != .
		isid loc Program Gender
			keep if Program == "Special Education"
	collapse (sum) TotalEnrollment, by(loc)
		merge 1:1 loc using data/prep/schl_enr, nogen keep(1 3)
		gen schl_percSPED_H1 = TotalEnrollment/schlEnr_H1
		keep loc schl_percSPED_H1
	save data/prep/schl_sped, replace	
	
	use "`p'\FT003_20${histyr1}", clear
		keep if SchoolYear == 20${histyr1}
	merge m:m SchoolNumb using "${ssipath}\data/prep/stateNum_to_gcpsNum_link", nogen keep(1 3)
		assert loc != .
		isid loc Program Gender
			keep if Program == "ESOL-Total"
	collapse (sum) TotalEnrollment, by(loc)
		merge 1:1 loc using data/prep/schl_enr, nogen keep(1 3)
		gen schl_percESOL_H1 = TotalEnrollment/schlEnr_H1
		keep loc schl_percESOL_H1
	save data/prep/schl_esol, replace	
		
	* % FRPL
	use "`p'\FRPL_20${histyr1}", clear
		keep if SchoolYear == 20${histyr1}
	merge m:m SchoolNumb using "${ssipath}\data/prep/stateNum_to_gcpsNum_link", nogen keep(1 3)
		assert loc != .
		isid loc
	rename PercentEligibleFreeOrReduced schlFRL_H1
		keep loc schlFRL_H1
	save data/prep/schl_frl, replace

	* % enrolled days attended
	use "`p'\ENR021_20${histyr1}", clear
		keep if SchoolYear == 20${histyr1}
	merge m:m SchoolNumb using "${ssipath}\data/prep/stateNum_to_gcpsNum_link", nogen keep(1 3)
		assert loc != .
		isid loc
	rename AverageDailyAttendance schlAtt_H1
		keep loc schlAtt_H1
	save data/prep/schl_att, replace

restore


********************
* Census data merge
********************

preserve
	use data/prep/wsa_census_dep, clear

		foreach var of varlist* { 
		destring `var', replace
		rename `var' `=lower("`var'")'
		}
	format %13.0f fips
	tempfile absm
	save `absm', replace
	
	use data/prep/wsa_fips_sy2011, clear
		
		foreach var of varlist* { 
		destring `var', replace
		rename `var' `=lower("`var'")'
		}
		format %13.0f fips
	merge m:1 fips using `absm', nogen keep(1 3)
			foreach var of varlist* {
			rename `var' `var'_H1
			}
			rename (id_H1 fips_H1) (id fips)
	
			
	save data/prep/census, replace
restore

*****************************************************************
*DISC - format to disc metric established w/ Bryan Long's Office
*****************************************************************
local eyr ${evalyr}
local hyr ${histyr1}
local hyr2 ${histyr2}

foreach yr in `eyr' `hyr' `hyr2' {

use data/prep/discipline_enroll_20`yr'_hs, clear

*extract date portion of %tc
		gen inciddt = dofc(reportdate)
		format inciddt %td
		gen dispstartdt = dofc(dispositionstartdate)
		format dispstartdt %td
		gen dispenddt = dofc(dispositionenddate)
		format dispenddt %td
		tab inciddt, m  
		drop reportdate dispositionstartdate dispositionenddate

			foreach v in days code description {
			rename	disposition`v' disp`v'
			} //END FOREACH v
			rename permnum id

						*verify dispdays is numeric - if not, assert step will generate "type mismatch"
						assert sum(dispdays)>=0
			
			preserve
						*summary of ISS/OSS days
						*use extracts/discipline_enroll_${schyr}v2, clear
						collapse (sum) dispdays if dispcode=="SISS" | dispcode=="SOTH" | dispcode=="SSTO" | dispcode=="SXLT", by (id)
							isid id
							tempfile susp_freq_`yr'
							save `susp_freq_`yr'', replace
			restore

/*****************************************************************************************************************
	Formatting for the disciplinary severity metric begins here
*****************************************************************************************************************/

										**aggregate behavioral data into count of non-rule 12 incidents, 
										**mean of enrolled days (b/c repeated for each case), sum of ISS and OSS days
										assert !missing(sequence)
								preserve
										collapse (mean) enrolleddays (count) sequence if dispcode!="NR12" & dispcode!="OR12", by (id)
											isid id
											tempfile incid_freq_`yr'
											save `incid_freq_`yr'', replace
								restore
											
										*Recode Disposition Days into factor added to severity value
										tab dispdays, m
								
								gen dispdaytrunc = .
									replace dispdaytrunc = dispdays if dispdays > 0 & dispdays < 10 & (dispcode=="SISS" | ///
									dispcode=="SOTH" | dispcode=="SSTO" | dispcode=="SXLT")
									replace dispdaytrunc = 10 if dispdays >= 10 & (dispcode=="SISS" | dispcode=="SOTH" | dispcode=="SSTO" | ///
									dispcode=="SXLT")
									replace dispdaytrunc = dispdays if dispdays > 0 & dispdays < 20 & (dispcode=="SB1" | dispcode=="SB11")
									replace dispdaytrunc = 20 if dispdays >= 20 & (dispcode=="SB1" | dispcode=="SB11")
									replace dispdaytrunc = 0 if dispdaytrunc ==.

									tab dispdaytrunc, m

									gen dispdayfactor = .
										replace dispdayfactor = dispdaytrunc/10 if (dispcode=="SISS" | dispcode=="SOTH" | dispcode=="SSTO" | dispcode=="SXLT")
										replace dispdayfactor = dispdaytrunc/20 if dispdayfactor==. // is missing if bus suspension days or not suspended either way zero in num = 0.
									tab dispdayfactor, m
									
									tempfile discipline_enroll_`yr'_metric
									save `discipline_enroll_`yr'_metric', replace
							

*merge in weight // new weights provided by Keith (7/5/11) - he asked we verify with Bryan upon his return 7/18
preserve
	insheet using "${path}\Jim_Appleton\RBES\NonRegSchools\Propensity_Score_Analysis\3rd_Round\orig\Disposition_Codes_Weights_Revised_Keith&Bryan.csv", clear
		rename dispositioncode dcode
		gen dispcode = trim(dcode)
		drop dcode
		tempfile dispcodewts
		save `dispcodewts', replace
restore
	rename dispcode dcode
	gen dispcode = trim(dcode)
	drop dcode
merge m:1 dispcode using `dispcodewts', nogen keep(1 3)
	drop disposition
	tab weight dispdesc if weight==.
tab dispcode weight, m
hist(weight)

egen dispseverityvalue=rsum(weight dispdayfactor)
tab dispseverityvalue, m
replace dispseverityvalue=6 if dispseverityvalue>6
hist(dispseverityvalue)
summarize dispseverityvalue // not normally distributed
tempfile discipline_enroll_`yr'_metric2
save `discipline_enroll_`yr'_metric2', replace

collapse (count) cnt_dispseverityvalue=dispseverityvalue (max) max_dispseverityvalue=dispseverityvalue (mean) mean_dispseverityvalue=dispseverityvalue, by (id)
isid id
	tempfile disp_severity_`yr'
	save `disp_severity_`yr'', replace
	
*assemble the pieces
merge 1:1 id using `incid_freq_`yr'', nogen keep (1 3)
merge 1:1 id using `susp_freq_`yr'', nogen keep (1 3) // this portion not part of disc metrics but used in many datasets
codebook // only dispdays should have missing values
rename dispdays iss_oss_days
replace iss_oss_days = 0 if iss_oss_days==.
rename enrolleddays disc_enrdays
gen drate=cnt_dispseverityvalue/disc_enrdays
rename max_dispseverityvalue dsevmx
rename mean_dispseverityvalue dsevmn
foreach v in drate dsevmx dsevmn {
replace `v' = 0 if `v'==.
rename `v' `v'_`yr'
	} //END FOREACH v
	codebook drate
	save data/prep/discipline_enroll_`yr'_metric_final_hs, replace

} //END FOREACH yr


********************************************
* Pseudo-GPA calculation
********************************************
*!! schoolyear is fall of academic year !!*

	use data/prep/yearstograd, clear
		gen _dt9th = dofc(date9thgrade)
			gen mn9th = month(_dt9th)
			gen yr9th = year(_dt9th)
				replace  yr9th = yr9th + 1 if mn9th > 7 // +1 to get year ending
			gen yr8th = yr9th - 1
			gen yr10th = yr9th + 1
			gen yr11th = yr9th + 2
			gen yr12th = yr9th + 3
			keep if id > 100000 // filters impossible GCPS student IDs
				keep id yr*
				
		* compute expected grade for use below
		
		local hy4 = ${histyr4} //09
		local hy3 = ${histyr3} //10
		local hy2 = ${histyr2} //11
		local hy1 = ${histyr1} //12


		
		foreach yr in `hy4' `hy3' `hy2' `hy1' { 
				if (`yr' < 10) {			//just deals with 2 vs 1 digit
					gen grade_200`yr' = .
				}
				else {
					gen grade_20`yr' = .
				} //END IF gen
			
			forval grd = 8/11 {
				if (`yr' < 10) {
					replace grade_200`yr' = `grd' - (yr`grd'th - 200`yr')
				} 
				else {
					replace grade_20`yr' = `grd' - (yr`grd'th - 20`yr')
				} //END IF replace
			} //END FOR grd
		} //END FOR yr
		
			drop yr*
				
				tempfile grades
				save `grades', replace
				save data/prep/grades, replace
				


	* HSGPA + HS courses completed // Is GPA in high school courses
	use data/orig/gpa_hs, clear
	count
	rename permnum id
	
* subset rigor indicators //class passing
preserve
	keep id schoolyear failind apind ibind //giftedind honorsind 
		assert failind != .
		gen passind = failind == 0
		rename (apind ibind) (_api _ibi)
		gen apind = _api*passind
		gen ibind = _ibi*passind
			drop _*
	collapse (sum) apind ibind, by(id schoolyear)
		tempfile apib	
		save `apib', replace
	gen ap_ib_pass = apind > 0 | ibind > 0
		tempfile byYear
		save `byYear', replace
	********************************
		use `apib', clear
	collapse (sum) apind ibind, by(id)
		gen ap_ib_pass_from8th = apind > 0 | ibind > 0
		tempfile from8th
		save `from8th', replace
	use `byYear', clear
		keep if schoolyear == 20${histyr1}
		merge 1:1 id using `from8th', nogen keep(1 3)
			drop schoolyear apind ibind
			
* get dual enrollment students
*******************************
	
			
 	tempfile rigorInd
	save `rigorInd', replace
restore


* subset * failures in core courses indicators //no rdg only la ma sc ss ot
preserve

	keep if coreind == 1
	gen sccourseind = coresubjectcode == "SC"
	gen sscourseind = coresubjectcode == "SS"
	keep id grade schoolyear failind *courseind
		foreach s in la ma sc ss {
			gen `s'fai = failind*`s'courseind
		}
	collapse (sum) *fai, by(id schoolyear)
		foreach s in la ma sc ss {
			gen `s'fail = `s'fai > 0
		}
		drop *fai
		gen corefail = lafail == 1 | mafail == 1 | scfail == 1 | ssfail == 1
		tempfile byYear2
		save `byYear2', replace
	********************************
	collapse (sum) *fail, by(id)
		foreach s in la ma sc ss {
			gen `s'fail_from8th = `s'fail > 0
		}
		drop *fail
		gen corefail_from8th = 	lafail_from8th == 1 | mafail_from8th == 1 | ///
								scfail_from8th == 1 | ssfail_from8th == 1
		tempfile from8th2
		save `from8th2', replace
		
	use `byYear2', clear
		keep if schoolyear == 20${histyr1}
			drop schoolyear
		merge 1:1 id using `from8th2', nogen keep(1 3)
	merge 1:1 id using `rigorInd', nogen keep(1 3)
	
			foreach var of varlist* { 
			rename `var' `var'_${histyr2}${histyr1}
			}
			rename id_${histyr2}${histyr1} id
		
	save data/prep/failCore_rigorInd, replace
restore
	
preserve
		keep if schoolyear <= 20${evalyr} & grade >= 9 // histyr2 b/c completed course are predictor
	
	* first prep and get proportion of "course-completion" reqs met
                 * core_ind + subject
                         * end of 12th: 4 ELA, 4 MA, 4 SC, 3 SS  = 15; 23        - 15 = 8         other credits
                         * end of 11th: 3 ELA, 3 MA, 3 SC, 2 SS  = 11; 17.25     - 11 = 6.25      other credits
                         * end of 10th: 2 ELA, 2 MA, 2 SC, 1 SS  =  7; 11.5      - 7 = 4.5       other credits
                         * end of 9th:  1 ELA, 1 MA, 1 SC        =  3; 5.75      - 3 = 2.75      other credits
	

                         replace creditsearned = . if creditsattempted == .

		//coreind is perfectly matched to coresubjectcode - so no need to keep it
		

                 collapse (sum) creditsearned, by(id coresubjectcode) 
                         
                         reshape wide creditsearned, i(id) j(coresubjectcode) s

                         rename(creditsearnedLA- creditsearnedSS) (la ma ot sc ss)

                                 foreach v in la ma ot sc ss {
                                         replace `v' = 0 if `v' == .
                                 }
                 isid id

		  merge 1:1 id using data/prep/grades, nogen keep(1 3)

		  
         tempfile courses11
         save `courses11', replace
		 
         ***********************
		     keep if grade_20${histyr1} == 12
								gen _la = la
								gen _ma = ma
								gen _sc = sc
								gen _ss = ss
									replace _la = 4 if la >= 4
									replace _ma = 4 if ma >= 4
									replace _sc = 4 if sc >= 4
									replace _ss = 3 if ss >= 3
                                 gen percCore = (_la/4*.25 + _ma/4*.25 + _sc/4*.25 + _ss/3*.25)
                                 gen percOth = .

										* case if taking typical # of core courses
                                         replace percOth = ot/8 if (la + ma + sc + ss) < = 15
										* case if core course taking is reducing taking other courses
                                         replace percOth = ot/(23 - (la + ma + sc + ss)) if (la + ma + sc + ss) > 15
                                         replace percOth = 2 if percOth < 0 | percOth > 2
											
											drop _la _ma _sc _ss
                                        tempfile crsesGr12
										save `crsesGr12', replace
restore 

preserve
		keep if schoolyear <= 20${histyr1} & grade >= 9 // histyr2 b/c completed course are predictor
	
	* first prep and get proportion of "course-completion" reqs met
                 * core_ind + subject
                         * end of 12th: 4 ELA, 4 MA, 4 SC, 3 SS  = 15; 23        - 15 = 8         other credits
                         * end of 11th: 3 ELA, 3 MA, 3 SC, 2 SS  = 11; 17.25     - 11 = 6.25      other credits
                         * end of 10th: 2 ELA, 2 MA, 2 SC, 1 SS  =  7; 11.5      - 7 = 4.5       other credits
                         * end of 9th:  1 ELA, 1 MA, 1 SC        =  3; 5.75      - 3 = 2.75      other credits
	

                         replace creditsearned = . if creditsattempted == .

		//coreind is perfectly matched to coresubjectcode - so no need to keep it
		

                 collapse (sum) creditsearned, by(id coresubjectcode) 
                         
                         reshape wide creditsearned, i(id) j(coresubjectcode) s

                         rename(creditsearnedLA- creditsearnedSS) (la ma ot sc ss)

                                 foreach v in la ma ot sc ss {
                                         replace `v' = 0 if `v' == .
                                 }
                 isid id

		  merge 1:1 id using data/prep/grades, nogen keep(1 3)

		  
         tempfile courses11
         save `courses11', replace

										use `courses11', clear
										
		***********************		
                 keep if grade_20${histyr1} == 11
								gen _la = la
								gen _ma = ma
								gen _sc = sc
								gen _ss = ss
									replace _la = 3 if la >= 3
									replace _ma = 3 if ma >= 3
									replace _sc = 3 if sc >= 3
									replace _ss = 2 if ss >= 2
                                 gen percCore = (_la/3*.25 + _ma/3*.25 + _sc/3*.25 + _ss/2*.25)
                                 gen percOth = .

										* case if taking typical # of core courses
                                         replace percOth = ot/6.25 if (la + ma + sc + ss) < = 11
										* case if core course taking is reducing taking other courses
                                         replace percOth = ot/(17.25 - (la + ma + sc + ss)) if (la + ma + sc + ss) > 11
                                         replace percOth = 2 if percOth < 0 | percOth > 2
											
											drop _la _ma _sc _ss
                                        tempfile crsesGr11
										save `crsesGr11', replace

										use `courses11', clear
         ***********************
                 keep if grade_20${histyr1} == 10
								gen _la = la
								gen _ma = ma
								gen _sc = sc
								gen _ss = ss
									replace _la = 2 if la >= 2
									replace _ma = 2 if ma >= 2
									replace _sc = 2 if sc >= 2
									replace _ss = 1 if ss >= 1
                                 gen percCore = (_la/2*.25 + _ma/2*.25 + _sc/2*.25 + _ss/1*.25)
								 gen percOth = .

										* case if taking typical # of core courses
                                         replace percOth = ot/4.5 if (la + ma + sc + ss) < = 7
										* case if core course taking is reducing taking other courses
                                         replace percOth = ot/(11.5 - (la + ma + sc + ss)) if (la + ma + sc + ss) > 7
                                         replace percOth = 2 if percOth < 0 | percOth > 2
											
											drop _la _ma _sc _ss		 
                                         tempfile crsesGr10
                                         save `crsesGr10', replace
										 
                                         use `courses11', clear
         ***********************
                 keep if grade_20${histyr1} == 9
								gen _la = la
								gen _ma = ma
								gen _sc = sc

									replace _la = 1 if la >= 1
									replace _ma = 1 if ma >= 1
									replace _sc = 1 if sc >= 1

                                 gen percCore = (_la/1*.33333 + _ma/1*.33334 + _sc/1*.33333)
								gen percOth = .

										* case if taking typical # of core courses
                                         replace percOth = (ot + ss)/3 if (la + ma + sc) < = 3
                                         replace percOth = (ot + ss)/(5.75 - (la + ma + sc)) if (la + ma + sc) > 3
                                         replace percOth = 2 if percOth < 0 | percOth > 2
										 
											drop _la _ma _sc
                                         tempfile crsesGr9
                                         save `crsesGr9', replace
						 append using `crsesGr10'
						 append using `crsesGr11'
						 append using `crsesGr12'
						 save data/prep/coursesTowardGrad, replace

restore

	

		destring mark, replace force
		drop if mark == .
		
		tempfile crs
		save `crs', replace

		local gpaYr = 20${evalyr}
		local gpaYrHist = `gpaYr' - 1

		* LA, Math, and core GPAs

		forvalues y = `gpaYr'(-1)`gpaYrHist' {
				local yminus = `y' - 1
			
					if (`y' == `gpaYr') { //not filter on 12th b/c want these data for kids by cohort not enrolled grade
					preserve
							keep if calendaryear == `yminus' & calendarmonth > 7 //only use first semester
							tempfile crsSeniors
							save `crsSeniors', replace
							
						foreach s in lacourse macourse core {
							keep id schoolyear mark creditsattempted `s'ind //wanted low marks to weigh in proportionately
							keep if `s'ind == 1
							gen wtd_mrk = mark*creditsattempted
							tab wtd_mrk,m
							
							collapse (sum) att_cr=creditsattempted wtd_mrk, by(id)
								isid id
									gen `s'1SemGPA`yminus'`y' = wtd_mrk/att_cr
									hist(`s'1SemGPA`yminus'`y')
									
											drop att_cr wtd_mrk
											count
												save data\prep\\`s'1SemGPA`yminus'`y'_hs, replace
												
												use `crsSeniors', clear
						} //END FOREACH s
					restore
							
							keep if schoolyear <= `y' //avoids courses beyond histyr or evalyr, respectively
					} //END IF gpaYr
			
			foreach s in lacourse macourse core {
							keep id schoolyear grade mark creditsattempted `s'ind //wanted low marks to weigh in proportionately
							keep if `s'ind == 1
							
							gen wtd_mrk = mark*creditsattempted
							tab wtd_mrk,m
					
						keep if schoolyear <= 20${evalyr} & grade >= 5 // gpa is predictor and outcome
				
					if `y' == `gpaYrHist' {

						preserve
								keep if schoolyear == `y' //no grade distinction but only credits accrued on year earlier
															//grade comes from demography to give meaning to these credits
								drop schoolyear
								collapse (sum) att_cr=creditsattempted wtd_mrk, by(id)
								isid id
									gen `s'CumulGPA`yminus'`y' = wtd_mrk/att_cr
									hist(`s'CumulGPA`yminus'`y')
									
											drop att_cr wtd_mrk
											count
												save data\prep\\`s'CumulGPA`yminus'`y', replace

						restore
					  } //END IF == gpaYrHist
							
						keep if schoolyear == `gpaYr' //after extracting historical data, pare down to evalyr

								collapse (sum) att_cr=creditsattempted wtd_mrk, by(id)
								isid id
									gen `s'AnnualGPA`yminus'`y' = wtd_mrk/att_cr
									hist(`s'AnnualGPA`yminus'`y')
									
											drop att_cr wtd_mrk
											count
												save data\prep\\`s'AnnualGPA`yminus'`y', replace
						
		use `crs', clear
		
			} //END FOREACH s
		} //END FORVALUES y
		
******************
* promotion data
******************

use data/orig/promotion_hs, clear
reshape wide endyear_grade startyear_grade repeatinggradeind, i(id) j(schoolyear)
keep id startyear_grade20${evalyr}  startyear_grade20${nxtyr}
* don't need KK or PK
	destring startyear_grade20${evalyr} startyear_grade20${nxtyr}, replace force
	drop if startyear_grade20${evalyr} == . & startyear_grade20${nxtyr} == .

save data/prep/promotion_hs, replace


log close

}

if $merge_all==1 {

log using code/logfiles/merge_all.smcl, replace

* MODEL DATASET
****************

use data/orig/histyr_hs, clear
	drop ageseptember1st //need age Sept variable from evalyr (here 2011-12)
	merge 1:1 id using data/orig/evalyr_hs, nogen keep(1 3)
	merge 1:1 id using data/prep/discipline_enroll_${histyr1}_metric_final_hs, nogen keep (1 3)
	merge 1:1 id using data/prep/discipline_enroll_${evalyr}_metric_final_hs, nogen keep (1 3)
	merge 1:1 id using data/prep/crct_final_hs, nogen keep (1 3)
	merge 1:1 id using data/prep/mobility${histyr2}${histyr1}_hs, nogen keep (1 3)
	merge 1:1 id using data/prep/yearstograd, nogen keep(1 3)
	
	
	//2011-12 GPA - 1Sem is outcome for 11th to 12th analyses
	merge 1:1 id using data/prep/core1SemGPA20${histyr1}20${evalyr}_hs, nogen keep(1 3)
		merge 1:1 id using data/prep/lacourse1SemGPA20${histyr1}20${evalyr}_hs, nogen keep(1 3)
			merge 1:1 id using data/prep/macourse1SemGPA20${histyr1}20${evalyr}_hs, nogen keep(1 3)
			
	//2011-12 GPA - Annual is 2011-12 model output
	merge 1:1 id using data/prep/coreAnnualGPA20${histyr1}20${evalyr}, nogen keep(1 3)
		merge 1:1 id using data/prep/lacourseAnnualGPA20${histyr1}20${evalyr}, nogen keep(1 3)
			merge 1:1 id using data/prep/macourseAnnualGPA20${histyr1}20${evalyr}, nogen keep(1 3)
			
	//2010-11 GPA - Cumul is 2011-12 model input
	merge 1:1 id using data/prep/coreCumulGPA20${histyr2}20${histyr1}, nogen keep(1 3)
		merge 1:1 id using data/prep/lacourseCumulGPA20${histyr2}20${histyr1}, nogen keep(1 3)
			merge 1:1 id using data/prep/macourseCumulGPA20${histyr2}20${histyr1}, nogen keep(1 3)

	merge 1:1 id using data/prep/sei, nogen keep(1 3)
	merge 1:1 id using data/prep/coursesTowardGrad, nogen keep(1 3)
	merge 1:1 id using data/prep/sat${histyr4}${evalyr}, nogen keep(1 3)
	merge 1:1 id using data/prep/act${histyr4}${evalyr}, nogen keep(1 3)
	merge 1:1 id using data/prep/eoct${histyr2}${evalyr}, nogen keep(1 3)

	merge 1:1 id using data/prep/promotion_hs, nogen keep(1 3)
	merge 1:1 id using data/orig/fay_evalyr_hs, nogen keep(1 3)
	merge 1:1 id using data/prep/failcore_rigorInd, nogen keep(1 3)
	merge 1:1 id using data/prep/census, nogen keep(1 3)
	merge 1:1 id using data/prep/psat${histyr2}${histyr1}, nogen keep(1 3)
	merge 1:1 id using data/prep/grad, nogen keep(1 3)
		replace grad = 0 if grad == .
	merge 1:1 id using data/prep/retgft04, nogen keep(1 3)
	
	* summarize prior crct score by school
	preserve
		collapse (mean) schl_LAcrct = ss_totLA schl_MAcrct = ss_totMA ///
						schl_RDcrct = ss_totRD schl_SCcrct = ss_totSC, ///
						by(loc_${histyr2}${histyr1})
					save data/prep/schl_crct, replace
	restore
	
			//add school-level variables
		merge m:1 loc using data/prep/schl_sped, nogen keep(1 3)
		merge m:1 loc using data/prep/schl_esol, nogen keep(1 3)
		merge m:1 loc using data/prep/schl_enr, nogen keep(1 3)
		merge m:1 loc using data/prep/schl_frl, nogen keep(1 3)
		merge m:1 loc using data/prep/schl_att, nogen keep(1 3)
		merge m:1 loc using data/prep/schl_crct, nogen keep(1 3)
		merge m:1 loc using data/prep/schl_sei, nogen keep(1 3)

		* make more nuanced SPED categories
		gen spedCatMin_H1 = inlist(specialeducationprimaryarea_${histyr2}${histyr1}, ///
					"Blind", "Deaf", "Deaf and Blind", "Hearing Impairment", ///
					"Orthopedic Impairment", "Other Health Impairment", ///
					"Speech/Language Impairment", "Visual Impairment")
		gen spedCatMod_H1 = inlist(specialeducationprimaryarea_${histyr2}${histyr1}, ///					
					"Autism", "Emotional/Behavioral Disorder", ///
					"Mild Intellectual Disability", "Specific Learning Disability")
		gen spedCatSev_H1 = inlist(specialeducationprimaryarea_${histyr2}${histyr1}, ///
					"Moderate Intellectual Disability", "Profound Intellectual Disability", ///
					"Severe Intellectual Disability", "Significant Development Delay", ///
					"Traumatic Brain Injury")
		
						
	save data/prep/hs_model, replace

	use data/prep/hs_model, clear

	tempfile modeling
	save `modeling', replace
	
	* look at 83 in 2011 in distribution by grade
	*hist core_pseudo_gpa1011, by( grade_2011) addplot(pci 0 83 .05 83)

	
	forval v =8/12 { //8/11 originally
	local up = `v' + 1
	local down = `v' - 1
	
	if (`v' == 12) {
	keep if grade_20${histyr1} == `v'
	}
	
	
	* historic year grades of 9-11
	if inlist(`v',9, 10, 11) {
	keep if grade_20${histyr1} == `v' & daysenrolled_${histyr1}${evalyr} !=.
	
	* only keep kids who where enrolled for 65% of school year
	}

	* historic year grades < 9
	if (`v' < 9) {
	keep if grade_${histyr2}${histyr1} == `v'
		*drop grade_20${histyr1}
	}
	gen ontimeGrad = yearstograd == 4 & yearstograd != . & grad == 1

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
			grade_${histyr1}${evalyr} specialeducation
		
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

	if (`v' == 12) {				
	preserve
		outsheet using data/prep/`v'th`up'th_model_only.csv, c replace
	restore
	}
		
	if (`v' == 11) {				
	preserve
		drop if startyear_grade_E ==.
		keep if scoreMA != . | scoreVE != . | scoreaEN != . | scoreaMA != . | scoreaRD !=.
		outsheet using data/prep/ACT_SAT_GPA_equating.csv, c replace
	restore

	preserve
		drop if startyear_grade_E ==. | fay_year_E != "Y"
		outsheet using data/prep/`v'th`up'th_model_only.csv, c replace
	restore

	} 
	else if (`v' < 11) {	
		
		drop if startyear_grade_E ==. | startyear_grade_N == . | ///
		fay_year_E != "Y"	//percentEnrolledDays_1011 < .65 ??regardless of how received them??
		outsheet using data/prep/`v'th`up'th_model_only.csv, c replace

	}

		use `modeling', clear

    }
  
 }

	
if $scoring==1 {
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
	


