


foreach yr in 09 10 11 {
local preyr = 20`yr' - 1
cd "S:\Superintendent\Private\Strategy & Performance\ResearchEvaluation\RBES\School Accountability Rpts\Issued `preyr'-`yr'\Student"

*ENR021
import excel using "ENR021 Student Attendance Report_jja.xls", clear first
	gen SchoolYear = `preyr'
	rename(Key Attendance) (SchoolNumb AverageDailyAttendance)
	
	save "data\prep\ENR021_`preyr'", replace
	
*FRPL
import excel using "fte_pack_frl001_public_jja.xls", clear first
	gen SchoolYear = `preyr'
	rename(Key FRL_Pct) (SchoolNumb PercentEligibleFreeOrReduced)
	
	save "data\prep\FRPL_`preyr'", replace


*FT002 (race/ethnicity)
import excel using "667_ALLSCH_jja.xls", clear first sheet("Sheet1")
	gen SchoolYear = `preyr'
	rename(Key W_Pct Enrollment_N) (SchoolNumb schl_percWht_H1 TotalEnrollment)
	
	save "data\prep\FT002_`preyr'", replace


*FT003 (ESOL)
import excel using "ESOL_jja.xls", clear first sheet("Sheet1")
	gen SchoolYear = `preyr'
	rename(Key ESOL_N) (SchoolNumb TotalEnrollment)
	
	save "data\prep\FT003_`preyr'", replace

*FT004 (SPED)
import excel using "SPED_jja.xls", clear first sheet("Sheet1")
	gen SchoolYear = `preyr'
	rename(Key SPED_N) (SchoolNumb TotalEnrollment)
	
	save "data\prep\FT004_`preyr'", replace
}

