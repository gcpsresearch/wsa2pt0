require(RODBC)

# change variable case; df name in quotations to be accepted
case.cols <- function(x) {
  x.df <- get(x)
  colnames(x.df) <- tolower(names(x.df))
  assign(x,x.df, env = .GlobalEnv)
}

yrs <- 2012:2014

ma_ch <- odbcConnect("ODS_Prod_MA", uid = "Research", pwd = "Research")

    dual.enr <- sqlQuery(ma_ch, paste0(

"
SELECT distinct t1.[SCHOOL_YEAR]
      ,t2.[TITLE]
      ,t1.[MARK_NUMERIC]
      ,t1.[CREDIT_EARNED]
      ,t1.[PERMNUM]
      ,t1.[SCHOOL]
      ,t1.[SCHOOL_CODE]
      ,t1.[RECORD_TYPE]
      ,t1.[COURSE]
      ,t1.[SECTION_NUMBER]
      ,t1.[TEACHER_ID]
      ,t1.[GIFTED_DEL_MODEL]
      ,t1.[GIFTED_CONTENT_AREA]
      ,t1.[MARKING_PERIOD]
      ,t1.[ALPHA_GRADE]
      ,t1.[ELL_DEL_MODEL]
      ,t1.[CREDIT_RECOVERY]
      ,t1.[CONTENT_COMPLETER]
      ,t1.[CREDIT_IN_LIEU_OF_COURSE]
      ,t1.[ONLINE_COURSE]
      ,t2.[STATUS]
      ,t2.[COURSE]
      ,t2.[LONGTITLE]
      ,t2.[NONACADEM]
      ,t2.[SUBJAREA1]
      ,t2.[SUBJAREA2]
      ,t2.[SUBJAREA3]
      ,t2.[DEPARTMENT]
      ,t2.[CREDVALUE]
      ,t2.[PROMCRIT]
  FROM [GSDR].[GEMS].[SDRD_HIST] t1 LEFT JOIN [GSDR].[GEMS].[SASI_ACRS] t2 ON 
  	t1.COURSE = t2.COURSE
  WHERE t1.SCHOOL_YEAR in ", paste("(", paste(yrs, collapse=","),")"), " and
		t2.LONGTITLE LIKE 'PS %'
  ORDER BY t1.COURSE
        "))

case.cols("dual.enr")
table(dual.enr$school_year)
table(dual.enr$title)
