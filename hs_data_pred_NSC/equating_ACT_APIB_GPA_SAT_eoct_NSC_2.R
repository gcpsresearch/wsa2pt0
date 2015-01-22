# equating/linking HSGPA and Econ EOCT to "college-ready" SAT and ACT scores
#   will be used to examine thresholds from CollegeBoard and ACT research

## answers the question: For graduates, what HSGPA, Econ EOCT, SAT, and ACT values
##    give students an 80% chance of enrolling in college, and 2-year persistence in college

#   created on    2014.03.21 by James Appleton
#   last updated  2015.01.20 by James Appleton

############
# Setup
############

require(ggplot2)
require(grid)
require(RODBC)
require(plyr)
require(foreign)
require(reshape2)
require(catspec)


# registerDoParallel(4, cores = 4)
# getDoParWorkers()

rm(list=ls())
path <- readLines("c:\\current_path.txt")

# set directories
setwd (paste(path,                        
             "\\Research Projects\\RaisngAchClsngGap",sep=""))
maindir <- paste(path,                        
             "\\Research Projects\\RaisngAchClsngGap",sep="")
dir ()

# functions
vplayout <- function(x, y) {
  viewport(layout.pos.row = x, layout.pos.col = y)
}

# convert factor variable to numeric
factorconvert <- function(f){as.numeric (levels (f))[f]}

# trim extra preceding and following characters
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# change variable case; df name in quotations to be accepted
case.cols <- function(x) {
  x.df <- get(x)
  colnames(x.df) <- tolower(names(x.df))
  assign(x,x.df, env = .GlobalEnv)
}

# write a simple function to add footnote
makeFootnote <- function(footnoteText =
                         format(Sys.time(), "%d %b %Y"),
                         size = .7, color = grey(.5))
{
  require(grid)
  pushViewport(viewport())
  grid.text(label = footnoteText ,
    x = unit(20,"mm"),
    y = unit(1, "mm"),
    just = c("left", "bottom"),
    gp = gpar(cex = size, col = color))
 popViewport()
}

# set years for graduation data

cohortYear_shrt <- c(2011, 2012, 2013) # b/c 2013 doesn't have 4 semesters of time yet

yrs <- length(cohortYear_shrt)  # number of years set below

startYear1       <- "2007-08" # for 2011 grads
startYear_shrt1  <- "2008"

startYear2       <- "2008-09" # for 2012 grads
startYear_shrt2  <- "2009"

startYear3       <- "2009-10" # for 2013 grads
startYear_shrt3  <- "2010"

startYear <- c(startYear1, startYear2, startYear3)
startYear_shrt <- c(startYear_shrt1, startYear_shrt2, startYear_shrt3)

###################################################
### Load the NSC data
###################################################

nsc <- read.csv(paste0(path, "\\Research Projects\\NSC Student Tracker\\", 
                       "NSC StudentTracker_2014.10_2014Graduates\\received\\", 
                       "1302550hs_10001139-28963-DETAIL-EFFDT-20141126-RUNDT-20141204.csv"),
                sep = ",", header = TRUE)

  nsc <- case.cols("nsc")

  # change NA enrollment begin and end dates so can't count within enrollment periods
  nsc[is.na(nsc$enrollment_begin), "enrollment_begin" ] <- 0
  nsc[is.na(nsc$enrollment_end), "enrollment_end" ] <- 0

  # keep students graduating in cohort years and assign cohort
  nsc$cohort <- NA

for (i in 1:yrs) {
  
    nsc[nsc$high_school_grad_date > (cohortYear_shrt[i] - 1)*10000 + 0801 & 
        nsc$high_school_grad_date < cohortYear_shrt[i]*10000 + 0731, dim(nsc)[2]] <- cohortYear_shrt[i]
}



# (F)ull-time, (H)alf-time, (L)ess than half-time, (Q) 3/4 time, 
#   (A) Leave of absence, (W)ithdrawn, (D)eceased
#   from: http://www.studentclearinghouse.org/colleges/files/ST_DetailReportGuide.pdf

  # create gcps id
  nsc[,1] <- as.character(nsc[,1])
  nsc$id <- as.numeric(substr(nsc[,1], 1, nchar(nsc[,1]) - 1))

      nsc <- nsc[!is.na(nsc$cohort), ]

  # create immed.transition and persist.enroll variables

  nsc$i.t <- FALSE
  nsc$p.e1 <- FALSE
#   nsc$p.e2 <- FALSE
#   nsc$p.e3 <- FALSE

  

  for (i in 1:yrs) {
    
    nsc[nsc$i.t == FALSE, "i.t"] <- nsc[nsc$i.t == FALSE, "enrollment_begin"] < cohortYear_shrt[i]*10000 + 1101 & 
                   nsc[nsc$i.t == FALSE, "enrollment_end"] > cohortYear_shrt[i]*10000 + 915 & 
                   nsc[nsc$i.t == FALSE, "cohort"] == cohortYear_shrt[i] #& 
                   #nsc[nsc$i.t == FALSE, "enrollment_status"] == "F"
    
#     nsc[nsc$i.t == FALSE, "i.t"] <- as.numeric(nsc[nsc$i.t == FALSE, "enrollment_begin"]) < cohortYear_shrt[i]*10000 + 1231 & 
#                    as.numeric(nsc[nsc$i.t == FALSE, "enrollment_begin"]) > cohortYear_shrt[i]*10000 + 0801
#                    nsc[nsc$i.t == FALSE, "cohort"] == cohortYear_shrt[i] 
#     
      it <- ddply(nsc[, c("id", "i.t")], "id", summarise, 
                  immed.t = sum(i.t))
      it$i.t <- it$immed.t > 0
        nsc <- nsc[, -(which(names(nsc) %in% c("i.t")))]
    
        nsc <- merge(nsc, it[, c(1, 3)], by.x = "id", by.y = "id", all.x = TRUE)
    
    nsc[nsc$p.e1 == FALSE, "p.e1"] <- nsc[nsc$p.e1 == FALSE, "i.t"] == TRUE & 
                    nsc[nsc$p.e1 == FALSE, "enrollment_begin"] < (cohortYear_shrt[i] + 1)*10000 + 501 & 
                    nsc[nsc$p.e1 == FALSE, "enrollment_end"] > (cohortYear_shrt[i] + 1)*10000 + 301 & 
                    nsc[nsc$p.e1 == FALSE, "cohort"] == cohortYear_shrt[i] & 
                    nsc[nsc$p.e1 == FALSE, "enrollment_status"] %in% c("F", "Q")

# b/c p.e. now only 1 academic year

#     nsc[nsc$p.e2 == FALSE, "p.e2"] <- nsc[nsc$p.e2 == FALSE, "i.t"] == TRUE &     
#                     nsc[nsc$p.e2 == FALSE, "enrollment_begin"] < (cohortYear_shrt[i] + 1)*10000 + 1101 & 
#                     nsc[nsc$p.e2 == FALSE, "enrollment_end"] > (cohortYear_shrt[i] + 1)*10000 + 915 & 
#                     nsc[nsc$p.e2 == FALSE, "cohort"] == cohortYear_shrt[i] & 
#                     nsc[nsc$p.e2 == FALSE, "enrollment_status"] %in% c("F", "Q")
#  
#     nsc[nsc$p.e3 == FALSE, "p.e3"] <- nsc[nsc$p.e3 == FALSE, "i.t"] == TRUE & 
#                     nsc[nsc$p.e3 == FALSE, "enrollment_begin"] < (cohortYear_shrt[i] + 2)*10000 + 501 & 
#                     nsc[nsc$p.e3 == FALSE, "enrollment_end"] > (cohortYear_shrt[i] + 2)*10000 + 301 & 
#                     nsc[nsc$p.e3 == FALSE, "cohort"] == cohortYear_shrt[i] & 
#                     nsc[nsc$p.e3 == FALSE, "enrollment_status"] %in% c("F", "Q")

  }
    
      mrg <- ddply(nsc[, c("id", "p.e1", 
                           #"p.e2", "p.e3", 
                           "i.t")], "id", summarise, 
                   pe1 = sum(p.e1), 
#                    pe2 = sum(p.e2), 
#                    pe3 = sum(p.e3), 
                   i.t = sum(i.t))
    
        mrg$p.e <- mrg$pe1 == 1 #& mrg$pe2 == 1 & mrg$pe3 == 1
    
        nsc <- merge(nsc, mrg[, c("id", "i.t", "p.e")], by.x = "id", by.y = "id", all.x = TRUE)
                  colnames(nsc)[which(names(nsc) == "i.t.x")] <- "i.t"

        nsc <- unique(nsc[, c("id", "first_name", "middle_name", "last_name", 
                              "high_school_grad_date", "cohort", "i.t", "p.e")])

    
  nsc.model <- nsc[, c("id", "i.t", "p.e")]
    
    

##################################
## get ACT data
##################################

ma_ch <- odbcConnect("ODS_Prod_MA", uid = "Research", pwd = "Research")

    act <- sqlQuery(ma_ch, paste0(
      "   SELECT [STUNUMB]
                ,[SCHOOL_YEAR]
                ,[TEST_KEY]
                ,[EXAM_ADMIN_DATE]
                ,[SUBJECT]
                ,[SCALE_SCORE]
          FROM [Assessment].[dbo].[TEST_STU_ACT]
          WHERE SCHOOL_YEAR >= ", cohortYear_shrt[1] - 3, " and ", 
		            "SCHOOL_YEAR <= ", cohortYear_shrt[length(cohortYear_shrt)], " and ", 
                "SCALE_SCORE is not null and
                SCALE_SCORE != 0
        "))

    act <- case.cols("act")
    names(act)[which(names(act) == "stunumb")] <- "id"
    
    # filter down to average scale score by kid

    actStu <- ddply(act[, c("id", "subject", "scale_score")], c("id", "subject"), 
                    summarise, 
                     actSS = mean(scale_score))

        stopifnot(anyDuplicated(actStu[, c("id", "subject")])==0)
        actStu$actSS <- round(actStu$actSS)
          stopifnot(actStu$actSS >= 1 & actStu$actSS <= 36)

    actStu <- dcast(actStu, id ~ subject)

odbcClose(ma_ch)

##################################
## get AP/IB data
##################################

# AP Exam of 3+ or A or B average in IB courses with >= 1.0 credits earned

ma_ch <- odbcConnect("ODS_Prod_MA", uid = "Research", pwd = "Research")


# ib courses
    ib.link <- sqlQuery(ma_ch, paste0("
          SELECT  [COURSE]
                 ,[LONGTITLE]
            FROM [GSDR].[GEMS].[SASI_ACRS]
            WHERE FTE_TYPE = 'B'
        "))

    ib.link <- case.cols("ib.link")


    gsdr.ib <- sqlQuery(ma_ch, paste0("
        SELECT [PERMNUM]
              ,[SCHOOL_YEAR]
              ,[COURSE]
              ,[CREDIT_EARNED]
              ,[MARK_NUMERIC]
        FROM   [GSDR].[GEMS].[SDRD_HIST]
        WHERE  [SCHOOL_YEAR] >=", cohortYear_shrt[1] - 3, " and 
  	           [SCHOOL_YEAR] <=", cohortYear_shrt[length(cohortYear_shrt)], " and 
		           cast(CREDIT_EARNED as int) > 0
        "))
      gsdr.ib <- case.cols("gsdr.ib")
    gsdr.ib <- merge(gsdr.ib, ib.link, by.x = "course", by.y = "course")

      names(gsdr.ib)[which(names(gsdr.ib) == "permnum")] <- "id"
      gsdr.ib$mark_numeric <- as.character(gsdr.ib$mark_numeric)
      gsdr.ib$mark_numeric <- as.numeric(gsdr.ib$mark_numeric)

      gsdr.ib.agg <- ddply(gsdr.ib[, c("id", "mark_numeric", "credit_earned")], "id", summarise, 
                  wtdMk  = sum(mark_numeric*credit_earned*.001),
                  creds = sum(credit_earned*.001))

      gsdr.ib.agg$mark <- gsdr.ib.agg$wtdMk/gsdr.ib.agg$creds
      gsdr.ib.agg$ib.t2 <- gsdr.ib.agg$mark >= 80 & gsdr.ib.agg$creds >= 1


    ib <- sqlQuery(ma_ch, paste0("
            SELECT [permnum]
                  ,[grade]
                  ,[school_year]
                  ,[term]
                  ,[course]
                  ,[course_name]
                  ,[mark]
                  ,[credit_attempted]
                  ,[credit_earned]
                  ,[course_identifier]
              FROM [ResearchAndEvaluation].[dbo].[APCourses]
              WHERE school_year >=", cohortYear_shrt[1] - 3, " and
              	    school_year <=", cohortYear_shrt[length(cohortYear_shrt)], " and 
                    course_identifier = 'I' and 
                    credit_earned > 0
        "))

    ib <- case.cols("ib")
    names(ib)[which(names(ib) == "permnum")] <- "id"
      ib$mark <- as.character(ib$mark)
      ib[ib$mark == "AUD", "mark"] <- "0"
      ib$mark <- as.numeric(ib$mark)

      ib.agg <- ddply(ib[, c("id", "mark", "credit_earned")], "id", summarise, 
                  wtdMk  = sum(mark*credit_earned),
                  creds = sum(credit_earned))

      ib.agg$mark <- ib.agg$wtdMk/ib.agg$creds
      ib.agg$ib.t <- ib.agg$mark >= 80 & ib.agg$creds >= 1
      

# ap exams; uses "- 4" to get back to 8th grade for earliest cohort
    ap <- sqlQuery(ma_ch, paste0("

            SELECT [STUNUMB]
                  ,[TEST_KEY]
                  ,[GRADE]
                  ,[EXAM_ADMIN_DATE]
                  ,[SCORE]
                  ,[SCHOOL_YEAR]
              FROM [Assessment].[dbo].[TEST_STU_APP]
              WHERE SCHOOL_YEAR >=", cohortYear_shrt[1] - 4, " and 
                    SCHOOL_YEAR <=", cohortYear_shrt[length(cohortYear_shrt)], " and 
                    SCORE is not null and 
                    SCORE != 0
        "))

    ap <- case.cols("ap")
    names(ap)[which(names(ap) == "stunumb")] <- "id"

      ap.agg <- ddply(ap[, c("id", "score")], "id", summarise, 
                  mxScr  = max(score))

      ap.agg$ap.t <- ap.agg$mxScr >= 3

odbcClose(ma_ch)

##################################
## get EOCT econ data
##################################

ma_ch <- odbcConnect("ODS_Prod_MA", uid = "Research", pwd = "Research")

    econECT <- sqlQuery(ma_ch, paste0(
      "   SELECT [SCHOOL_YR]
              ,[LOC]
              ,[EXAM_ADMIN_DATE]
              ,[GRADE]
              ,[STUNUMB]
              ,[SUBJECT]
              ,[TOTAL_SCALE_SCORE]
          FROM [Assessment].[dbo].[TEST_STU_ECT]
          WHERE SUBJECT = 'ECO' and 
                SCHOOL_YR in (", paste(cohortYear_shrt, sep = "", collapse = ", "), 
                ") and ", 
                "TOTAL_SCALE_SCORE is not null and
                TOTAL_SCALE_SCORE != 0
        "))
    
    #close(ma_ch)

    econECT <- case.cols("econECT")

    # filter down to average scale score by kid

    econECT <- ddply(econECT[, c("stunumb", "total_scale_score")], "stunumb", summarise, 
                     econSS = mean(total_scale_score))

        stopifnot(anyDuplicated(econECT$stunumb)==0)
        econECT$econSS <- round(econECT$econSS)
          stopifnot(econECT$econSS >= 200 & econECT$econSS <= 650)

odbcClose(ma_ch)

########################
# linking file for GTID
########################

ma_ch <- odbcConnect("ODS_Prod_MA", uid = "Research", pwd = "Research")

    id <- sqlQuery(ma_ch, paste0(
      
    "      SELECT [STUDENT_ID]
                ,[PERMNUM]
            FROM [GSDR].[GEMS].[SDRC_HIST]
    "))

    id <- case.cols("id")
      colnames(id) <- c("gtid", "id")
    
    # keep only one set of these // only 2 dups - better vetting method?
    id <- id[!duplicated(id$gtid), ]

    close(ma_ch)

#######################################
# load the cohorts and graduation data
#######################################

for (i in 1:3) { # b/c class of 2011 has only ~ 2K for 4-year intersect ALL school.ids

fileLoc <- paste0(path,
                  "\\RBES\\Graduation Rate\\Cohort Graduation Rate Data\\ClassOfSY", cohortYear_shrt[i])

# this portion removes excess quotation marks
d <- readLines(con = paste0(fileLoc, "\\data\\orig\\667Grad_candidate_list.csv"))
d <- gsub("\"", "", d)
      write.table(d, file = "d.txt", quote = FALSE, row.names = FALSE, col.names 
                  = FALSE)
      df <- read.csv(file = "d.txt",  header = TRUE)

# df <- read.csv(paste0("..\\RaisngAchClsngGap\\data\\prep\\DOECohortData_", startYear[i],  
#                         "_jja.csv"), sep = ",", header = TRUE)

df <- case.cols("df")
  names(df)[40] <- "update.diploma.type"

      df <- df[df$grad.rate.type == 4 & df$school.id == "ALL", ]
      df$grad <- df$update.diploma.type %in% c("G", "C", "B", "V")
      df$cohort <- cohortYear_shrt[i]

  df <- merge(df, id, by.x = "student.id", by.y = "gtid", all.x = TRUE)

assign(paste0("cohort.", cohortYear_shrt[i]), df)

}


cohort.list <- as.list(rbind(list=ls(pattern = "cohort.2")))
cohorts <- get(paste0(cohort.list[1]))

for (i in 2:length(cohort.list)) {
cohorts <- rbind(cohorts, get(paste0(cohort.list[i])))
}

# have a few state errors to fix; we will keep best case (e.g., grad over non-grad)
                                  # after best case will take most recent
if (anyDuplicated(cohorts$id) != 0) {
  
  cohorts <- cohorts[order(cohorts$id, -cohorts$grad, -cohorts$entry.school.year), ]
  
    cohorts <- cohorts[!duplicated(cohorts$id), ]
}

rm(list=ls(pattern = "cohort.2"))
gc()

# updated.withdrawal.reason that is transfer out
transfers <- cbind(c("Transfer: Private School", "Transfer: Homeschool", "Transfer: Other System", 
                     "Transfer: Another State", "Transfer: Out of Country", "Transfer: DJJ", 
                     "Death", "SB10 Transfer to State Schools", "SB10 Transfer to Private School", 
                     "SB10 Transfer to Public School"), 
                   c("K", "H", "T", "X", "J", "4", "D", "Y", "Z", "1"))

cohorts <- cohorts[!(cohorts$updated.withdrawal.reason %in% transfers[, 2]), ]

##################################
## get GPA data
##################################

ma_ch <- odbcConnect("ODS_Prod_MA", uid = "Research", pwd = "Research")

cohortYear_shrt2 <- c(cohortYear_shrt - 6, cohortYear_shrt - 3, cohortYear_shrt)[-(1:2)]

for (i in 1:length(cohortYear_shrt2)) {

gpa <- sqlQuery(ma_ch, paste0(
      "SELECT * 
  		FROM	[Predictive_Analytics].[PAVIEW2].[v_Student_Course_History_DETAIL] 
			WHERE 	SchoolYear = ", cohortYear_shrt2[i], " and 
					Grade in ('08', '09', '10', '11', '12')
      "))

    gpa <- case.cols("gpa")

    gpa <- merge(gpa, cohorts, by.x = "permnum", by.y = "id")
    
    f <- sapply(gpa, is.factor)
    gpa[f] <- lapply(gpa[f], as.character)

    #gpa[grepl("Science", gpa$coresubjectcode), "coreind"] <- 1
  
  assign(paste0("gpa.", cohortYear_shrt2[i]), gpa)

}

df <- get(paste0("gpa.", cohortYear_shrt2[1]))
  for (j in 2:length(cohortYear_shrt2)) {
    df2 <- get(paste0("gpa.", cohortYear_shrt2[j]))
    df <- rbind(df, df2)
  }

gpa <- df

rm(df, df2, list = ls(pattern = "gpa."))

#format GPA

      # merge in cohort year to know latest year of data to accept
        # we will take that year down to 4 years prior
          # expected grad year (cohort year), exp 11th (CY - 1), exp 10th (CY - 2), 
          # exp 9th (CY - 3), exp 8th (CY - 4)

      cohort.range <- cohorts[, c("cohort", "id")]
        cohort.range$min <- cohort.range$cohort - 4
        names(cohort.range)[which(names(cohort.range) == "cohort")] <- "max"
      
      # generate weighted core GPA
        gpa.core <- gpa[gpa$coreind == 1, ]
          gpa.core <- merge(gpa.core, cohort.range, by.x = "permnum", by.y = "id")
          gpa.core <- gpa.core[gpa.core$schoolyear >= gpa.core$min & 
                      gpa.core$schoolyear <= gpa.core$max, ]

      # aggregate
        gpa.agg <- ddply(gpa.core[, c("permnum", "creditsattempted", 
                                            "creditweightedmark", "coresubjectcode")], 
                          c("permnum", "coresubjectcode"), summarise, 
                          N = length(permnum),
                          ca = sum(creditsattempted),
                          cw = sum(creditweightedmark), .parallel =FALSE)


          gpa.aggm <- melt(gpa.agg[, c(1:2, 4:5)], id.vars = c(1:2))
          gpa.aggr <- dcast(gpa.aggm, permnum ~ coresubjectcode + variable)


            
            gpa.aggr$cu.gpa.la <- round(gpa.aggr[, "LA_cw"] / gpa.aggr[, "LA_ca"], 1)
            gpa.aggr$cu.gpa.ma <- round(gpa.aggr[, "MA_cw"] / gpa.aggr[, "MA_ca"], 1)
            gpa.aggr$cu.gpa.sc <- round(gpa.aggr[, "SC_cw"] / gpa.aggr[, "SC_ca"], 1)
            gpa.aggr$cu.gpa.ss <- round(gpa.aggr[, "SS_cw"] / gpa.aggr[, "SS_ca"], 1)
            gpa.aggr$cu.gpa.core <- round(apply(gpa.aggr[, c("LA_cw", "MA_cw", "SC_cw", "SS_cw")], 
                                                   1, function(x) sum(x, na.rm = TRUE)) / 
                                             apply(gpa.aggr[, c("LA_ca", "MA_ca", "SC_ca", "SS_ca")], 
                                                   1, function(x) sum(x, na.rm = TRUE)), 1)
        
              gpa.aggf <- gpa.aggr[, c("permnum", "cu.gpa.la", "cu.gpa.ma", 
                                         "cu.gpa.sc", "cu.gpa.ss", "cu.gpa.core")]

                gpa.aggf <- gpa.aggf[!is.na(gpa.aggf$cu.gpa.la) | !is.na(gpa.aggf$cu.gpa.ma) |
                                     !is.na(gpa.aggf$cu.gpa.sc) | !is.na(gpa.aggf$cu.gpa.ss), ]

  rm(gpa, gpa.core, gpa.agg, gpa.aggm, gpa.aggr)



# below is 1st semester, 12th grade GPA - just for WSA, not district comparison
      
#       # for 12th grade keep only 1st semester
#         gpa.core.12th <- gpa.core[gpa.core$calendarmonth > 7 & gpa.core$grade == 12, ]
#   
# 
#       # aggregate
#         gc12.agg <- ddply(gpa.core.12th[, c("permnum", "schoolyear", "creditsattempted", 
#                                             "creditweightedmark", "coresubjectcode")], 
#                           c("permnum", "schoolyear", "coresubjectcode"), summarise, 
#                           N = length(permnum),
#                           ca = sum(creditsattempted),
#                           cw = sum(creditweightedmark))
# 
#           gc12.aggm <- melt(gc12.agg[, c(1:3, 5:6)], id.vars = c(1:3))
#           gc12.aggr <- dcast(gc12.aggm, permnum + schoolyear ~ coresubjectcode + variable)
# 
# 
#             
#             gc12.aggr$sem1.gpa.la <- round(gc12.aggr[, "LA_cw"] / gc12.aggr[, "LA_ca"], 1)
#             gc12.aggr$sem1.gpa.ma <- round(gc12.aggr[, "MA_cw"] / gc12.aggr[, "MA_ca"], 1)
#             gc12.aggr$sem1.gpa.sc <- round(gc12.aggr[, "SC_cw"] / gc12.aggr[, "SC_ca"], 1)
#             gc12.aggr$sem1.gpa.ss <- round(gc12.aggr[, "SS_cw"] / gc12.aggr[, "SS_ca"], 1)
#             gc12.aggr$sem1.gpa.core <- round(apply(gc12.aggr[, c("LA_cw", "MA_cw", "SC_cw", "SS_cw")], 
#                                                    1, function(x) sum(x, na.rm = TRUE)) / 
#                                              apply(gc12.aggr[, c("LA_ca", "MA_ca", "SC_ca", "SS_ca")], 
#                                                    1, function(x) sum(x, na.rm = TRUE)), 1)
#         
#               gc12.aggf <- gc12.aggr[, c("permnum", "schoolyear", "sem1.gpa.la", "sem1.gpa.ma", 
#                                          "sem1.gpa.sc", "sem1.gpa.ss", "sem1.gpa.core")]
# 
#   rm(gc12.agg, gc12.aggm, gc12.aggr, list=ls(pattern = "gpa"))
  gc()
  
odbcClose(ma_ch)

##################################
## get SAT data
##################################

ma_ch <- odbcConnect("ODS_Prod_MA", uid = "Research", pwd = "Research")

    sat <- sqlQuery(ma_ch, paste0(
      "   SELECT [STUNUMB]
  						  ,[TEST_KEY]
							  ,[EXAM_ADMIN_DATE]
							  ,[NONSTAND_IND]
							  ,[SUBJECT]
							  ,[SCORE]
						  FROM [Assessment].[dbo].[TEST_STU_SAT]
						  WHERE EXAM_ADMIN_DATE >= ", cohortYear_shrt2[3], "0531 and
                    EXAM_ADMIN_DATE <= ", cohortYear_shrt2[length(cohortYear_shrt2)], "0531 and								
                    NONSTAND_IND = '' and
								    SUBJECT in ('MA', 'VE')
        "))
    

    sat <- case.cols("sat")

    # filter down to average scale score by kid

    sat <- ddply(sat[, c(1, 5:6)], c("stunumb", "subject"), summarise, 
                     satSS = mean(score))

        stopifnot(anyDuplicated(sat[, 1:2])==0)
        sat$satSS <- (round(sat$satSS*.1 + .000001, 0))*10 # to round as usual, but back to nearest 10
          stopifnot(sat$satSS >= 200 & sat$satSS <= 800)

        # restructure
        sat <- dcast(sat, stunumb ~ subject)

        sat <- case.cols("sat")
          colnames(sat)[2:3] <- trim(names(sat)[2:3])

odbcClose(ma_ch)

################################
# make dfs and graph/model
################################

# keep all - think those entering late transferred in; [cohorts$entry.school.year %in% startYear_shrt, ]
cohorts.nsc <- merge(cohorts, nsc.model, by.x = "id", by.y = "id", all.x = TRUE)
cohorts.nsc[is.na(cohorts.nsc$i.t), "i.t"] <- FALSE
cohorts.nsc[is.na(cohorts.nsc$p.e), "p.e"] <- FALSE

#########
# ACT
#########
act.nsc <- merge(cohorts.nsc, actStu, by.x = "id", by.y = "id")#, all.x = TRUE)
#   act.nsc <- merge(act.nsc, sat, by.x = "id", by.y = "stunumb", all.x = TRUE)
#     act.nsc$mi <- apply(act.nsc[, c("CO", "EN", "MA", "RD", "SC", "ma", "ve")], 1, function(x) sum(is.na(x)))
#     act.nsc <- act.nsc[act.nsc$mi < 7, -((dim(act.nsc)[2] - 2):dim(act.nsc)[2])]
    


subj <- as.data.frame(matrix(cbind(c("EN", "MA", "RD", "SC", "CO"), 
                                   c("English", "Mathematics", "Reading", "Science", "Composite"), 
                                   c(17, 18, 19, 19, 20), 
                                   c(24, 23, 23, 23, 24)), 
                                   ncol = 4))
subj[,3:4] <- lapply(subj[, 3:4], function(x) {as.numeric(levels(x))[x]})
s <- sapply(subj, is.factor)
subj[s] <- lapply(subj[s], as.character)
names(subj)[3:4] <- c("it.t", "pe.t")


# CO, EN, MA, RD, SC
####
  act.nsc$cohort2 <- act.nsc$cohort - 3

# reshape for facet wrap by subj & cohort
act.nsc.m <- melt(act.nsc[, c("id", "CO", "EN", "MA", "RD", "SC", "cohort2", "i.t", "p.e")], 
                  id.vars = c("id", "cohort2", "i.t", "p.e"))





# function to aggregate scores at ends w/ less than 100 cases across subjects
t <- as.data.frame(table(act.nsc.m$variable, act.nsc.m$value))

# max less than median; min greater than median for any subject

t$lt100 <- t$Freq < 100
names(t)[1] <- "Subj"
names(t)[2] <- "Var1"
  t$Var1 <- as.numeric(levels(t$Var1))[t$Var1]
  low <- max(t[t$Var1 < median(t$Var1) & t$lt100 == TRUE, "Var1"]) + 1
  high <- min(t[t$Var1 > median(t$Var1) & t$lt100 == TRUE, "Var1"]) - 1

    t$value2 <- t$Var1
    t[t$Var1 <= low, dim(t)[2]] <- low
    t[t$Var1 >= high, dim(t)[2]] <- high

  act.nsc.m <- merge(act.nsc.m, unique(t[, c("Var1", "value2")]), by.x = "value", by.y = "Var1", all.x = TRUE)
  names(act.nsc.m)[6:7] <- c("Subject", "Score")

  act.nsc.m.t <- merge(act.nsc.m, subj[, c(1, 3:4)], by.x = "Subject", by.y = "V1", all.x = TRUE)
    act.nsc.m.t$pe.mt <- ifelse(act.nsc.m.t$Score >= act.nsc.m.t$pe.t & !(is.na(act.nsc.m.t$Score)), "Yes", "No")

  act.nsc.m2 <- ddply(act.nsc.m[, c("cohort2", "Subject", "Score", "i.t")], c("cohort2", "Subject", "Score"), 
                   summarise, 
                   N = length(i.t), 
                   it_avg = mean(i.t))

  act.nsc.m2.p <- ddply(act.nsc.m[, c("cohort2", "Subject", "Score", "p.e")], c("cohort2", "Subject", "Score"), 
                   summarise, 
                   N = length(p.e), 
                   pe_avg = mean(p.e))

  act.nsc.m2.pt <- ddply(act.nsc.m.t[, c("cohort2", "Subject", "pe.mt", "p.e")], c("cohort2", "Subject", "pe.mt"), 
                   summarise, 
                   N = length(p.e), 
                   pe_avg = mean(p.e))
  
    
    # check if every cell, once divided by cohort, is >= 30 cases
    stopifnot(table(act.nsc.m$cohort2, act.nsc.m$Subject, act.nsc.m$Score) >= 30)

# graph


# immediate transition
###

for (i in 1:length(subj[, 1])) {
  
  # create totals for footnote
  N <- ddply(act.nsc.m2[act.nsc.m2$Subject == subj[i, 1] & !is.na(act.nsc.m2$Score), c("cohort2", "N")], "cohort2", summarise, 
             N = sum(N))
  N$tot <- paste(N$cohort2, " = ", N$N, sep = "", collapse = ", ")
  
  

pt <- ggplot(act.nsc.m2[act.nsc.m2$Subject == subj[i, 1] , ], 
             aes(factor(Score), y = round(it_avg*100, 1), label = round(it_avg*100, 0)))
pt <- pt + geom_bar(stat = "identity")
pt <- pt + ggtitle(paste0("Proportion of 9th Grade Cohort with Seamless \nPost-Secondary Transition by ", 
                          "ACT ", subj[i, 2], " Score"))
pt <- pt + xlab(paste0("ACT ", subj[i, 2], " Score\n\n\n"))
pt <- pt + ylab("% with NSC Indicated Seamless Transition")
pt <- pt + scale_y_continuous(breaks = c(seq(0, 100, 20)), limits = c(0, 100))
pt <- pt + geom_text(vjust = 1, color = "white", size = 3)
pt <- pt + geom_hline(aes(yintercept = 80), size = 1, colour = "red", linetype = "dashed")
pt <- pt + geom_vline(aes(xintercept = subj[i, 3] - (low-1)), size = 4, colour = "red", linetype = "solid", alpha = 0.25)
pt <- pt + facet_wrap( ~ cohort2, ncol = 1)
print(pt)
# pt <- pt + annotate("text", label = paste0("*Due to small counts\nACT scores of 1-", low, " in bar ", 
#                     low, "\nand ", high, "-36 in ", high), 
#                     x = 2.5, y = 99, size = 3)

png(paste("../RaisngAchClsngGap/results/graphs/act_", 
            subj[i, 1], "_to_nsc_imTrans_by_cohort.png", sep = ""), 
     res = 125, width = 1000, height = 674)#, 
     #width = 8, height = 6)
   print(pt)

makeFootnote(paste0("Sample is students in GCPS fall 9th grade cohort with an ACT score (", N$tot[1], ").\n", 
                    "Due to small counts, ACT scores of 1-", low, " are in bar ", low, " and ", high, "-36 in ", high, ".",
                    "\nData sources are GCPS administrative records and ", 
                    "The National Student Clearinghouse.\n",
                    "Red vertical bar indicates 80% threshold value."), 
                    color = "grey60", size = .5)
 
  dev.off()
   assign(paste0(subj[i, 1], "act_to_nsc_imTrans"), pt, envir = .GlobalEnv)

}

# persistence
###


for (i in 1:length(subj[, 1])) {
  
# create totals for footnote
N <- ddply(act.nsc.m2.p[act.nsc.m2.p$Subject == subj[i, 1] & !is.na(act.nsc.m2.p$Score), c("cohort2", "N")], "cohort2", summarise, 
           N = sum(N))
N$tot <- paste(N$cohort2, " = ", N$N, sep = "", collapse = ", ")
  

pt <- ggplot(act.nsc.m2.p[act.nsc.m2.p$Subject == subj[i, 1] & !is.na(act.nsc.m2.p$Score), ], 
             aes(factor(Score), y = round(pe_avg*100, 1), label = round(pe_avg*100, 0)))
pt <- pt + geom_bar(stat = "identity")
pt <- pt + ggtitle(paste0("Proportion of 9th Grade Cohort with One-Year\n Post-Secondary Persistence by ", 
                          "ACT ", subj[i, 2], " Score"))
pt <- pt + xlab(paste0("ACT ", subj[i, 2], " Score\n\n\n"))
pt <- pt + ylab("% with NSC Indicated Persistence")
pt <- pt + scale_y_continuous(breaks = c(seq(0, 100, 20)), limits = c(0, 100))
pt <- pt + geom_text(vjust = 1, color = "white", size = 3)
pt <- pt + geom_hline(aes(yintercept = 67), size = 1, colour = "red", linetype = "dashed")
pt <- pt + geom_vline(aes(xintercept = subj[i, 4] - (low-1)), size = 4, colour = "red", linetype = "solid", alpha = 0.25)
pt <- pt + facet_wrap( ~ cohort2, ncol = 1)
print(pt)
# pt <- pt + annotate("text", label = paste0("*Due to small counts\nACT scores of 1-", low, " in bar ", 
#                     low, "\nand ", high, "-36 in ", high), 
#                     x = 2.5, y = 99, size = 3)

png(paste("../RaisngAchClsngGap/results/graphs/act_", 
            subj[i, 1], "_to_nsc_persist_by_cohort.png", sep = ""), 
     res = 125, width = 1000, height = 674)#, 
     #width = 8, height = 6)
   print(pt)

makeFootnote(paste0("Sample is students in GCPS fall 9th grade cohort with an ACT score (", N$tot[1], ").\n", 
                    "Due to small counts, ACT scores of 1-", low, " are in bar ", low, " and ", high, "-36 in ", high, ".",
                    "\nData sources are GCPS administrative records and ", 
                    "The National Student Clearinghouse.\n",
                    "Red vertical bar indicates 67% threshold value."), 
             color = "grey60", size = .5)
 
  dev.off()
   assign(paste0(subj[i, 1], "act_to_nsc_persist"), pt, envir = .GlobalEnv)


# threshold only
# graphs

pt <- ggplot(act.nsc.m2.pt[act.nsc.m2.pt$Subject == subj[i, 1] & !is.na(act.nsc.m2.pt$pe.mt), ], 
             aes(factor(pe.mt), y = round(pe_avg*100, 1), label = round(pe_avg*100, 0)))
pt <- pt + geom_bar(stat = "identity")
pt <- pt + ggtitle(paste0("Proportion of 9th Grade Cohort with One-Year\n Post-Secondary Persistence by ", 
                          "ACT ", subj[i, 2], " Threshold"))
pt <- pt + xlab(paste0("Met ACT ", subj[i, 2], " Threshold of ", subj[i, 4], "\n\n\n"))
pt <- pt + ylab("% with NSC Indicated Persistence")
pt <- pt + scale_y_continuous(breaks = c(seq(0, 100, 20)), limits = c(0, 100))
pt <- pt + geom_text(vjust = 1, color = "white", size = 5)
pt <- pt + geom_hline(aes(yintercept = 67), size = 1, colour = "red", linetype = "dashed")
pt <- pt + facet_wrap( ~ cohort2, ncol = 3)
print(pt)
# pt <- pt + annotate("text", label = paste0("*Due to small counts\nACT scores of 1-", low, " in bar ", 
#                     low, "\nand ", high, "-36 in ", high), 
#                     x = 2.5, y = 99, size = 3)


png(paste("../RaisngAchClsngGap/results/graphs/act_", 
            subj[i, 1], "_to_nsc_persist_thresh_by_cohort.png", sep = ""), 
     res = 125, width = 1000, height = 674)#, 
     #width = 8, height = 6)
   print(pt)

makeFootnote(paste0("Sample is students in GCPS fall 9th grade cohort with an ACT score (", N$tot[1], ").\n", 
                    "Data sources are GCPS administrative records and ", 
                    "The National Student Clearinghouse."),
             color = "grey60", size = .5)
 
  dev.off()
   assign(paste0(subj[i, 1], "act_to_nsc_persist_thresh"), pt, envir = .GlobalEnv)

}



# # v and x are predictor and nsc variables, respectively;
#   # y is predictor formal name; z is nsc formal name
# cumFreq <- function(v, x, y, z) {
#   t <- table(v, x)
#   p <- prop.table(t)
#   c <- cumsum(p)
#   return()            


act.nscF <- as.data.frame(lapply(act.nsc[, c("CO", "MA", "i.t", "p.e")], as.factor))


cors.ita <- ddply(act.nsc[, c("cohort", "CO", "i.t")], "cohort", summarise, 
              r = cor(CO, i.t))
cors.pea <- ddply(act.nsc[, c("cohort", "CO", "p.e")], "cohort", summarise, 
              r = cor(CO, p.e))

act.it <- ggplot(act.nsc, aes(i.t, CO)) + geom_boxplot()
act.it + geom_text(aes(label = round(CO, 1)))
act.it
act.pe <- ggplot(act.nsc, aes(p.e, CO)) + geom_boxplot()
act.pe

#########
# AP/IB
#########

apib.nsc <- merge(cohorts.nsc, ap.agg[, c("id", "ap.t")], by.x = "id", by.y = "id", 
                  all.x = TRUE)
apib.nsc <- merge(apib.nsc, ib.agg[, c("id", "ib.t")], by.x = "id", by.y = "id", 
                  all.x = TRUE)
apib.nsc <- merge(apib.nsc, gsdr.ib.agg[, c("id", "ib.t2")], by.x = "id", by.y = "id", 
                  all.x = TRUE)
  apib.nsc$apib.t <-  ifelse(apib.nsc$ap.t == TRUE & !is.na(apib.nsc$ap.t) | 
                      apib.nsc$ib.t == TRUE & !is.na(apib.nsc$ib.t) | 
                      apib.nsc$ib.t2 == TRUE & !is.na(apib.nsc$ib.t2), "Yes", "No")
  apib.nsc$cohort2 <- apib.nsc$cohort - 3

  

  # create totals for footnote
  apib.nsc.aggIT <- ddply(apib.nsc[, c("apib.t", "i.t", "cohort2")], c("cohort2", "apib.t"), summarise, 
                   N = length(i.t), 
                   it_avg = mean(i.t))

  N <- ddply(apib.nsc.aggIT[!is.na(apib.nsc.aggIT$apib.t), c("cohort2", "N")], "cohort2", summarise, 
             N = sum(N))
  N$tot <- paste(N$cohort2, " = ", N$N, sep = "", collapse = ", ")
    
    
# graphs

pt <- ggplot(apib.nsc.aggIT, 
             aes(factor(apib.t), y = round(it_avg*100, 1), label = round(it_avg*100, 0)))
pt <- pt + geom_bar(stat = "identity")
pt <- pt + ggtitle(paste0("Proportion of 9th Grade Cohort with Seamless ", 
                          "Post-Secondary Transition by \nAt Least: 1 AP Exam Scored 3 or Higher OR ", 
                          "1 IB Course Graded B or Higher"))
pt <- pt + xlab("\nAP Exam of (3, 4, or 5) OR IB Course Grade of (A or B)\n\n")
pt <- pt + ylab("% with NSC Indicated Seamless Transition")
pt <- pt + scale_y_continuous(breaks = c(seq(0, 100, 20)), limits = c(0, 100))
pt <- pt + geom_text(vjust = 1, color = "white", size = 5)
pt <- pt + geom_hline(aes(yintercept = 80), size = 1, colour = "red", linetype = "dashed")
pt <- pt + facet_wrap(~ cohort2, ncol = 3)
print(pt)


png(paste("../RaisngAchClsngGap/results/graphs/", 
          "apib_to_nsc_imTrans_thresh_by_cohort.png", sep = ""), 
     res = 125, width = 1000, height = 674)#, 
     #width = 8, height = 6)
   print(pt)
    makeFootnote(paste0("Sample is students in GCPS fall 9th grade cohort (", N$tot[1], ").\n", 
                    "Data sources are GCPS administrative records and ", 
                    "The National Student Clearinghouse."), 
             color = "grey60", size = .5)
 
  dev.off()
   assign(paste0(subj[i, 1], "apib_to_nsc_imTrans_thresh"), pt, envir = .GlobalEnv)




  # create totals for footnote
  apib.nsc.aggPE <- ddply(apib.nsc[, c("apib.t", "p.e", "cohort2")], c("cohort2", "apib.t"), summarise, 
                   N = length(p.e), 
                   pe_avg = mean(p.e))

  N <- ddply(apib.nsc.aggPE[!is.na(apib.nsc.aggPE$apib.t), c("cohort2", "N")], "cohort2", summarise, 
             N = sum(N))
  N$tot <- paste(N$cohort2, " = ", N$N, sep = "", collapse = ", ")


pt <- ggplot(apib.nsc.aggPE, 
             aes(factor(apib.t), y = round(pe_avg*100, 1), label = round(pe_avg*100, 0)))
pt <- pt + geom_bar(stat = "identity")
pt <- pt + ggtitle(paste0("Proportion of 9th Grade Cohort with One-Year Post-Secondary ", 
                          "Persistence by \nAt Least: 1 AP Exam Scored 3 or Higher OR ", 
                          "1 IB Course Graded B or Higher"))
pt <- pt + xlab("\nAP Exam of (3, 4, or 5) OR IB Course Grade of (A or B)\n\n")
pt <- pt + ylab("% with NSC Indicated Persistence")
pt <- pt + scale_y_continuous(breaks = c(seq(0, 100, 20)), limits = c(0, 100))
pt <- pt + geom_text(vjust = 1, color = "white", size = 5)
pt <- pt + geom_hline(aes(yintercept = 67), size = 1, colour = "red", linetype = "dashed")
pt <- pt + facet_wrap(~ cohort2, ncol = 3)
print(pt)


png(paste("../RaisngAchClsngGap/results/graphs/", 
          "apib_to_nsc_persist_thresh_by_cohort.png", sep = ""), 
     res = 125, width = 1000, height = 674)#, 
     #width = 8, height = 6)
   print(pt)
    makeFootnote(paste0("Sample is students in GCPS fall 9th grade cohort (", N$tot[1], ").\n", 
                    "Data sources are GCPS administrative records and ", 
                    "The National Student Clearinghouse."), 
             color = "grey60", size = .5)
 
  dev.off()
   assign(paste0(subj[i, 1], "apib_to_nsc_persist_thresh"), pt, envir = .GlobalEnv)

##########
# GPA
##########

gpa.aggf[, c("cu.gpa.la", "cu.gpa.ma", "cu.gpa.sc", "cu.gpa.ss", "cu.gpa.core")] <- 
  trunc(round(gpa.aggf[, c("cu.gpa.la", "cu.gpa.ma", "cu.gpa.sc", 
                           "cu.gpa.ss", "cu.gpa.core")], 0))

gpa.nsc <- merge(gpa.aggf, cohorts.nsc, by.x = "permnum", by.y = "id")
gpa.nsc$cohort2 <- gpa.nsc$cohort - 3
# 2008 2009 2010 // counts by cohort
# 8966 9039 8667 

cors.itgm <- cor(gpa.nsc$cu.gpa.ma, gpa.nsc$i.t, use = "pairwise.complete.obs")
cors.pegm <- cor(gpa.nsc$cu.gpa.ma, gpa.nsc$p.e, use = "pairwise.complete.obs")

gpa.itm <- ggplot(gpa.nsc, aes(i.t, cu.gpa.ma)) + geom_boxplot()
gpa.itm
gpa.pem <- ggplot(gpa.nsc, aes(p.e, cu.gpa.ma)) + geom_boxplot()
gpa.pem

gpa.itv <- ggplot(gpa.nsc, aes(i.t, cu.gpa.core)) + geom_boxplot()
gpa.itv
gpa.pev <- ggplot(gpa.nsc, aes(p.e, cu.gpa.core)) + geom_boxplot()
gpa.pev



# barplots by gpa value
####

names(gpa.nsc)[which(names(gpa.nsc) == "permnum")] <- "id"

# reshape for facet wrap by subj & cohort
gpa.nsc.m <- melt(gpa.nsc[, c("id", "cu.gpa.la", "cu.gpa.ma", "cu.gpa.sc",             
                              "cu.gpa.ss", "cu.gpa.core", "cohort2", "i.t", "p.e")], 
                  id.vars = c("id", "cohort2", "i.t", "p.e"))

# function to aggregate scores at ends w/ less than 100 cases across subjects
t <- as.data.frame(table(gpa.nsc.m$variable, gpa.nsc.m$value))

subj <- as.data.frame(matrix(cbind(c("cu.gpa.la",  "cu.gpa.ma", "cu.gpa.sc", "cu.gpa.ss", "cu.gpa.core"), 
                                   c("LA Cumulative HSGPA", "MA Cumulative HSGPA", "SC Cumulative HSGPA", 
                                     "SS Cumulative HSGPA", "Core Cumulative HSGPA"), 
                                   c(89, 87, 88, 90, 87), 
                                   c(92, 91, 92, 92, 92)), 
                                   ncol = 4))
subj[,3:4] <- lapply(subj[, 3:4], function(x) {as.numeric(levels(x))[x]})
s <- sapply(subj, is.factor)
subj[s] <- lapply(subj[s], as.character)
names(subj)[3:4] <- c("it.t", "pe.t")

# max less than median; min greater than median for any subject

t$lt100 <- t$Freq < 100
names(t)[1] <- "Subj"
names(t)[2] <- "Var1"
  t$Var1 <- as.numeric(levels(t$Var1))[t$Var1]
  low <- max(t[t$Var1 < 80 & t$lt100 == TRUE, "Var1"]) + 1
  high <- min(t[t$Var1 > 80 & t$lt100 == TRUE, "Var1"]) - 1

    t$value2 <- t$Var1
    t[t$Var1 <= low, dim(t)[2]] <- low
    t[t$Var1 >= high, dim(t)[2]] <- high

  gpa.nsc.m <- merge(gpa.nsc.m, unique(t[, c("Var1", "value2")]), by.x = "value", by.y = "Var1", all.x = TRUE)
  names(gpa.nsc.m)[6:7] <- c("Subject", "cumul.gpa")

  gpa.nsc.m.t <- merge(gpa.nsc.m, subj[, c(1, 3:4)], by.x = "Subject", by.y = "V1", all.x = TRUE)
    gpa.nsc.m.t$pe.mt <- ifelse(gpa.nsc.m.t$cumul.gpa >= gpa.nsc.m.t$pe.t & !(is.na(gpa.nsc.m.t$cumul.gpa)), "Yes", "No")

  gpa.nsc.m2 <- ddply(gpa.nsc.m[, c("cohort2", "Subject", "cumul.gpa", "i.t")], c("cohort2", "Subject", "cumul.gpa"), 
                   summarise, 
                   N = length(i.t), 
                   it_avg = mean(i.t))

  gpa.nsc.m2.p <- ddply(gpa.nsc.m[, c("cohort2", "Subject", "cumul.gpa", "p.e")], c("cohort2", "Subject", "cumul.gpa"), 
                   summarise, 
                   N = length(p.e), 
                   pe_avg = mean(p.e))

  gpa.nsc.m2.pt <- ddply(gpa.nsc.m.t[, c("cohort2", "Subject", "pe.mt", "p.e")], c("cohort2", "Subject", "pe.mt"), 
                   summarise, 
                   N = length(p.e), 
                   pe_avg = mean(p.e))
  
    
    # check if every cell, once divided by cohort, is >= 30 cases
    stopifnot(table(gpa.nsc.m$cohort, gpa.nsc.m$Subject, gpa.nsc.m$cumul.gpa) >= 30)

# graph

# immediate transition
###

for (i in 1:length(subj[, 1])) {
  
  # create totals for footnote
  N <- ddply(gpa.nsc.m2[gpa.nsc.m2$Subject == subj[i, 1] & !is.na(gpa.nsc.m2$cumul.gpa), c("cohort2", "N")], "cohort2", summarise, 
             N = sum(N))
  N$tot <- paste(N$cohort, " = ", N$N, sep = "", collapse = ", ")
  
# create plot
pt <- ggplot(gpa.nsc.m2[gpa.nsc.m2$Subject == subj[i, 1] & !is.na(gpa.nsc.m2$cumul.gpa), ], 
             aes(factor(cumul.gpa), y = round(it_avg*100, 1), label = round(it_avg*100, 0)))
pt <- pt + geom_bar(stat = "identity")
pt <- pt + ggtitle(paste0("Proportion of 9th Grade Cohort with Seamless \nPost-Secondary Transition by ", 
                          subj[i, 2]))
pt <- pt + xlab(paste0(subj[i, 2], "\n\n\n"))
pt <- pt + ylab("% with NSC Indicated Seamless Transition")
pt <- pt + scale_y_continuous(breaks = c(seq(0, 100, 20)), limits = c(0, 100))
pt <- pt + geom_text(vjust = 1, color = "white", size = 3)
pt <- pt + geom_hline(aes(yintercept = 80), size = 1, colour = "red", linetype = "dashed")
pt <- pt + geom_vline(aes(xintercept = subj[i, 3] - (low-1)), size = 4, colour = "red", linetype = "solid", alpha = 0.25)
pt <- pt + facet_wrap( ~ cohort2, ncol = 1)
print(pt)
# pt <- pt + annotate("text", label = paste0("*Due to small counts\nACT scores of 1-", low, " in bar ", 
#                     low, "\nand ", high, "-36 in ", high), 
#                     x = 2.5, y = 99, size = 3)

png(paste("../RaisngAchClsngGap/results/graphs/gpa_", 
            subj[i, 1], "_to_NSC_imTrans_by_cohort.png", sep = ""), 
     res = 125, width = 1000, height = 674)#, 
     #width = 8, height = 6)
   print(pt)

makeFootnote(paste0("Sample is students in GCPS fall 9th grade cohort with a cumulative HSGPA (", N$tot[1], ").\n", 
                    "Due to small counts, HSGPA values of 0-", low, " are in bar ", low, 
                    " and ", high, "-110 in ", high, ".", 
                    "\nData sources are GCPS administrative records and ", 
                    "The National Student Clearinghouse.\n",
                    "Red vertical bar indicates 80% threshold value."), 
             color = "grey60", size = .5)
 
  dev.off()
   assign(paste0(subj[i, 1], "gpa_to_nsc_imTrans"), pt, envir = .GlobalEnv)

}

# persistence
###


for (i in 1:length(subj[, 1])) {
  
   # create totals for footnote
  N <- ddply(gpa.nsc.m2[gpa.nsc.m2$Subject == subj[i, 1] & !is.na(gpa.nsc.m2$cumul.gpa), c("cohort2", "N")], "cohort2", summarise, 
             N = sum(N))
  N$tot <- paste(N$cohort, " = ", N$N, sep = "", collapse = ", ")

pt <- ggplot(gpa.nsc.m2.p[gpa.nsc.m2.p$Subject == subj[i, 1] & !is.na(gpa.nsc.m2$cumul.gpa), ],
             aes(factor(cumul.gpa), y = round(pe_avg*100, 1), label = round(pe_avg*100, 0)))
pt <- pt + geom_bar(stat = "identity")
pt <- pt + ggtitle(paste0("Proportion of 9th Grade Cohort with One-Year\n Post-Secondary Persistence by ", 
                          subj[i, 2]))
pt <- pt + xlab(paste0(subj[i, 2], "\n\n\n"))
pt <- pt + ylab("% with NSC Indicated Persistence")
pt <- pt + scale_y_continuous(breaks = c(seq(0, 100, 20)), limits = c(0, 100))
pt <- pt + geom_text(vjust = 1, color = "white", size = 3)
pt <- pt + geom_hline(aes(yintercept = 67), size = 1, colour = "red", linetype = "dashed")
pt <- pt + geom_vline(aes(xintercept = subj[i, 4] - (low-1)), size = 4, colour = "red", linetype = "solid", alpha = 0.25)
pt <- pt + facet_wrap( ~ cohort2, ncol = 1)
print(pt)
# pt <- pt + annotate("text", label = paste0("*Due to small counts\nACT scores of 1-", low, " in bar ", 
#                     low, "\nand ", high, "-36 in ", high), 
#                     x = 2.5, y = 99, size = 3)

png(paste("../RaisngAchClsngGap/results/graphs/gpa_", 
            subj[i, 1], "_to_NSC_persist_by_cohort.png", sep = ""), 
     res = 125, width = 1000, height = 674)#, 
     #width = 8, height = 6)
   print(pt)

makeFootnote(paste0("Sample is students in GCPS fall 9th grade cohort with cumulative HSGPA (", N$tot[1], ").\n", 
                    "Due to small counts, HSGPA values of 0-", low, " are in bar ", low, 
                    " and ", high, "-110 in ", high, ".",
                    "\nData sources are GCPS administrative records and ", 
                    "The National Student Clearinghouse.\n",
                    "Red vertical bar indicates 67% threshold value."), 
             color = "grey60", size = .5)
 
  dev.off()
   assign(paste0(subj[i, 1], "gpa_to_nsc_persist"), pt, envir = .GlobalEnv)


# threshold only
# graphs

pt <- ggplot(gpa.nsc.m2.pt[gpa.nsc.m2.pt$Subject == subj[i, 1] & !is.na(gpa.nsc.m2.pt$pe.mt), ], 
             aes(factor(pe.mt), y = round(pe_avg*100, 1), label = round(pe_avg*100, 0)))
pt <- pt + geom_bar(stat = "identity")
pt <- pt + ggtitle(paste0("Proportion of 9th Grade Cohort with One-Year\n Post-Secondary Persistence by ", 
                          subj[i, 2], " Threshold"))
pt <- pt + xlab(paste0("\nMet ", subj[i, 2], " Threshold of ", subj[i, 4], "\n\n"))
pt <- pt + ylab("% with NSC Indicated Persistence")
pt <- pt + scale_y_continuous(breaks = c(seq(0, 100, 20)), limits = c(0, 100))
pt <- pt + geom_text(vjust = 1, color = "white", size = 5)
pt <- pt + geom_hline(aes(yintercept = 67), size = 1, colour = "red", linetype = "dashed")
pt <- pt + facet_wrap( ~ cohort2, ncol = 3)
print(pt)
# pt <- pt + annotate("text", label = paste0("*Due to small counts\nACT scores of 1-", low, " in bar ", 
#                     low, "\nand ", high, "-36 in ", high), 
#                     x = 2.5, y = 99, size = 3)


png(paste("../RaisngAchClsngGap/results/graphs/gpa_", 
            subj[i, 1], "_to_nsc_persist_thresh_by_cohort.png", sep = ""), 
     res = 125, width = 1000, height = 674)#, 
     #width = 8, height = 6)
   print(pt)

makeFootnote(paste0("Sample is students in GCPS fall 9th grade cohort with an ACT score (", N$tot[1], ").\n", 
                    "Data sources are GCPS administrative records and ", 
                    "The National Student Clearinghouse."),
             color = "grey60", size = .5)
 
  dev.off()
   assign(paste0(subj[i, 1], "gpa_to_nsc_persist_thresh"), pt, envir = .GlobalEnv)

}

###########
# SAT
###########

sat.nsc <- merge(sat, cohorts.nsc, by.x = "stunumb", by.y = "id")


sat.itm <- ggplot(sat.nsc, aes(i.t, ma)) + geom_boxplot()
sat.itm
sat.pem <- ggplot(sat.nsc, aes(p.e, ma)) + geom_boxplot()
sat.pem

sat.itv <- ggplot(sat.nsc, aes(i.t, ve)) + geom_boxplot()
sat.itv
sat.pev <- ggplot(sat.nsc, aes(p.e, ve)) + geom_boxplot()
sat.pev


sat.nsc$cohort2 <- sat.nsc$cohort - 3
# 2008 2009 2010 // by cohort
# 6300 6884 6743 




# barplots by gpa value
####

names(sat.nsc)[which(names(sat.nsc) == "stunumb")] <- "id"

# reshape for facet wrap by subj & cohort
sat.nsc.m <- melt(sat.nsc[, c("id", "ma", "ve", "cohort2", "i.t", "p.e")], 
                  id.vars = c("id", "cohort2", "i.t", "p.e"))

# function to aggregate scores at ends w/ less than 100 cases across subjects
t <- as.data.frame(table(sat.nsc.m$variable, sat.nsc.m$value))

subj <- as.data.frame(matrix(cbind(c("ma",  "ve"), 
                                   c("SAT Mathematics Scale Score", "SAT Critical Reading Scale Score"), 
                                   c(460, 440), 
                                   c(580, 580)), 
                                   ncol = 4))
subj[,3:4] <- lapply(subj[, 3:4], function(x) {as.numeric(levels(x))[x]})
s <- sapply(subj, is.factor)
subj[s] <- lapply(subj[s], as.character)
names(subj)[3:4] <- c("it.t", "pe.t")

# max less than median; min greater than median for any subject

t$lt100 <- t$Freq < 100
names(t)[1] <- "Subj"
names(t)[2] <- "Var1"
  t$Var1 <- as.numeric(levels(t$Var1))[t$Var1]
  low <- max(t[t$Var1 < median(t$Var1) & t$lt100 == TRUE, "Var1"]) + 10
  high <- min(t[t$Var1 > median(t$Var1) & t$lt100 == TRUE, "Var1"]) - 10

    t$value2 <- t$Var1
    t[t$Var1 <= low, dim(t)[2]] <- low
    t[t$Var1 >= high, dim(t)[2]] <- high

  sat.nsc.m <- merge(sat.nsc.m, unique(t[, c("Var1", "value2")]), by.x = "value", by.y = "Var1", all.x = TRUE)
  names(sat.nsc.m)[6:7] <- c("Subject", "Score")

  sat.nsc.m.t <- merge(sat.nsc.m, subj[, c(1, 3:4)], by.x = "Subject", by.y = "V1", all.x = TRUE)
    sat.nsc.m.t$pe.mt <- ifelse(sat.nsc.m.t$Score >= sat.nsc.m.t$pe.t & !(is.na(sat.nsc.m.t$Score)), "Yes", "No")

  sat.nsc.m2 <- ddply(sat.nsc.m[, c("cohort2", "Subject", "Score", "i.t")], c("cohort2", "Subject", "Score"), 
                   summarise, 
                   N = length(i.t), 
                   it_avg = mean(i.t))

  sat.nsc.m2.p <- ddply(sat.nsc.m[, c("cohort2", "Subject", "Score", "p.e")], c("cohort2", "Subject", "Score"), 
                   summarise, 
                   N = length(p.e), 
                   pe_avg = mean(p.e))

  sat.nsc.m2.pt <- ddply(sat.nsc.m.t[, c("cohort2", "Subject", "pe.mt", "p.e")], c("cohort2", "Subject", "pe.mt"), 
                   summarise, 
                   N = length(p.e), 
                   pe_avg = mean(p.e))
  
    
    # check if every cell, once divided by cohort, is >= 30 cases
    stopifnot(table(sat.nsc.m$cohort, sat.nsc.m$Subject, sat.nsc.m$Score) >= 30)

# graph


# immediate transition
###

for (i in 1:length(subj[, 1])) {
  
  # create totals for footnote
  N <- ddply(sat.nsc.m2[sat.nsc.m2$Subject == subj[i, 1] & !is.na(sat.nsc.m2$Score), c("cohort2", "N")], "cohort2", summarise, 
             N = sum(N))
  N$tot <- paste(N$cohort, " = ", N$N, sep = "", collapse = ", ")
  
# create plot
pt <- ggplot(sat.nsc.m2[sat.nsc.m2$Subject == subj[i, 1] & !is.na(sat.nsc.m2$Score), ], 
             aes(factor(Score), y = round(it_avg*100, 1), label = round(it_avg*100, 0)))
pt <- pt + geom_bar(stat = "identity")
pt <- pt + ggtitle(paste0("Proportion of 9th Grade Cohort with Seamless \nPost-Secondary Transition by ", 
                          subj[i, 2]))
pt <- pt + xlab(paste0(subj[i, 2], "\n\n\n"))
pt <- pt + ylab("% with NSC Indicated Seamless Transition")
pt <- pt + scale_x_discrete(breaks = c("350", "400", "450", "500", "550", "600", "650", "700", "750", "800"))
pt <- pt + scale_y_continuous(breaks = c(seq(0, 100, 20)), limits = c(0, 100))
pt <- pt + geom_text(vjust = 1, color = "white", size = 3)
pt <- pt + geom_hline(aes(yintercept = 80), size = 1, colour = "red", linetype = "dashed")
pt <- pt + geom_vline(aes(xintercept = subj[i, 3]*.1 - (low*.1-1)), size = 4, colour = "red", linetype = "solid", alpha = 0.25)
pt <- pt + facet_wrap( ~ cohort2, ncol = 1)
print(pt)
# pt <- pt + annotate("text", label = paste0("*Due to small counts\nACT scores of 1-", low, " in bar ", 
#                     low, "\nand ", high, "-36 in ", high), 
#                     x = 2.5, y = 99, size = 3)

png(paste("../RaisngAchClsngGap/results/graphs/sat_", 
            subj[i, 1], "_to_nsc_imTrans_by_cohort.png", sep = ""), 
     res = 125, width = 1000, height = 674)#, 
     #width = 8, height = 6)
   print(pt)

makeFootnote(paste0("Sample is students in GCPS fall 9th grade cohort with an SAT Score (", N$tot[1], ").\n", 
                    "Due to small counts, SAT Scores of 200-", low, " are in bar ", low, 
                    " and ", high, "-800 in ", high,
                    "\nData sources are GCPS administrative records and ", 
                    "The National Student Clearinghouse.\n",
                    "Red vertical bar indicates 80% threshold value."), 
             color = "grey60", size = .5)
 
  dev.off()
   assign(paste0(subj[i, 1], "sat_to_nsc_imTrans"), pt, envir = .GlobalEnv)

}

# persistence
###


for (i in 1:length(subj[, 1])) {
  
   # create totals for footnote
  N <- ddply(sat.nsc.m2[sat.nsc.m2$Subject == subj[i, 1] & !is.na(sat.nsc.m2$Score), c("cohort2", "N")], "cohort2", summarise, 
             N = sum(N))
  N$tot <- paste(N$cohort, " = ", N$N, sep = "", collapse = ", ")

pt <- ggplot(sat.nsc.m2.p[sat.nsc.m2.p$Subject == subj[i, 1] & !is.na(sat.nsc.m2$Score), ],
             aes(factor(Score), y = round(pe_avg*100, 1), label = round(pe_avg*100, 0)))
pt <- pt + geom_bar(stat = "identity")
pt <- pt + ggtitle(paste0("Proportion of 9th Grade Cohort with One-Year\n Post-Secondary Persistence by ", 
                          subj[i, 2]))
pt <- pt + xlab(paste0(subj[i, 2], "\n\n\n"))
pt <- pt + ylab("% with NSC Indicated Persistence")
pt <- pt + scale_x_discrete(breaks = c("350", "400", "450", "500", "550", "600", "650", "700", "750", "800"))
pt <- pt + scale_y_continuous(breaks = c(seq(0, 100, 20)), limits = c(0, 100))
pt <- pt + geom_text(vjust = 1, color = "white", size = 3)
pt <- pt + geom_hline(aes(yintercept = 67), size = 1, colour = "red", linetype = "dashed")
pt <- pt + geom_vline(aes(xintercept = subj[i, 4]*.1 - (low*.1-1)), size = 4, colour = "red", linetype = "solid", alpha = 0.25)
pt <- pt + facet_wrap( ~ cohort2, ncol = 1)
print(pt)
# pt <- pt + annotate("text", label = paste0("*Due to small counts\nACT scores of 1-", low, " in bar ", 
#                     low, "\nand ", high, "-36 in ", high), 
#                     x = 2.5, y = 99, size = 3)

png(paste("../RaisngAchClsngGap/results/graphs/sat_", 
            subj[i, 1], "_to_nsc_persist_by_cohort.png", sep = ""), 
     res = 125, width = 1000, height = 674)#, 
     #width = 8, height = 6)
   print(pt)

makeFootnote(paste0("Sample is students in GCPS fall 9th grade cohort with an SAT Score (", N$tot[1], ").\n", 
                    "Due to small counts, SAT Scores of 200-", low, " are in bar ", low, 
                    " and ", high, "-800 in ", high,
                    "\nData sources are GCPS administrative records and ", 
                    "The National Student Clearinghouse.\n",
                    "Red vertical bar indicates 67% threshold value."), 
             color = "grey60", size = .5)
 
  dev.off()
   assign(paste0(subj[i, 1], "gpa_to_nsc_persist"), pt, envir = .GlobalEnv)


# threshold only
# graphs

pt <- ggplot(sat.nsc.m2.pt[sat.nsc.m2.pt$Subject == subj[i, 1] & !is.na(sat.nsc.m2.pt$pe.mt), ], 
             aes(factor(pe.mt), y = round(pe_avg*100, 1), label = round(pe_avg*100, 0)))
pt <- pt + geom_bar(stat = "identity")
pt <- pt + ggtitle(paste0("Proportion of 9th Grade Cohort with One-Year\n Post-Secondary Persistence by ", 
                          "SAT ", subj[i, 2], " Threshold"))
pt <- pt + xlab(paste0("Met SAT ", subj[i, 2], " Threshold of ", subj[i, 4], "\n\n\n"))
pt <- pt + ylab("% with NSC Indicated Persistence")
pt <- pt + scale_y_continuous(breaks = c(seq(0, 100, 20)), limits = c(0, 100))
pt <- pt + geom_text(vjust = 1, color = "white", size = 5)
pt <- pt + geom_hline(aes(yintercept = 67), size = 1, colour = "red", linetype = "dashed")
pt <- pt + facet_wrap( ~ cohort2, ncol = 3)
print(pt)


png(paste("../RaisngAchClsngGap/results/graphs/sat_", 
            subj[i, 1], "_to_nsc_persist_thresh_by_cohort.png", sep = ""), 
     res = 125, width = 1000, height = 674)#, 
     #width = 8, height = 6)
   print(pt)

makeFootnote(paste0("Sample is students in GCPS fall 9th grade cohort with an SAT score (", N$tot[1], ").\n", 
                    "Data sources are GCPS administrative records and ", 
                    "The National Student Clearinghouse."),
             color = "grey60", size = .5)
 
  dev.off()
   assign(paste0(subj[i, 1], "sat_to_nsc_persist_thresh"), pt, envir = .GlobalEnv)


}

save.image("../RaisngAchClsngGap/data/prep/act_apib_gpa_sat_nsc.RData")





