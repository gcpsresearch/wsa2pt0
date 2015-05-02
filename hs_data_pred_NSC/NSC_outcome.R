#  use w/ CCR code to get grad date, immediate enrollment, persistence
#   will be used to examine thresholds from CollegeBoard and ACT research

#   created on    2014.03.21 by James Appleton
#   last updated  2015.04.24 by James Appleton  #relaxed requirements for status for i.t and p.e

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
require(data.table)
require(lubridate)


# registerDoParallel(4, cores = 4)
# getDoParWorkers()

rm(list=ls()) 
gc()
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

gradYear_shrt <- c(2011, 2012, 2013) # b/c 2014 doesn't have 2 semesters of time yet

yrs <- length(gradYear_shrt)  # number of years set below

fourYear1       <- "2007-08" # for 2011 grads
fourYear_shrt1  <- "2008"

fourYear2       <- "2008-09" # for 2012 grads
fourYear_shrt2  <- "2009"

fourYear3       <- "2009-10" # for 2013 grads
fourYear_shrt3  <- "2010"

fourYear <- c(fourYear1, fourYear2, fourYear3)
fourYear_shrt <- c(fourYear_shrt1, fourYear_shrt2, fourYear_shrt3)

###################################################
### Load the NSC data
###################################################

nsc <- read.csv(paste0(path, "\\Research Projects\\NSC Student Tracker\\", 
                       "NSC StudentTracker_2014.10_2014Graduates\\received\\", 
                       "1302550hs_10001139-28963-DETAIL-EFFDT-20141126-RUNDT-20141204.csv"),
                sep = ",", header = TRUE)

  nsc <- case.cols("nsc")

  # parse date to extract calendar days enrolled
  nsc$e.beg <- ymd(nsc$enrollment_begin)
  nsc$e.end <- ymd(nsc$enrollment_end)

  # change NA enrollment begin and end dates so can't count within enrollment periods
#   nsc[is.na(nsc$e.beg), "e.beg" ] <- 0
#   nsc[is.na(nsc$e.end), "e.end" ] <- 0

  # keep students graduating in cohort years and assign cohort
  nsc$gradGroup <- NA
  nsc$academicYr <- NA

for (i in 1:yrs) {
  
    nsc[nsc$high_school_grad_date >= (gradYear_shrt[i] - 1)*10000 + 0801 & 
        nsc$high_school_grad_date <= gradYear_shrt[i]*10000 + 0731, "gradGroup"] <- gradYear_shrt[i]
    
    nsc[!is.na(nsc$enrollment_begin) &
          nsc$enrollment_begin >= (gradYear_shrt[i])*10000 + 0801 & 
          !is.na(nsc$enrollment_end) & 
        nsc$enrollment_end <= (gradYear_shrt[i] + 1)*10000 + 0815, "academicYr"] <- gradYear_shrt[i]
}

# check that both enrollment_begin and enrollment_end are always both NA if either one is NA
stopifnot(is.na(nsc[is.na(nsc$e.beg), "e.end"]))
  stopifnot(is.na(nsc[is.na(nsc$e.end), "e.beg"]))
# check day differences are as expected: enrollment_end greater than enrollment_begin
  stopifnot(nsc[!is.na(nsc$e.end), "e.end"] - 
              nsc[!is.na(nsc$e.end), "e.beg"] >= 0)


nsc$e.days <- NA
nsc[!is.na(nsc$e.beg), "e.days"] <- nsc[!is.na(nsc$e.end), "e.end"] - 
              nsc[!is.na(nsc$e.end), "e.beg"]
nsc$e.days <- nsc$e.days/(60*60*24)



# (F)ull-time, (H)alf-time, (L)ess than half-time, (Q) 3/4 time, 
#   (A) Leave of absence, (W)ithdrawn, (D)eceased, (" ") blank if school doesn't define
  # to deal with this we will (" ") will not exclude an enrollment from counting
#   from: http://www.studentclearinghouse.org/colleges/files/ST_DetailReportGuide.pdf

recY_noGrad <- expression(nsc$record_found_y.n == "Y" & is.na(nsc$graduation_date))

  # create gcps id
  nsc[,1] <- as.character(nsc[,1])
  nsc$id <- as.numeric(substr(nsc[,1], 1, nchar(nsc[,1]) - 1))

      nsc <- nsc[!is.na(nsc$gradGroup), ]

  # create immed.transition and persist.enroll variables
    # date choice source: http://gardnercenter.stanford.edu/resources/publications/TechnicalGuide.CRIS.pdf (p.6)
      # check on variation in days enrolled (e.g., Feb. 29 through March 2) - can we set min num of days?

  nsc$i.t <- FALSE
  nsc$p.e <- FALSE
  

  for (i in gradYear_shrt) {
          
          # it filter
          filt.it <- expression(nsc$i.t == FALSE & 
                                  nsc$record_found_y.n == "Y" & 
                                  is.na(nsc$graduation_date))
    
        # immediate transition (i.t) calculation
        nsc[eval(filt.it), "i.t"] <- nsc[eval(filt.it), "enrollment_begin"] < i*10000 + 1101 & 
                                      nsc[eval(filt.it), "enrollment_end"] > i*10000 + 915 & 
                                      nsc[eval(filt.it), "gradGroup"] == i & 
                                      nsc[eval(filt.it), "enrollment_status"] %in% c("F", "Q", "H", "L", "D", "")
        
            # need to sum days across parameter-meeting enrollments b/c institutions split enrollments differently
            it <- ddply(nsc[!is.na(nsc$e.days), c("id", "i.t", "e.days")], c("id", "i.t"), summarise, 
                        immed.t = sum(i.t),
                        e.sum   = sum(e.days))
          
            # does the sum of parameter-meeting enrollments meet our minimum number of days?
            it$i.t2 <- it$immed.t > 0 & it$e.sum >= 54 # quarters range from 8 to 13 weeks or 56 to 91 days; 54 allows for end bf weekend
          
              # filter down to one row per student    
              it <- ddply(it[, c("id", "i.t2")], "id", summarise, 
                          i.t2 = sum(i.t2) > 0)
        
              nsc[nsc$gradGroup == i, "i.t"] <- FALSE
  
              # keep accruing i.t TRUEs for subsequent p.e step; Assumes we only care that >= 1 record is i.t
              nsc[nsc$i.t == FALSE & 
                    nsc$id %in% it[it$i.t2 == TRUE, "id"], 
                  "i.t"] <- TRUE
    
          # pe filter
          filt.pe <- expression(nsc$p.e == FALSE & 
                                  nsc$record_found_y.n == "Y" & 
                                  is.na(nsc$graduation_date))
        
      # persistent enrollment (p.e) calculation     
       nsc[eval(filt.pe), "p.e"] <- nsc[eval(filt.pe), "i.t"] == TRUE &
                                      nsc[eval(filt.pe), "enrollment_begin"] < (i + 1)*10000 + 501 & 
                                      nsc[eval(filt.pe), "enrollment_end"] > (i + 1)*10000 + 301 & 
                                      nsc[eval(filt.pe), "gradGroup"] == i & 
                                      nsc[eval(filt.pe), "enrollment_status"] %in% c("F", "Q", "H", "L", "D", "")


            pe <- ddply(nsc[!is.na(nsc$e.days), c("id", "p.e", "e.days")], c("id", "p.e"), summarise, 
                        pers.e = sum(p.e),
                        e.sum   = sum(e.days))
          
            pe$p.e2 <- pe$pers.e > 0 & pe$e.sum >= 54 # quarters range from 8 to 13 weeks or 56 to 91 days; 54 allows for end bf weekend
    
            # filter down to one row per student    
            pe <- ddply(pe[, c("id", "p.e2")], "id", summarise, 
                        p.e2 = sum(p.e2) > 0)
        
            # keep accruing p.e TRUEs; assumes we only care that >= 1 record is p.e
              nsc[nsc$p.e == FALSE & 
                nsc$id %in% pe[pe$p.e2 == TRUE, "id"], 
                "p.e"] <- TRUE
                  
  }

      
        nsc <- unique(nsc[, c("id", "first_name", "middle_name", "last_name", 
                              "high_school_grad_date", "gradGroup", "i.t", "p.e")])

        nsc.model <- ddply(nsc[, c("id", "i.t", "p.e")], "id", summarise, 
                          i.t = sum(i.t) > 0, 
                          p.e = sum(p.e) > 0)

        # check percentages relative to NSC report
        print(mean(nsc.model$i.t))
        print(mean(nsc.model$p.e))

  stopifnot(.69 - mean(nsc.model$i.t) < .03)

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
        WHERE  [SCHOOL_YEAR] >=", gradYear_shrt[1] - 3, " and 
               [SCHOOL_YEAR] <=", gradYear_shrt[length(gradYear_shrt)], " and 
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
              WHERE school_year >=", gradYear_shrt[1] - 3, " and
              	    school_year <=", gradYear_shrt[length(gradYear_shrt)], " and 
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
              WHERE SCHOOL_YEAR >=", gradYear_shrt[1] - 4, " and 
                    SCHOOL_YEAR <=", gradYear_shrt[length(gradYear_shrt)], " and 
                    SCORE is not null and 
                    SCORE != 0
        "))

    ap <- case.cols("ap")
    names(ap)[which(names(ap) == "stunumb")] <- "id"

      ap.agg <- ddply(ap[, c("id", "score")], "id", summarise, 
                  mxScr  = max(score))

      ap.agg$ap.t <- ap.agg$mxScr >= 3

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
                  "\\RBES\\Grad Rate Reports\\ACGR Data\\ClassOf", gradYear_shrt[i], "ACGRData_4and5yr.csv")

# this portion removes excess quotation marks
d <- readLines(con = fileLoc)
d <- gsub("\"", "", d)
      write.table(d, file = "d.txt", quote = FALSE, row.names = FALSE, col.names 
                  = FALSE)
      df <- read.csv(file = "d.txt",  header = TRUE)

# df <- read.csv(paste0("..\\RaisngAchClsngGap\\data\\prep\\DOECohortData_", fourYear[i],  
#                         "_jja.csv"), sep = ",", header = TRUE)

df <- case.cols("df")
  names(df)[40] <- "update.diploma.type"

      df <- df[df$grad.rate.type == 4 & df$school.id == "ALL", ]
      df$grad <- df$update.diploma.type %in% c("G", "C", "B", "V")
      df$cohort <- gradYear_shrt[i]

  df <- merge(df, id, by.x = "student.id", by.y = "gtid", all.x = TRUE)

assign(paste0("cohort.", gradYear_shrt[i]), df)

}


cohort.list <- as.list(rbind(list=ls(pattern = "cohort.2")))
cohorts <- get(paste0(cohort.list[1]))
nms <- names(cohorts)

for (i in 2:length(cohort.list)) {
df <- get(paste0(cohort.list[i]))
names(df) <- nms
cohorts <- rbind(cohorts, df)
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
                     "Transfer: Another State", "Transfer: Out of Country", "Tcoransfer: DJJ", 
                     "Death", "SB10 Transfer to State Schools", "SB10 Transfer to Private School", 
                     "SB10 Transfer to Public School"), 
                   c("K", "H", "T", "X", "J", "4", "D", "Y", "Z", "1"))

cohorts <- cohorts[!(cohorts$updated.withdrawal.reason %in% transfers[, 2]), ]

################################
# make dfs and graph/model
################################

# keep all - think those entering late transferred in; [cohorts$entry.school.year %in% fourYear_shrt, ]
cohorts.nsc <- merge(cohorts, nsc.model, by.x = "id", by.y = "id", all.x = TRUE)
cohorts.nsc[is.na(cohorts.nsc$i.t) | cohorts.nsc$grad == FALSE, "i.t"] <- FALSE
cohorts.nsc[is.na(cohorts.nsc$p.e) | cohorts.nsc$grad == FAcLSE, "p.e"] <- FALSE

save.image("../RaisngAchClsngGap/data/prep/NSC_outcome.RData")




