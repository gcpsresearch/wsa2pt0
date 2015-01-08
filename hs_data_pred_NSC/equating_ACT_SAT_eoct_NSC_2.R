# equating/linking HSGPA and Econ EOCT to "college-ready" SAT and ACT scores
#   will be used to examine thresholds from CollegeBoard and ACT research

## answers the question: For graduates, what HSGPA, Econ EOCT, SAT, and ACT values
##    give students an 80% chance of enrolling in college, and 2-year persistence in college

#   created on    2014.03.21 by James Appleton
#   last updated   2014.01.06 by James Appleton

require(ggplot2)
require(grid)
require(RODBC)
require(plyr)
require(foreign)
require(reshape2)

rm(list=ls())
path <- readLines("c:\\current_path.txt")

# set directories
setwd (paste(path,                        
             "\\Research Projects\\RaisngAchClsngGap",sep=""))
maindir <- paste(path,                        
             "\\Research Projects\\RaisngAchClsngGap",sep="")
dir ()

# function
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

# set years for graduation data

cohortYear_shrt <- c(2010, 2011, 2012) # b/c 2013 doesn't have 4 semesters of time yet

yrs <- length(cohortYear_shrt)  # number of years set below

startYear1       <- "2006-07" # for 2011 grads
startYear_shrt1  <- "2007"

startYear2       <- "2007-08" # for 2011 grads
startYear_shrt2  <- "2008"

startYear3       <- "2008-09" # for 2012 grads
startYear_shrt3  <- "2009"

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
  nsc$p.e2 <- FALSE
  nsc$p.e3 <- FALSE

  

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

    nsc[nsc$p.e2 == FALSE, "p.e2"] <- nsc[nsc$p.e2 == FALSE, "i.t"] == TRUE &     
                    nsc[nsc$p.e2 == FALSE, "enrollment_begin"] < (cohortYear_shrt[i] + 1)*10000 + 1101 & 
                    nsc[nsc$p.e2 == FALSE, "enrollment_end"] > (cohortYear_shrt[i] + 1)*10000 + 915 & 
                    nsc[nsc$p.e2 == FALSE, "cohort"] == cohortYear_shrt[i] & 
                    nsc[nsc$p.e2 == FALSE, "enrollment_status"] %in% c("F", "Q")
 
    nsc[nsc$p.e3 == FALSE, "p.e3"] <- nsc[nsc$p.e3 == FALSE, "i.t"] == TRUE & 
                    nsc[nsc$p.e3 == FALSE, "enrollment_begin"] < (cohortYear_shrt[i] + 2)*10000 + 501 & 
                    nsc[nsc$p.e3 == FALSE, "enrollment_end"] > (cohortYear_shrt[i] + 2)*10000 + 301 & 
                    nsc[nsc$p.e3 == FALSE, "cohort"] == cohortYear_shrt[i] & 
                    nsc[nsc$p.e3 == FALSE, "enrollment_status"] %in% c("F", "Q")

  }
    
      mrg <- ddply(nsc[, c("id", "p.e1", "p.e2", "p.e3", "i.t")], "id", summarise, 
                   pe1 = sum(p.e1), 
                   pe2 = sum(p.e2), 
                   pe3 = sum(p.e3), 
                   i.t = sum(i.t))
    
        mrg$p.e <- mrg$pe1 == 1 & mrg$pe2 == 1 & mrg$pe3 == 1
    
        nsc <- merge(nsc, mrg[, c("id", "i.t", "p.e")], by.x = "id", by.y = "id", all.x = TRUE)

        nsc <- unique(nsc[, c(1, 3:5, 10, 25, 29, 31)])
          colnames(nsc)[which(names(nsc) == "i.t.x")] <- "i.t"
    
  nsc.model <- nsc[, c("id", "cohort", "i.t", "p.e")]

    ma_ch <- odbcConnect("ODS_Prod_MA", uid = "Research", pwd = "Research")
    
    

##################################
## get ACT data
##################################

    act <- sqlQuery(ma_ch, paste0(
      "   SELECT [STUNUMB]
                ,[SCHOOL_YEAR]
                ,[TEST_KEY]
                ,[EXAM_ADMIN_DATE]
                ,[SUBJECT]
                ,[SCALE_SCORE]
          FROM [Assessment].[dbo].[TEST_STU_ACT]
          WHERE SCHOOL_YEAR >= 2008 and 
		            SCHOOL_YEAR <= 2012 and
                SCALE_SCORE is not null and
                SCALE_SCORE != 0
        "))

    act <- case.cols("act")
    names(act)[which(names(act) == "stunumb")] <- "id"
    
    # filter down to average scale score by kid

    actStu <- ddply(act[, c(1, 5:6)], c("id", "subject"), summarise, 
                     actSS = mean(scale_score))
        stopifnot(anyDuplicated(actStu[, 1:2])==0)
        actStu$actSS <- round(actStu$actSS)
          stopifnot(actStu$actSS >= 1 & actStu$actSS <= 36)


##################################
## get EOCT econ data
##################################

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
                SCHOOL_YR in ('2010', '2011', '2012') and
                TOTAL_SCALE_SCORE is not null and
                TOTAL_SCALE_SCORE != 0
        "))
    
    #close(ma_ch)

    # filter down to average scale score by kid

    econECT <- ddply(econECT[, c(5, 7)], "STUNUMB", summarise, 
                     econSS = mean(TOTAL_SCALE_SCORE))
        stopifnot(anyDuplicated(econECT$STUNUMB)==0)
        econECT$econSS <- round(econECT$econSS)
          stopifnot(econECT$econSS >= 200 & econECT$econSS <= 650)


##################################
## get GPA data
##################################

for (i in 1:length(cohortYear_shrt)) {

gpa <- sqlQuery(ma_ch, paste0(
      "SELECT * 
  		FROM	[Predictive_Analytics].[PAVIEW2].[v_Student_Course_History_DETAIL] 
			WHERE 	SchoolYear = ", cohortYear_shrt[i], " and 
					Grade in ('03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
      "))

    gpa <- case.cols("gpa")
    #gpa[grepl("Science", gpa$coresubjectcode), "coreind"] <- 1
  
  assign(paste0("gpa.", cohortYear_shrt[i]), gpa)

}

df <- get(paste0("gpa.", cohortYear_shrt[1]))
  for (j in 2:length(cohortYear_shrt)) {
    df2 <- get(paste0("gpa.", cohortYear_shrt[i]))
    df <- rbind(df, df2)
  }

gpa <- df

rm(df, df2, list = ls(pattern = "gpa."))

#format GPA
      
      
      # generate weighted core GPA
        gpa.core <- gpa[gpa$coreind == 1, ]
      
      # for 12th grade keep only 1st semester
        gpa.core.12th <- gpa.core[gpa.core$calendarmonth > 7 & gpa.core$grade == 12, ]
  

      # aggregate
        gc12.agg <- ddply(gpa.core.12th[, c("permnum", "schoolyear", "creditsattempted", 
                                            "creditweightedmark", "coresubjectcode")], 
                          c("permnum", "schoolyear", "coresubjectcode"), summarise, 
                          N = length(permnum),
                          ca = sum(creditsattempted),
                          cw = sum(creditweightedmark))

          gc12.aggm <- melt(gc12.agg[, c(1:3, 5:6)], id.vars = c(1:3))
          gc12.aggr <- dcast(gc12.aggm, permnum + schoolyear ~ coresubjectcode + variable)


            
            gc12.aggr$sem1.gpa.la <- round(gc12.aggr[, "LA_cw"] / gc12.aggr[, "LA_ca"], 1)
            gc12.aggr$sem1.gpa.ma <- round(gc12.aggr[, "MA_cw"] / gc12.aggr[, "MA_ca"], 1)
            gc12.aggr$sem1.gpa.sc <- round(gc12.aggr[, "SC_cw"] / gc12.aggr[, "SC_ca"], 1)
            gc12.aggr$sem1.gpa.ss <- round(gc12.aggr[, "SS_cw"] / gc12.aggr[, "SS_ca"], 1)
            gc12.aggr$sem1.gpa.core <- round(apply(gc12.aggr[, c("LA_cw", "MA_cw", "SC_cw", "SS_cw")], 
                                                   1, function(x) sum(x, na.rm = TRUE)) / 
                                             apply(gc12.aggr[, c("LA_ca", "MA_ca", "SC_ca", "SS_ca")], 
                                                   1, function(x) sum(x, na.rm = TRUE)), 1)
        
              gc12.aggf <- gc12.aggr[, c("permnum", "schoolyear", "sem1.gpa.la", "sem1.gpa.ma", 
                                         "sem1.gpa.sc", "sem1.gpa.ss", "sem1.gpa.core")]

  rm(gc12.agg, gc12.aggm, gc12.aggr, list=ls(pattern = "gpa"))
  gc()
  


##################################
## get SAT data
##################################

    sat <- sqlQuery(ma_ch, paste0(
      "   SELECT [STUNUMB]
  						  ,[TEST_KEY]
							  ,[EXAM_ADMIN_DATE]
							  ,[NONSTAND_IND]
							  ,[SUBJECT]
							  ,[SCORE]
						  FROM [Assessment].[dbo].[TEST_STU_SAT]
						  WHERE EXAM_ADMIN_DATE >= ", cohortYear_shrt[1], "0531 and
                    EXAM_ADMIN_DATE <= ", cohortYear_shrt[length(cohortYear_shrt)], "0531 and								
                    NONSTAND_IND = '' and
								    SUBJECT in ('MA', 'VE')
        "))
    
    close(ma_ch)
    sat <- case.cols("sat")

    # filter down to average scale score by kid

    sat <- ddply(sat[, c(1, 5:6)], c("stunumb", "subject"), summarise, 
                     satSS = mean(score))

        stopifnot(anyDuplicated(sat[, 1:2])==0)
        sat$satSS <- round(sat$satSS)
          stopifnot(sat$satSS >= 200 & sat$satSS <= 800)

        # restructure
        sat <- dcast(sat, stunumb ~ subject)

###########################
# load the graduation data
###########################

for (i in 2:3) {

fileLoc <- paste0(path,
                  "\\RBES\\Graduation Rate\\Cohort Graduation Rate Data\\ClassOfSY", cohortYear_shrt[i])




df <- read.csv(paste0("..\\RaisngAchClsngGap\\data\\prep\\DOECohortData_", startYear[i],  
                        "_jja.csv"), sep = ",", header = TRUE)

df <- case.cols("df")
  names(df)[40] <- "update.diploma.type"

  df <- df[df$grad.rate.type == 4 & df$school.id == "ALL" & 
              df$update.diploma.type %in% c("G", "C", "B", "V"), ]
















    


df <- merge(df, econECT, by.x = "id", by.y = "STUNUMB", all.x = TRUE)

# remove NAs
a.e <- as.data.frame(df[complete.cases(df[, c(2, 4, 8, 11:13)]), c(2, 4, 8, 11:13)])
  colnames(a.e) <- c("loc", "ELA.GPA", "eng.ACT", "school", "gr11", "econSS")
a.m <- as.data.frame(df[complete.cases(df[, c(2, 5, 9, 11:13)]), c(2, 5, 9, 11:13)])
  colnames(a.m) <- c("loc", "math.GPA", "math.ACT", "school", "gr11", "econSS")
a.r <- as.data.frame(df[complete.cases(df[, c(2, 4, 10, 11:13)]), c(2, 4, 10, 11:13)])
  colnames(a.r) <- c("loc", "ELA.GPA", "rdg.ACT", "school", "gr11", "econSS")
s.m <- as.data.frame(df[complete.cases(df[, c(2, 5, 6, 11:13)]), c(2, 5, 6, 11:13)])
  colnames(s.m) <- c("loc", "math.GPA", "math.SAT", "school", "gr11", "econSS")
s.v <- as.data.frame(df[complete.cases(df[, c(2, 4, 7, 11:13)]), c(2, 4, 7, 11:13)])
  colnames(s.v) <- c("loc", "ELA.GPA", "verbal.SAT", "school", "gr11", "econSS")


q.titles <- c("Mathematics: GPA and ACT\n(r = ",
              "E/LA: GPA and ACT\n(r = ",
              "E/LA GPA and Reading ACT\n(r = ",
              "Mathematics: GPA and SAT\n(r = ",
              "E/LA GPA and SAT Verbal\n(r = ")

q.objects <- cbind(c("aMath", "aEng", "aRD", "sMath", "sVerb"),
                   c("a.m", "a.e", "a.r", "s.m", "s.v"))

q.labels <- cbind(c("Mathematics GPA", 
                    "English/Language Arts GPA",
                    "English/Language Arts GPA",
                    "Mathematics GPA", 
                    "English/Language Arts GPA"), 
                  c("Mathematics ACT Score",
                    "English ACT Score", 
                    "Reading ACT Score",
                    "Mathematics SAT Score", 
                    "Verbal SAT Score"))



q <- cbind(q.titles, q.objects, q.labels)

    rm(q.titles, q.objects, q.labels)

######################################*

  schlTstGPA <- as.data.frame(matrix(rep(NA, 7), nrow = 1))
    colnames(schlTstGPA) <- c("N", "perc.11th", "prior.perf", "school", "test", "gpa", "r")
  
    df[, 11] <- lapply(df[, 11], as.character)
  
  modelGPA <- function(x, y) {  # y is location code
    model <- lm(x[, 3] ~ x[, 6], na.action = "na.omit", x)
    gpa <- round((line-summary(model)$coefficients[1, 1])/
                 summary(model)$coefficients[2, 1], 0)
    r <- round(cor(x[, 3], x[, 6]), 2)
    #assign(paste0("gpa.", q[i, 2], ".", y), gpa, envir = .GlobalEnv)
      newDF <- rbind(schlTstGPA, c(length(model$residuals), 
                                   round(length(model$residuals)/mean(x[, 5])*100, 1), 
                                   median(x[, 6]), paste0(unique(x[, 4])), q[i, 2], get("gpa"), 
                                   get("r")))
    assign("schlTstGPA", newDF, envir = .GlobalEnv)   
  }

  schls <- unique(df[, 2])

for (i in 1:5) {
  assign("df1", get(paste(q[i, 3], sep = "")))
  
  if (i %in% (4:5)) {
    line <- 520
  } else if (i == 3) {
    line <- 18
  } else {
    line <- 22
  }
  
  for (l in 1:length(schls)) {
    
    df2 <- df1[df1$loc == schls[l], ]
    
      if(length(complete.cases(df2)) >= 10) {
    
        modelGPA(df2, df2[1, 4])
      }
  }
}

schlTstGPA[, c(1:3, 6:7)] <- lapply(schlTstGPA[, c(1:3, 6:7)], as.numeric)


schlTstGPA <- schlTstGPA[schlTstGPA$N >= 20 & !is.na(schlTstGPA$N), ]
schlTstGPA <- schlTstGPA[order(schlTstGPA$test, schlTstGPA$gpa), ]



    write.table(schlTstGPA, 
                file = paste0("..//student.success.factor//data//metadata//", 
                              "equating//gpa_to_ACT_SAT_by_School.csv"), 
                sep = ",", 
                row.names = FALSE,
                col.names = TRUE)


###################################################################*
 schlTstECT <- as.data.frame(matrix(rep(NA, 6), nrow = 1))
    colnames(schlTstECT) <- c("N", "perc.11th", "school", "test", "eoct", "r")
  
  modelECT <- function(x, y) {  # y is location code
    model <- lm(x[, 3] ~ x[, 6], na.action = "na.omit", x)
    gpa <- round((line-summary(model)$coefficients[1, 1])/
                 summary(model)$coefficients[2, 1], 0)
    r <- round(cor(x[, 3], x[, 6]), 2)
    #assign(paste0("gpa.", q[i, 2], ".", y), gpa, envir = .GlobalEnv)
      newDF <- rbind(schlTstECT, c(length(model$residuals), 
                                   round(length(model$residuals)/mean(x[, 5])*100, 1), 
                                   paste0(unique(x[, 4])), q[i, 2], get("gpa"), 
                                   get("r")))
    assign("schlTstECT", newDF, envir = .GlobalEnv)   
  }

  schls <- unique(df[, 2])

for (i in 1:5) {
  assign("df1", get(paste(q[i, 3], sep = "")))
  
  if (i %in% (4:5)) {
    line <- 520
  } else if (i == 2) {
    line <- 18
  } else {
    line <- 22
  }
  
  for (l in 1:length(schls)) {
    
    df2 <- df1[df1$loc == schls[l], ]
    
      if(length(complete.cases(df2)) >= 10) {
    
        modelECT(df2, df2[1, 4])
      }
  }
}

schlTstECT[, c(1:2, 5:6)] <- lapply(schlTstECT[, c(1:2, 5:6)], as.numeric)


schlTstECT <- schlTstECT[schlTstECT$N >= 20 & !is.na(schlTstECT$N), ]
schlTstECT <- schlTstECT[order(schlTstECT$test, schlTstECT$eoct), ]



    write.table(schlTstECT, 
                file = paste0("..//student.success.factor//data//metadata//", 
                              "equating//eoct_to_ACT_SAT_by_School.csv"), 
                sep = ",", 
                row.names = FALSE,
                col.names = TRUE)




