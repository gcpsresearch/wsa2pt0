# Revising WSA work to incorporate NSC persistent enrollment as new outcome var
#
#
#   created on    2014.03.21 by James Appleton
#   last updated  2015.03.13 by Roland Richard

#========================#
# Setup/Load Packages ####
#========================#

####
packages <- c("plyr", "dplyr", "reshape", "reshape2", "ggplot2", "grid", "catspec",
              "RODBC", "foreign","ggthemes", "grid", "gridExtra", "doParallel")
lapply(packages, require, character.only=T)

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

#Setup Parallel processing ?
cl <- makeCluster(detectCores())
registerDoParallel(cl, cores = detectCores())

#============================================================#
# Load original predictive modeling data (w/o OTPSR var.) ####
#============================================================#
# set directories
setwd (paste(path,                        
             "\\RBES\\WSA 2.0\\student.success.factor",sep=""))
maindir <- paste(path,                        
                 "\\RBES\\WSA 2.0\\student.success.factor", sep = "")


dir ()

# variables to keep
keep <-   c("id", "dsevmx_H1", "dsevmn_H1", "drate_H1", "ss_totLA", "ss_totMA", 
            "ss_totRD", "ss_totSC", "mob_H1", "coreCumulGPA_H1", "lacourseCumulGPA_H1", 
            "macourseCumulGPA_H1", "fg", "fsl", "sei_all", "startyear_grade_E", "loc_H1", 
            "lafail_H1", "mafail_H1", "scfail_H1", "zoned_school_E", "zoned_school_name_E", 
            "ssfail_H1", "corefail_H1", "lafail_from8th_H1", "mafail_from8th_H1", 
            "scfail_from8th_H1", "ssfail_from8th_H1", "corefail_from8th_H1", "gft_from8th_H1", 
            "pct_16to19_wHSdiplOrEnr_H1", "pct_16PlusEmployed_H1", "pct_incToPovRatioLess1_H1", 
            "pct_incToPovRatioFrom1toLess2_H1", "pct_incToPovRatio2Plus_H1", 
            "pct_25andUp_wCollDegPlus_H1", "retained_from04", "gft_H1", "gft_from8th_H1", "gft_H1", 
            "schl_percSPED_H1", "schl_percESOL_H1", "schlEnr_H1", "schl_percWht_H1", "schlFRL_H1", 
            "schlAtt_H1", "schl_LAcrct", "schl_MAcrct", "schl_RDcrct", "schl_SCcrct", "spedCatMin_H1", 
            "spedCatMod_H1", "spedCatSev_H1", "pabs_H1", "percentEnrolledDays_H1", "female", "black", 
            "hispanic", "other", "frl_H1", "lep_H1", "repgrd_H1", "ss_totLAsq", "ss_totMAsq", "ss_totRDsq", 
            "ss_totSCsq", "schl_LAcrctsq", "schl_MAcrctsq", "schl_RDcrctsq", "schl_fg", "schl_fsl",
            "schl_sei_all", "schl_SCcrctsq", "i.t.", "p.e")

zoned <- c("zoned_school_E", "zoned_school_name_E")

g8 <-     ""
g9 <-     c("la", "ma", "sc", "ss", "percCore", "percOth", "la", "ma", "sc", "ss", 
            "percCore", "percOth", "ap_ib_pass_H1", "ap_ib_pass_from8th_H1")
g10 <-    c("psat_scoreCR", "psat_scoreMA", "psat_scoreWR", "psat_collReady", "la", 
            "ma", "sc", "ss", "percCore", "percOth", "ap_ib_pass_H1", 
            "ap_ib_pass_from8th_H1")
g11 <-    g10

factors <- c("spedCatMin_H1", "spedCatMod_H1", "spedCatSev_H1", "lafail_H1", 
             "loc_H1", "mafail_H1", "scfail_H1", "ssfail_H1", "corefail_H1", 
             "lafail_from8th_H1", "mafail_from8th_H1", "scfail_from8th_H1", 
             "ssfail_from8th_H1", "corefail_from8th_H1", "ap_ib_pass_H1", "gft_H1", 
             "ap_ib_pass_from8th_H1", "gft_from8th_H1", "psat_collReady", 
             "retained_from04", "female", "black", "hispanic", "other", 
             "frl_H1", "lep_H1", "repgrd_H1", , "i.t.", "p.e")

disc <- c("dsevmx_H1", "dsevmn_H1", "drate_H1")

# convert factor variable to numeric
factorconvert <- function(f){as.numeric (levels (f))[f]}

# vplayout
vplayout <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
#==============================================================================



for (p in 11:8) {
  
  #  for (yr in 2012:2013) {
  
  p.grd <- p # historic year grade
  
  # load and clean data
  df <- read.csv(paste0("..\\student.success.factor\\data\\prep\\", p.grd, "th", 
                        p.grd + 1, "th_model_only", ".csv"), sep=",", 
                 header = TRUE)
  
  # add polynomials
  for(i in c(which(names(df) %in% c("ss_totLA", "ss_totMA", "ss_totRD", 
                                    "ss_totSC", "schl_LAcrct", 
                                    "schl_MAcrct", "schl_RDcrct", 
                                    "schl_SCcrct")))) {
    
    df[[paste0(names(df)[i], "sq")]] <- (df[[i]])**2
    
  }
  


#=================================================#
### Load the NSC data ####
#=================================================#

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


