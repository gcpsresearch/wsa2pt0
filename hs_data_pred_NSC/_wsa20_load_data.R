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
              "RODBC", "foreign","ggthemes", "grid", "gridExtra", "doParallel", 
              "AppliedPredictiveModeling", "caret", "gbm", "data.table")
lapply(packages, require, character.only=T)

#rm(list=ls()) 
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

startYear1       <- "2010-2011" 
startYear_shrt1  <- "2011"

startYear2       <- "2011-2012" # for 2012 grads
startYear_shrt2  <- "2012"

startYear3       <- "2012-2013" # for 2013 grads
startYear_shrt3  <- "2013"

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
            "ssfail_H1", "corefail_H1", "lafail_from8th_H1", "mafail_from8th_H1", "psr",
            "scfail_from8th_H1", "ssfail_from8th_H1", "corefail_from8th_H1", "gft_from8th_H1", 
            "retained_from04", "gft_H1", "gft_from8th_H1", "gft_H1", "sep_index_H1", "gini_index_H1",
            "schl_percSPED_H1", "schl_percESOL_H1", "schlEnr_H1", "schl_percWht_H1", "schlFRL_H1", 
            "schlAtt_H1", "schl_LAcrct", "schl_MAcrct", "schl_RDcrct", "schl_SCcrct", "spedCatMin_H1", 
            "spedCatMod_H1", "spedCatSev_H1", "pabs_H1", "percentEnrolledDays_H1", "female", "black", 
            "hispanic", "other", "frl_H1", "lep_H1", "repgrd_H1", "ss_totLAsq", "ss_totMAsq", "ss_totRDsq", 
            "ss_totSCsq", "schl_LAcrctsq", "schl_MAcrctsq", "schl_RDcrctsq", "schl_fg", "schl_fsl",
            "schl_sei_all", "schl_SCcrctsq", "p.e", "ap.t","ib.t",  "cum.gpa", "ps.t" )

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
             "ssfail_from8th_H1", "corefail_from8th_H1", "ap_ib_pass_H1", 
             "gft_H1", "sep_index_H1", "gini_index_H1", "psr",
             "ap_ib_pass_from8th_H1", "gft_from8th_H1", "psat_collReady", 
             "retained_from04", "female", "black", "hispanic", "other", 
             "frl_H1", "lep_H1", "repgrd_H1", "p.e",
             "ap.t","ib.t", "cum.gpa","ps.t")

disc <- c("dsevmx_H1", "dsevmn_H1", "drate_H1")

# convert factor variable to numeric
factorconvert <- function(f){as.numeric (levels (f))[f]}

# vplayout
vplayout <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
#==============================================================================
load("S:/Superintendent/Private/Strategy & Performance/ResearchEvaluation/Research Projects/RaisngAchClsngGap/data/prep/act_apib_gpa_sat_nsc.RData")

for (p in 11:11) {
  
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

  #create on-time, post-secondary-ready indicator (OTPSR.Grad)
  
  df$psr <- 0 
  
  # use best prior predictors if exist and convert to evalyr equivalents
  # remove unchangeable things (e.g., PSAT)
  if (exists("bal.f")) {
    
    bal.f.clr <- bal.f[(grep("dsev|drate|lacourse|macourse|pabs", 
                             names(bal.f)))] # keeping ones for which we hv E yr data
    
    
    names(bal.f.clr) <- gsub("CumulGPA_H1", "AnnualGPA_E", names(bal.f.clr))
    names(bal.f.clr) <- gsub("H1", "E", names(bal.f.clr))
    
  }
  
  for(z in 1:3) {
    if (exists(paste0("bal.ind.f.", z))) {
      
      ind <- get(paste0("bal.ind.f.", z))
      
      ind.clr <- ind[(grep("dsev|drate|lacourse|macourse|pabs", 
                           names(ind)))] # keeping ones for which we hv E yr data
      
      
      names(ind.clr) <- gsub("CumulGPA_H1", "AnnualGPA_E", 
                             names(ind.clr))
      names(ind.clr) <- gsub("H1", "E", 
                             names(ind.clr))
      
      assign(paste0("bal.ind.f.", z, ".clr"), ind.clr)
      
    }
  }
  
  if (p.grd == 11) {
# ontime Grad (includes summers via ceiling function) AND
df[df[, which(names(df)=="ontimeGrad")] & 
     
    # SAT M and CR >= 520 OR
    ((is.na(df[, which(names(df)=="scoreMA")]) & 
        df[, which(names(df)=="scoreMA")] >= 520 & 
        !is.na(df[, which(names(df)=="scoreVE")]) & 
        df[, which(names(df)=="scoreVE")] >= 520) |
       
       # ACT E >= 18 and M and R >= 22 OR
       (!is.na(df[, which(names(df)=="scoreaEN")]) & 
          df[, which(names(df)=="scoreaEN")] >= 18 & 
          !is.na(df[, which(names(df)=="scoreaMA")]) & 
          df[, which(names(df)=="scoreaMA")] >= 22 & 
          !is.na(df[, which(names(df)=="scoreaRD")]) & 
          df[, which(names(df)=="scoreaRD")] >= 22) |
       
       # evaluated year 1st semester GPA >= 83
       (!is.na(df[, which(names(df)=="lacourse1SemGPA_E")]) & 
          df[, which(names(df)=="lacourse1SemGPA_E")] >= 83 & 
          !is.na(df[, which(names(df)=="macourse1SemGPA_E")]) & 
          df[, which(names(df)=="macourse1SemGPA_E")] >= 83)),
        dim(df)[2]] <- 1

  } # END IF p.grd

if (p.grd != 11) {
  
  
  # maybe make conditions vector and just run cases against it?  the below is really getting complex!!
  
  bal.df <- do.call("rbind", replicate(dim(df)[1], bal.f.clr, simplify = FALSE))
  
  
  for (z in 1:3) {
    if (exists(paste0("bal.ind.f.", z))) {
      b <- get(paste0("bal.ind.f.", z))
      cbind(bal.df <- do.call("rbind", replicate(dim(df)[1], b, simplify = FALSE)))
    }
  }
  
  # add the ind ones, bind whole thing to df keeping only Vs in bals, diff based on length counts
  
  df <- cbind(df, bal.df)
  
  
  
  # BALANCED PROFILE OUTCOMES
  # end year in cohort subsequent grade AND
  df[df[, which(names(df)=="startyear_grade_N")] == p.grd + 2 & 
       
       # req 1 AND
       (!is.na(df[, which(names(df)==names(bal.f.clr)[1])]) & 
          df[, which(names(df)==names(bal.f.clr)[1])] >= as.numeric(bal.f[1])) & 
       
       if(length(bal.f.clr) >= 2) {
         # req 2 AND
         (!is.na(df[, which(names(df)==names(bal.f)[2])]) & 
            df[, which(names(df)==names(bal.f)[2])] >= as.numeric(bal.f[2])) 
       } &
       
       
       if(length(bal.f.clr) >= 3) {
         # req OR (
         (!is.na(df[, which(names(df)==names(bal.f)[3])]) & 
            df[, which(names(df)==names(bal.f)[3])] >= as.numeric(bal.f[3]))
       }
     , ] <- 1
  
  #-----------------------------#
  # INDIVIDUAL PROFILE OUTCOMES
  
  
  if (exists("bal.ind.f.1")) {
    # indiv profile req 1 OR
    (!is.na(df[, which(names(df)==names(bal.ind.f.1))]) & 
       df[, which(names(df)==names(bal.ind.f.1))] >= bal.f[1]) 
  } |
    
    if (exists("bal.ind.f.2")) {
      # indiv profile req 2 OR
      (!is.na(df[, which(names(df)==names(bal.ind.f.2))]) & 
         df[, which(names(df)==names(bal.ind.f.2))] >= bal.ind.f.2)
    } |
    
    if (exists("bal.ind.f.3")) {
      # indiv profile req 2 OR
      (!is.na(df[, which(names(df)==names(bal.ind.f.3))]) & 
         df[, which(names(df)==names(bal.ind.f.3))] >= bal.ind.f.3) 
    }
  
  dim(df)[2] <- 1


} # END IF p.grd != 11



# keep only variables for modeling
dfm <- df[, which(names(df) %in% c(keep, get(paste("g", p.grd, sep = ""))))]

# check only removed intended variables
names(df)[-which(names(df) %in% names(dfm))]

# subset numeric variables for graphing
nums <- names(dfm[, sapply(dfm, function(x) class(x) %in% 
                             c("numeric", "integer"))])


# check all kids enrolled in E year
stopifnot(min(df$daysenrolled_E) > 0)

# set NA for disciplinary incidents to 0
dfm[, which(names(dfm) %in% disc)][is.na(dfm[, which(names(dfm) %in% disc)]) 
                                   & dfm$percentEnrolledDays_H1 > 0] <- 0
# convert from incidents per day to per 90 days
dfm[, which(names(dfm) %in% c("drate_H1", "mob_H1"))] <- 
  dfm[, which(names(dfm) %in% c("drate_H1", "mob_H1"))]*90

if(p.grd > 9) {
  dfm[!is.na(dfm$psat_scoreCR) & dfm$psat_scoreCR == 0, 
      which(names(dfm) %in% c("psat_scoreCR", "psat_collReady"))] <- NA
  dfm[!is.na(dfm$psat_scoreMA) & dfm$psat_scoreMA == 0, 
      which(names(dfm) %in% c("psat_scoreMA", "psat_collReady"))] <- NA
  dfm[!is.na(dfm$psat_scoreWR) & dfm$psat_scoreWR == 0, 
      which(names(dfm) == "psat_scoreWR")] <- NA
}

#### LOAD DUAL ENROLLMENT DATA ####
####===========================####
ma_ch <- odbcConnect("ODS_Prod_MA", uid = "Research", pwd = "Research")

dual <- sqlQuery(ma_ch, paste0("
                               SELECT distinct t1.[SCHOOL_YEAR]
                               ,t2.[TITLE]
                               ,t1.[MARK_NUMERIC]
                               ,t1.[PERMNUM]
                               ,t1.[SCHOOL]
                               ,t2.[COURSE]
                               ,t2.[CREDVALUE]
                               FROM [GSDR].[GEMS].[SDRD_HIST] t1 LEFT JOIN [GSDR].[GEMS].[SASI_ACRS] t2 ON 
                               t1.COURSE = t2.COURSE
                               WHERE t1.SCHOOL_YEAR >= 2012 and
                               t2.LONGTITLE LIKE 'PS %'
                               "))
odbcClose(ma_ch)


dual <- case.cols("dual")
names(dual)[which(names(dual) == "permnum")] <- "id"
dual$mark.t <- dual$mark_numeric >=90

dual <- data.table(dual)

keycols  <- c("id", "school_year", "title")
dual.agg <- dual[,.(mean(mark_numeric), sum(mark.t), sum(credvalue)), by = "id,school_year,title"]
setnames(dual.agg,c("V1","V2","V3"), c("course_avg", "credits_earned", "credits_att"))
setkeyv(dual.agg, keycols)

dual.agg$ps <- dual.agg$credits_earned >=1
dual.agg.t <-  dual.agg[,.(mean(course_avg), sum(credits_att), sum(credits_earned), mean(ps)), by = "id,school_year"]
setnames(dual.agg.t,"V1", "cum_gpa")
dual.agg.t$ps.t <- dual.agg.t$V4 >0

dual.enr <- dual.agg.t[,c("id", "school_year", "cum_gpa", "ps.t"), with = FALSE]
dual.enr <- data.frame(dual.enr)


dfm <- merge(df, nsc.model, by = "id", all.x =  TRUE)

apib <- merge(ap.agg, ib.agg, by = "id", all.x = TRUE, all.y = TRUE)

dfm.m <- merge(dfm, apib, by = "id", all.x = TRUE)

dfm <- merge(dfm.m, dual.enr, by = "id", all.x = TRUE)
dfm <- data.frame(dfm)

# keep only variables for modeling
dfm <- dfm[, which(names(dfm) %in% c(keep, get(paste("g", p.grd, sep = ""))))]

# check only removed intended variables
names(dfm)[-which(names(dfm) %in% names(dfm))]

dfm$ap.t[is.na(dfm$ap.t)] <- FALSE
dfm$ib.t[is.na(dfm$ib.t)] <- FALSE

dfm$p.e[is.na(dfm$p.e)] <- FALSE

# subset numeric variables for graphing
nums <- names(dfm[, sapply(dfm, function(x) class(x) %in% 
                             c("numeric", "integer"))])

#save.image("H:/wsa2.0/data/prep/wsa2pt0_18mar2015.RData")

#=====================================================================================#

# check all kids enrolled in E year
stopifnot(min(df$daysenrolled_E) > 0)

# set NA for disciplinary incidents to 0
dfm[, which(names(dfm) %in% disc)][is.na(dfm[, which(names(dfm) %in% disc)]) 
                                   & dfm$percentEnrolledDays_H1 > 0] <- 0
# convert from incidents per day to per 90 days
dfm[, which(names(dfm) %in% c("drate_H1", "mob_H1"))] <- 
  dfm[, which(names(dfm) %in% c("drate_H1", "mob_H1"))]*90

if(p.grd > 9) {
  dfm[!is.na(dfm$psat_scoreCR) & dfm$psat_scoreCR == 0, 
      which(names(dfm) %in% c("psat_scoreCR", "psat_collReady"))] <- NA
  dfm[!is.na(dfm$psat_scoreMA) & dfm$psat_scoreMA == 0, 
      which(names(dfm) %in% c("psat_scoreMA", "psat_collReady"))] <- NA
  dfm[!is.na(dfm$psat_scoreWR) & dfm$psat_scoreWR == 0, 
      which(names(dfm) == "psat_scoreWR")] <- NA
}

#=========
# EXPLORE ####
#=========

# ADD CORR PLOT, ADD CHECK HISTOGRAMS AFTER TRANSFORMATION (MAYBE MAKE FUNCTION?)

## The correlation matrix of the new data
dfm.c <- dfm[, nums]
dfm.c <- dfm.c[, -c(nearZeroVar(dfm.c), which(names(dfm.c) == "id"))]
predCor <- cor(dfm.c, use = "pairwise.complete.obs")
library(corrplot)
pdf(paste(maindir, "\\results\\graphs\\", p.grd, "th", p.grd + 1, 
          "corrPlot.pdf", sep = ""), 
    width = 13, height = 15)
corrplot(predCor,
         order = "hclust", 
         type = "upper")
dev.off()

require(grid)
# examine univariate histograms; requires adjacent variables for looping
pdf(paste(maindir, "\\results\\graphs\\", p.grd, "th", p.grd + 1, 
          "thUnivHists.pdf", sep = ""), 
    width = 24, height = 18)

dfn <- dfm[, which(names(dfm) %in% nums[-which(nums %in% c("id", "loc_H1"))])]

# set variable range once by column #s or column names
start <-  1
vs <- dim(dfn)[2]

#         start <- which(names(df.n) == "dsevmx_H1")
#         vs <- which(names(df.n) == "coreCumulGPA_H1") - start + 2

for(i in 1:(ceiling(vs/7))) {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(min(7, vs), 4)))
  
  # scales to matrix location of variables for graphing
  range <- (min((i-1)*6+(start)):max((i-1)*6+(start + 5)))
  for(j in range) {
    
    # variable-specific removal of NAs
    d.h <- dfn[!is.na(dfn[, j]), ]
    
    sd <- sd(d.h[, j])
    
    bin <- min(ceiling(length(unique(d.h[, j]))/30), .2*sd)
    h2 <- ggplot(data = d.h, aes(d.h[, j]))
    h2 <- h2 + geom_histogram(binwidth = 2*bin) #colour = df.n$p.e,
    h2 <- h2 + xlab(paste(names(d.h)[j], ": binwidth = ", 
                          round(2*bin, 2), sep = ""))
    h2
    
    h1 <- ggplot(data = d.h, aes(d.h[, j]))
    h1 <- h1 + geom_histogram(binwidth = 1*bin) # colour = "white", 
    h1 <- h1 + xlab(paste(names(d.h)[j], ": binwidth = ", 
                          round(1*bin, 2), sep = ""))
    h1 
    
    h.5 <- ggplot(data = d.h, aes(d.h[, j]))
    h.5 <- h.5 + geom_histogram(binwidth = .5*bin) # colour = "white", 
    h.5 <- h.5 + xlab(paste(names(d.h)[j], ": binwidth = ", 
                            round(.5*bin, 2), sep = ""))
    h.5 
    
    h.1 <- ggplot(data = d.h, aes(d.h[, j]))
    h.1 <- h.1 + geom_histogram(binwidth = .1*bin) # colour = "white", 
    h.1 <- h.1 + xlab(paste(names(d.h)[j], ": binwidth = ", 
                            round(.1*bin, 2), sep = ""))
    h.1 
    
    
    # + 2 scales graphs to 2nd row to lv room for title
    k <- as.numeric((j - min(range) + 2))
    
    print(h2, vp = vplayout(k, 1))
    print(h1, vp = vplayout(k, 2))
    print(h.5, vp = vplayout(k, 3))
    print(h.1, vp = vplayout(k, 4))
    
  }
  grid.text(paste("WSA 2.0 SSI", p.grd, "th", p.grd + 1, 
                  "Predictor Distributions: ", 
                  "\n Shown with Varied Bin Widths \n Page: ", i, sep = ""), 
            vp = viewport(layout.pos.row = 1, layout.pos.col = 1:4, 
                          just = "centre"))
  
}



dev.off()

#=============================================
# preProcess //center, scale, impute missing ####
#=============================================
# later remember to preProcess w/ each cross-validation
# questions - how apply cv preProcessing to set for scoring?

set.seed(721)
split <- createDataPartition(dfm$p.e, p = .7, list = FALSE)

dfm$Class <- as.factor(ifelse(dfm$p.e == 1, "Yes", "No"))

train <- dfm[ split, ]
test  <- dfm[-split, ]

predVars <- names(dfm)[!(names(dfm) %in% c("id",  "p.e",
                                           "loc_H1", "Class", 
                                           "zoned_school_E", 
                                           "zoned_school_name_E"))]

# check distribution of missing values across rows
hist(apply(train[, predVars], 1, function(x) sum(is.na(x))))

# stop code if a row is entirely missing
stopifnot(dim(train[, predVars])[2] - 
            max(apply(train[, predVars], 1, function(x) sum(is.na(x)))) > 0)

trainX <-train[, predVars]
# remove if complete cases subset has constant variables
#rmv <- which(apply(trainX, 2, sd) == 0)
rmv <- which(apply(trainX, 2, function(x) sd(x, na.rm = TRUE)) == 0)

if(sum(rmv) == 0) {
  trainX$rmv <- 1
  rmv <- dim(trainX)[2]
}



preProcValues1 <- preProcess(trainX[, -rmv], 
                             method = c("center", "scale", "knnImpute"), 
                             k = 5, 
                             knnsummary = mean,
                             fudge = .1)

preProcValues2 <- preProcess(trainX[, -rmv], 
                             method = c("knnImpute"), 
                             k = 5, 
                             knnsummary = mean,
                             fudge = .01)

training <- predict(preProcValues1, (trainX[, -rmv]))
training80 <- predict(preProcValues2, (trainX[, -rmv]))
training <- cbind(training, train[, -which(names(train) %in% names(training))])
training80 <- cbind(training80, train[, -which(names(train) %in% names(training80))])
trainFinal <- cbind(training[, c(predVars, "Class")])
trainFinal80 <- cbind(training80[, c(predVars, "Class")])

# save datasets for glm after the loops
assign(paste0("trainFinal80.", p.grd, "th", p.grd + 1, "th"), trainFinal80)

#=========================
# post-process histograms ####
#=========================

require(grid)
# examine univariate histograms; requires adjacent variables for looping
pdf(paste(maindir, "\\results\\graphs\\", p.grd, "th", p.grd + 1, 
          "thUnivHistsPostProc.pdf", sep = ""), 
    width = 24, height = 18)

dfn <- training[, which(names(training) %in% nums[-which(nums %in% c("id", "loc_H1"))])]

# set variable range once by column #s or column names
start <-  1
vs <- dim(dfn)[2]

#         start <- which(names(df.n) == "dsevmx_H1")
#         vs <- which(names(df.n) == "coreCumulGPA_H1") - start + 2

for(i in 1:(ceiling(vs/7))) {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(min(7, vs), 4)))
  
  # scales to matrix location of variables for graphing
  range <- (min((i-1)*6+(start)):max((i-1)*6+(start + 5)))
  for(j in range) {
    
    # variable-specific removal of NAs
    d.h <- dfn[!is.na(dfn[, j]), ]
    
    sd <- sd(d.h[, j])
    
    bin <- min(ceiling(length(unique(d.h[, j]))/30), .2*sd)
    h2 <- ggplot(data = d.h, aes(d.h[, j]))
    h2 <- h2 + geom_histogram(binwidth = 2*bin) #colour = df.n$p.e,
    h2 <- h2 + xlab(paste(names(d.h)[j], ": binwidth = ", 
                          round(2*bin, 2), sep = ""))
    h2
    
    h1 <- ggplot(data = d.h, aes(d.h[, j]))
    h1 <- h1 + geom_histogram(binwidth = 1*bin) # colour = "white", 
    h1 <- h1 + xlab(paste(names(d.h)[j], ": binwidth = ", 
                          round(1*bin, 2), sep = ""))
    h1 
    
    h.5 <- ggplot(data = d.h, aes(d.h[, j]))
    h.5 <- h.5 + geom_histogram(binwidth = .5*bin) # colour = "white", 
    h.5 <- h.5 + xlab(paste(names(d.h)[j], ": binwidth = ", 
                            round(.5*bin, 2), sep = ""))
    h.5 
    
    h.1 <- ggplot(data = d.h, aes(d.h[, j]))
    h.1 <- h.1 + geom_histogram(binwidth = .1*bin) # colour = "white", 
    h.1 <- h.1 + xlab(paste(names(d.h)[j], ": binwidth = ", 
                            round(.1*bin, 2), sep = ""))
    h.1 
    
    
    # + 2 scales graphs to 2nd row to lv room for title
    k <- as.numeric((j - min(range) + 2))
    
    print(h2, vp = vplayout(k, 1))
    print(h1, vp = vplayout(k, 2))
    print(h.5, vp = vplayout(k, 3))
    print(h.1, vp = vplayout(k, 4))
    
  }
  grid.text(paste("WSA 2.0 SSI", p.grd, "th", p.grd + 1, 
                  "Predictor Distributions: ", 
                  "\n Shown with Varied Bin Widths \n Page: ", i, sep = ""), 
            vp = viewport(layout.pos.row = 1, layout.pos.col = 1:4, 
                          just = "centre"))
  
}



dev.off()
#####

testX <- test[, predVars]
if(dim(trainX)[2] == dim(trainFinal)[2]) {
  testX$rmv <- 1
}

testing <- predict(preProcValues1, testX[, -rmv])
testing <- cbind(testing, test[, -which(names(test) %in% names(testing))])
testFinal <- cbind(testing[, c(predVars, "Class")])
testFinal$p.e <- ifelse(testFinal$Class == "Yes", 1, 0)

# make nzv and nzv.corr datasets (all are cs)
# subset numeric variables
nums2 <- names(trainFinal[, sapply(trainFinal, function(x) class(x) %in% 
                                     c("numeric", "integer"))])

nums3 <- names(train[, sapply(train, function(x) class(x) %in% 
                                c("numeric", "integer"))])

# not enough variation so drop severe SPED cases
# use below line b/c spedCatSev_H1 is standardized at this point
std <- min(trainFinal$spedCatSev_H1)
nzv <- trainFinal[trainFinal$spedCatSev_H1 == std, nums2]
nzv <- nzv[, -(nearZeroVar(nzv))]
nzv.corr <- nzv[, -(findCorrelation(cor(nzv), cutoff = .85, verbose = FALSE))]

nzv <- cbind(nzv, trainFinal[trainFinal$spedCatSev_H1 == std, 
                             which(names(trainFinal) == "Class")])
names(nzv)[dim(nzv)[2]] <- "Class"

nzv.corr <- cbind(nzv.corr, trainFinal[trainFinal$spedCatSev_H1 == std, 
                                       which(names(trainFinal) == "Class")])
names(nzv.corr)[dim(nzv.corr)[2]] <- "Class"

# make same data frame for later outcome glm regressions

train.nzv.corr <- train[train$spedCatSev_H1 == 0, names(nzv.corr)]

train.nzv.corr <- cbind(train.nzv.corr, train[train$spedCatSev_H1 == 0, 
                                              which(names(train) == "Class")])

# add p.e so all dataframes are the same
nzv$p.e <- ifelse(nzv$Class == "Yes", 1, 0)
nzv.corr$p.e <- ifelse(nzv.corr$Class == "Yes", 1, 0)

#=============================================
# model, tune, test ####
#=============================================

# extract model characteristics from caret
caretModels <- modelLookup()

# select models to use
models    <- as.data.frame(c("gbm", "glm", "lda", "rf", "svmRadial"))
colnames(models) <- "model"
tuningPs  <- as.data.frame(merge(caretModels[, 1:2], models, by.x = "model", 
                                 by.y = "model", all.x = FALSE))
tuningPs$parameter <- as.character(tuningPs$parameter, stringsAsFactors = FALSE)
tuningPs[tuningPs$parameter == "parameter", 2] <- ""
tuningPs <- tuningPs[order(tuningPs$model, tuningPs$parameter), ]


print(tuningPs)

# datasets by model
data <- cbind(unique(tuningPs[, 1]), 
              c("trainFinal", "nzv.corr", "nzv.corr", 
                "trainFinal", "trainFinal"),
              c("Class", rep("p.e", 4)),
              c("p.e", rep("Class", 4)))


#========================================
# set tuning parameters (where needed) ####
#========================================
library(kernlab)

sigmaRange <- sigest(as.matrix(trainFinal[, -which(names(trainFinal) %in% c("Class", "p.e"))]))



userTunePs <- c("seq(1, 7, by = 2)", "seq(100, 1000, by = 50)", "c(0.01, 0.1)", # gbm
                "",                                                             # glm (none)
                #"c(0, .1, .2, .3, .4, .6, .8, 1)", "seq(.01, .2, length = 40)", # glmnet
                "",                                                             # lda (none)
                paste0("seq(2, ", dim(trainFinal)[2] - 1, ", ", 
                       "by = ", floor((dim(trainFinal)[2] - 1)/5), ")"),        # rf
                paste0(as.vector(sigmaRange[2])), "2^(-3:11)")                  # svm

#  userTunePs <- lapply(userTunePs, function(x) as.character(x))
#   print(userTunePs)

tuningPsSet <- cbind(tuningPs, userTunePs)
tuningPsSet$userTunePs <- as.character(tuningPsSet$userTunePs)

# filter down to only rows with tuning parameters
#     tuningPsSet <- tuningPsSet[tuningPsSet$parameter != "", ]
print(tuningPsSet)

ctrl <- trainControl(method = "cv",
                     number = 10, 
                     classProbs = TRUE,
                     allowParallel = TRUE,
                     summaryFunction = twoClassSummary)

trainFinal$p.e <- factor(ifelse(trainFinal$Class == "Yes", 1, 0),
                                levels = c(0, 1),
                                labels = c("No", "Yes"))

#================================
# Tuning the Model using train() ####
#================================

# iterate tuning process through models specified

# make grid if needed
for(i in 1: length(unique(tuningPsSet[, 1]))) {
  
  ps <- tuningPsSet[tuningPsSet$model == unique(tuningPsSet[, 1])[i], ]
  
  if(ps[1, 2] != "") {
    eg <- lapply(ps[, 3], function(x) eval(parse(text = x)))
    grid <- expand.grid(eg)
    names(grid) <- paste0(ps[, 2])
    assign(paste0("grid.", ps[1, 1]), grid)
  }
  
  ps <- rbind(ps, 1:3) # ensure all ps are matrices
  
  # model and tune if parameters exist
  trn <- get(data[i, 2])
  trn <- trn[, names(trn)[-which(names(trn) == data[i, 3])]]
  forModel <- trn
  preds <- forModel[, -which(names(forModel) == data[i, 4])]
  
  # Start the clock!
  ptm <- proc.time()
  
  if(ps[1, 2] == "") { 
    set.seed(721)
    mFull <- train(preds,
                   forModel[, which(names(forModel) == data[i, 4])],
                   method = as.character(ps[1,1]),
                   metric = "ROC",
                   #                preProc = c("center", "scale"), # not now b/c did b/f this
                   trControl = ctrl)
  } 
  
  if(ps[1, 2] != "") {
    
    set.seed(721)
    mFull <- train(preds,
                   forModel[, which(names(forModel) == data[i, 4])],
                   method = as.character(ps[1,1]),
                   metric = "ROC",
                   verbose = FALSE,
                   #                preProc = c("center", "scale"), # not now b/c did b/f this
                   tuneGrid = get(paste0("grid.", as.character(ps[1,1]))),
                   trControl = ctrl)
  }
  
  assign(paste0(ps[1,1], ".tuned.", p.grd, "th", p.grd + 1, "th"), mFull)
  
  # Stop the clock!
  time <- proc.time() - ptm
  assign(paste0(ps[1,1], ".time"), time)
  
  #==================
  # tuned parameters ####
  #==================
  
  if(ps[1, 2] != "")
    sink(file = paste0(maindir, "\\results\\", p.grd, "th", p.grd + 1, 
                       "th", as.character(ps[1, 1]), "Full", "Tuned.txt"),
         append = FALSE, split = TRUE)
  print(mFull)
  sink()
  
  #================================
  # Plot Methods for train Objects ####
  #================================
  
  if(ps[1, 2] != "") {
    
    pdf(file = paste0(maindir, "\\results\\graphs\\", p.grd, "th", p.grd + 1, 
                      "th", as.character(ps[1,1]), "Full", "Tuned.pdf"),  
        width = 8, height = 8)
    trellis.par.set(caretTheme())
    t <- plot(mFull, 
              metric = "ROC",
              auto.key = list(columns = 4, lines = TRUE))
    print(t)
    
  }
  if(ps[1, 2] != "") {
    dev.off()
  }
  
  #===============================================
  # Predicting New Samples and Confusion Matrices ####
  #===============================================
  tst <- testFinal[, which(names(testFinal) %in% names(get(data[i, 2])))]
  tst <- tst[, names(tst)[-which(names(tst) == "p.e")]]
  
  mPred <- predict(mFull, tst[, names(tst)[-which(names(tst) == "Class")]])
  
  sink(file = paste(maindir, "\\results\\", p.grd, "th", p.grd + 1, 
                    "th", as.character(ps[1,1]), "Full", "confusionMatrix.txt", sep = ""),
       append = FALSE, split = TRUE)
  Sys.time()
  cm <- confusionMatrix(mPred, tst$Class, positive = "Yes")
  print(cm)
  sink()
  
  #==================================================
  # Predicting Class Probabilities and the ROC Curve ####
  #==================================================
  
  
  mProbs <- predict(mFull, 
                    testFinal[, names(tst)[-which(names(tst) %in% c("p.e", 
                                                                    "Class"))]], 
                    type = "prob")
  head(mProbs, 3)
  
  final <- cbind(tst[, names(tst)[which(names(tst) == "Class")]], mProbs)
  names(final)[1] <- "Class"
  
  # save probs for later use in comparing ROC curves
  assign(paste0(ps[1,1], ".probs"), final)
  
  # check calibration of probabilities
  pdf(file = paste(maindir, "\\results\\graphs\\", p.grd, "th", p.grd + 1, 
                   "th", ps[1,1], "Calibration_mFull.pdf", sep = ""),
      width = 7, height = 7)
  calCurve <- calibration(Class ~ No, data = final)
  c <- xyplot(calCurve, auto.key = list(columns = 2), 
              main = paste0("Calibration of ", ps[1,1], " Model-based Probabilities: \nFailing to ", 
                            "Graduate OT-PSR Against True Probabilities \n Predicting ",
                            "from grade: ", p.grd, " to grade ", p.grd + 1))
  print(c)
  dev.off()
  
  # save calCurve for later use in determining deviation from 45 degrees
  assign(paste0(ps[1,1], ".calCurve"), calCurve)
  
  
  
  library(pROC)
  rocObject <- roc(predictor = mProbs$Yes,
                   response = tst$Class,
                   levels = rev(levels(tst$Class)))
  
  rocObject
  
  pdf(file = paste(maindir, "\\results\\graphs\\", p.grd, "th", p.grd + 1, 
                   "th", ps[1,1], "ROCclassProbs.pdf", sep = ""),
      width = 7, height = 7)
  plot(rocObject, print.thres = 0.50, 
       main = paste0(ps[1,1], " ROC Curve and .50 Threshold: ", 
                     "from grade ", p.grd, " to grade ", p.grd + 1, "\n", 
                     paste0("Area under the curve = ", round(rocObject$auc, 2)))
  ) 
  print(rocObject)
  dev.off()
  
  #===================================================
  # A Histogram of Class Probabilities by True Class ####
  #===================================================
  png(filename = paste(maindir, "\\results\\graphs\\", p.grd, "th", p.grd + 1, 
                       "th", ps[1,1], "ProbSuccess.png", sep = ""),
      width = 700, height = 700)
  h <- histogram(~mProbs$Yes|tst$Class,
                 xlab = paste0(ps[1,1], " Probability of Success in ", p.grd + 1, 
                               "th Grade"))
  print(h)
  dev.off()
  
  rm(mFull)
  gc()
  
}

#=================================================
# Examine ROC Curves and AUCs for all tuned models ####
#=================================================
require(ROCR)
# first ROC

pdf(file = paste(maindir, "\\results\\graphs\\", p.grd, "th", p.grd + 1, 
                 "th", "ROC_AUC_comparison.pdf", sep = ""),
    width = 8, height = 6)

j <- 1L
coords <- as.double(c(.80, .50))
cols <- c("green", "red", "blue", "gray", "black", "orange")

tuned <- get(paste0(data[j,1], ".tuned.", p.grd, "th", p.grd + 1, 
                    "th"))
aucs <- as.data.frame(matrix(ncol = 2, 
                             nrow = length(unique(tuningPsSet[, 1]))))
aucs[j, ] <- c(paste0(data[j, 1]), max(tuned$results$ROC))


pred <- prediction(get(paste0(data[j,1], ".probs"))[3], 
                   get(paste0(data[j,1], ".probs"))[1])
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf,  col = cols[j], 
     main = paste0(p.grd, "th Grade Predicting ", p.grd + 1, "th", 
                   "\nROC Curves and AUC for ", 
                   length(unique(tuningPsSet[, 1])), 
                   " Tuned Models"))
text(coords[[1]], coords[[2]] - (j - 1)*.05, 
     paste0(data[j,1], " (auc = ", 
            round(max(tuned$results$ROC), 2),
            ")"), 
     col = cols[j])

# add ROCs for other models
coords <- as.double(c(.80, .50))
cols <- c("green", "red", "blue", "gray", "black", "orange")
add <- c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)

for(j in 2: length(unique(tuningPsSet[, 1]))) {
  
  tuned <- get(paste0(data[j,1], ".tuned.", p.grd, "th", p.grd + 1, 
                      "th"))
  aucs[j, ] <- c(paste0(data[j, 1]), max(tuned$results$ROC))
  
  pred <- prediction(get(paste0(data[j,1], ".probs"))[3], 
                     get(paste0(data[j,1], ".probs"))[1])
  perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  plot(perf, add = add[j], col = cols[j])
  text(coords[[1]], coords[[2]] - (j - 1)*.05, 
       paste0(data[j,1], " (auc = ", 
              round(max(tuned$results$ROC), 2),
              ")"), 
       col = cols[j])
}

dev.off()

#=================================
# examine lift plot of all models ####
#=================================

# !!this section needs work!!

#   probs <- lapply(data[, 1], function(x) get(paste0(x, ".probs")))
#     liftCurve <- lift(Class ~ Yes, data = glm.probs[, 2])

#===================================
# check modeling times (in minutes) ####
#===================================

m.times <- matrix(data = NA, nrow = length(unique(tuningPsSet[, 1])), ncol = 3)
for(k in 1:length(unique(tuningPsSet[, 1]))) {
  m.times[k, ] <- get(paste0(data[k, 1], ".time"))[1:3]
}

sink(file = paste0(maindir, "\\results\\", p.grd, "th", p.grd + 1, 
                   "th modeling time.txt"),
     append = FALSE, split = TRUE)

# range of elapsed times
m.time.range <- summary(m.times[, 3]/60) 

# sum of elapsed times
m.time.sum <- sum(m.times[, 3]/60)

print(m.time.range)
print(m.time.sum)

sink()

#==========================================
# extract tuning parameters to use for rfe ####
#==========================================

# merge is based upon model and parameter

# filter out if AUC < .90
fs.models <- aucs[unique(tuningPsSet[, 1]) %in% aucs[aucs$V2 >= .75, 1], 1]
#     fs.models <- fs.models[which(fs.models != "gbm")] # b/c gbm is self-selecting

# make vector of only models w/ tuning parameters
dataTuned <- unique(tuningPsSet[tuningPsSet$parameter != "" &
                                  tuningPsSet$model %in% fs.models, 1])

# get final tuning parameters
tunes <- data.frame(t(rep(NA, 3)))
colnames(tunes) <- c("model", "parameter", "userTunePs")

for(k in 1:length(dataTuned)) {
  tuneObj <- get(paste0(dataTuned[k], ".tuned.", p.grd, "th", 
                        p.grd + 1, "th"))
  pars <- t(rbind(names(tuneObj$bestTune), tuneObj$bestTune))
  pars <- cbind(rep(dataTuned[k], dim(tuneObj$bestTune)[2]), pars)
  
  colnames(pars) <- c("model", "parameter", "userTunePs")
  tunes <- rbind(tunes, pars)
}

# remove 1st NA row
tunes <- tunes[complete.cases(tunes), ]



# #=============================================
# # feature selection ####
# #=============================================
# 
data2 <- data[data[, 1] %in% fs.models, ]

# This summary function is used to evaluate the models.
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))

# this needs to be more general if other models are used // !could make big table later
# need to also specify locations of most influential predictors
wrappers <- cbind(c("gbm", "glm", "lda", "rf"), 
                  c("caret", "lr", "lda", "rf"), 
                  c("Generalized Boosted Model", "Generalized Linear Model", 
                    "Linear Discriminant Analysis", "Random Forests"), 
                  c("RFE$optVariables", 
                    "names(RFE$fit$coefficients[-1])", 
                    "names(as.data.frame(RFE$fit$means))", 
                    "names(RFE$fit$forest$ncat)"))


# plot(cbind(2*(1:length(t))-1, t))

for (l in 2:length(fs.models)) {
  
  r.trn <- get(data2[l, 2])
  r.trn <- r.trn[, names(r.trn)[-which(names(r.trn) == data2[l, 3])]]
  r.forModel <- r.trn
  r.preds <- r.forModel[, -which(names(r.forModel) == data2[l, 4])]
  
  ## The candidate set of the number of predictors to evaluate
  varSeq <- seq(1, dim(r.preds)[2]-1, by = 2)
  
  set.seed(104)
  index <- createMultiFolds(r.forModel[, which(names(r.forModel) == 
                                                 data2[l, 4])],
                            times = 1)
  
  # for models w/ tuning parameters
  if (fs.models[l] %in% tunes[l, 1]) {
    
    rs <- tunes[tunes$model == fs.models[l], ]
    row.names(rs) <- NULL
    
    # make grid 
    
    rg <- lapply(rs[, 3], function(x) eval(parse(text = x)))
    rgrid <- expand.grid(rg)
    names(rgrid) <- paste0(rs[, 2])
    assign(paste0("rgrid.", rs[1, 1]), rgrid)
    
  }
  
  ctrl2 <- rfeControl(method = "repeatedcv", repeats = 5,
                      saveDetails = TRUE,
                      index = index,
                      returnResamp = "final")
  
  
  ctrl2$functions <- get(paste0(wrappers[l, 2], "Funcs"))
  ctrl2$functions$summary <- fiveStats
  ctrl2$verbose = FALSE
  set.seed(721)
  
  # Start the clock!
  ptm <- proc.time()
  
  dep <- as.data.frame(r.forModel[, which(names(r.forModel) == data2[l, 4])])
  names(dep) <- "class"
  
  # case when using caretFuncs and need to specify method
  if (wrappers[l, 2] == "caret") {
    
    RFE <- rfe(r.preds,
               r.forModel[, which(names(r.forModel) == data2[l, 4])],
               sizes = varSeq,
               metric = "ROC",
               method = wrappers[l, 1], 
               rfeControl = ctrl2,
               tuneGrid = get(paste0("rgrid.", as.character(rs[1,1]))))
  }
  
  # case when not using caretFuncs
  if (wrappers[l, 2] != "caret" & fs.models[l] %in% tunes[l, 1]) {
    
    RFE <- rfe(r.preds,
               r.forModel[, which(names(r.forModel) == data2[l, 4])],
               sizes = varSeq,
               metric = "ROC",
               rfeControl = ctrl2,
               tuneGrid = get(paste0("rgrid.", as.character(rs[1,1]))))
  }
  
  # for models w/o tuning parameters
  if (wrappers[l, 2] != "caret" & !(fs.models[l] %in% tunes[l, 1])) {
    
    ctrl2$functions <- get(paste0(wrappers[l, 2], "Funcs"))
    ctrl2$functions$summary <- fiveStats
    ctrl2$verbose = FALSE
    set.seed(721)
    
    # Start the clock!
    ptm <- proc.time()
    
    dep <- r.forModel[, which(names(r.forModel) == data2[l, 4])]
    
    RFE <- rfe(r.preds,
               r.forModel[, which(names(r.forModel) == data2[l, 4])],
               sizes = varSeq,
               metric = "ROC",
               rfeControl = ctrl2)
    
  }
  
  
  # save output and record time
  
  sink(file = paste0(maindir, "\\results\\", p.grd, "th", p.grd + 1, 
                     "th", wrappers[l, 2], "RFE.txt"),
       append = FALSE, split = TRUE)
  
  print(RFE)
  sink()
  
  
  # plot RFE results
  #-----------------*
  
  pdf(file = paste(maindir, "\\results\\graphs\\", p.grd, "th", p.grd + 1, 
                   "th", wrappers[l, 1], "RFE.pdf", sep = ""),
      width = 10, height = 7)
  
  rfePlot <- plot(RFE, 
                  main = paste0(wrappers[l, 3], " - Recursive Feature Elimination ", 
                                "Results\n ", p.grd, "th Grade Predicting ", p.grd + 1, 
                                "th Grade\nOn-Path, College/Career Readiness"),
                  sub = paste0(paste0("\nTop 5 of the best (", 
                                      length(eval(parse(text =  wrappers[l, 4]))), 
                                      ") variables are: "),
                               paste0("\n", eval(parse(text =  paste0(wrappers[l, 4], "[1]")))),
                               paste0("\n", eval(parse(text =  paste0(wrappers[l, 4], "[2]")))),
                               paste0("\n", eval(parse(text =  paste0(wrappers[l, 4], "[3]")))),
                               paste0("\n", eval(parse(text =  paste0(wrappers[l, 4], "[4]")))),
                               paste0("\n", eval(parse(text =  paste0(wrappers[l, 4], "[5]"))))
                  ),
  )
  
  print(rfePlot)
  
  dev.off()
  
  assign(paste0(wrappers[l, 2], "RFE"), RFE)
  assign(paste0(wrappers[l, 2], "RFE.", p.grd, "th", p.grd + 1, "th"), RFE)
  
  # Stop the clock!
  time <- proc.time() - ptm
  assign(paste0(wrappers[l,1], "RFE.time"), time)
  
  
}

#===================================
# check RFE times (in minutes) ####
#===================================

r.times <- matrix(data = NA, nrow = length(fs.models), ncol = 3)
for(m in 2:length(fs.models)) {
  r.times[m, ] <- get(paste0(wrappers[m, 1], "RFE.time"))[1:3]
}

sink(file = paste0(maindir, "\\results\\", p.grd, "th", p.grd + 1, 
                   "th RFE time.txt"),
     append = FALSE, split = TRUE)

# range of elapsed times
r.time.range <- summary(r.times[, 3]/60) 

# sum of elapsed times
r.time.sum <- sum(r.times[, 3]/60, na.rm = TRUE)

print(r.time.range)
print(r.time.sum)

sink()

#=============================================
# test GLM and extract outcome ####
#=============================================

if(max(lrRFE$results$ROC) >= .85) {
  
  stopifnot(length(lrRFE$fit$coefficients[-1]) >= 1)
  
  # plan: get dataset that excludes demographic and programmatic variables
  # believe these are indicated by integer variables with max == 1 (i.e., 0,1 variables)
  # merge these to the coefficients by variable names
  # build model with 3 most predictive values as indicated by the RFE
  # conduct the values needed for each of the three profiles:
  # balanced, slightly uneven, very uneven
  
  # get 3 most predictive variables
  lrCoeffs <- as.data.frame(lrRFE$fit$coefficients)
  lrCoeffs <- cbind(row.names(lrCoeffs), lrCoeffs)
  row.names(lrCoeffs) <- NULL
  names(lrCoeffs) <- c("variable", "coefficient")
  
  
  
  int <- train[, sapply(train, function(x) class(x) %in% c("integer"))]
  int <- int[, -which(unlist(lapply(names(int), function(x) 
    grepl('fail|psat', x, ignore.case = TRUE))))]
  rm2 <- train[, which(unlist(lapply(names(train), function(x) 
    grepl('schl_|_from|zoned|pct_|schlAtt_|schlEnr_|mob_H1|schlFRL|percentEnrolled|ss_', x, ignore.case = TRUE))))]
  
  glmData <- train[, -which(names(train) %in% c(names(int), names(rm2))), ]
  
  # see removed variables
  names(train[, -which(names(train) %in% names(glmData))])
  
  # keep only relevant indicators
  glmCoeffs <- lrCoeffs[which(lrCoeffs$variable %in% c("(Intercept)", 
                                                       names(glmData))),]
  
  # require at least 2 coefficients besides the intercept
  stopifnot((dim(glmCoeffs)[1] - 1) >= 2)
  
  
  glmData <- as.data.frame(train.nzv.corr[, names(train.nzv.corr) %in% 
                                            c(as.character(glmCoeffs[2:4, 1]), 
                                              "Class")])
  
  glmData <- glmData[complete.cases(glmData), ]
  
  
  
  
  # model their relationship to the outcome
  
  ctrl <- trainControl(method = "cv",
                       number = 10, 
                       classProbs = TRUE,
                       allowParallel = TRUE,
                       summaryFunction = twoClassSummary)
  
  # warning handling function
  warning_handling <- function(code) {
    
    tryCatch(code,
             warning = function(c) "warning")
  }
  
  # wrap train for glm in warning handling function
  rm(list=ls(pattern = "go1.coeffs"))
  counter.o <- NA
  warning_handling(
    for (n in 1:min(3, dim(glmData)[2] - 1)) {
      
      glm.out <- train(glmData[, 1:n],
                       glmData$Class, 
                       method = "glm", 
                       metric = "ROC", 
                       na.action = "na.omit", 
                       trControl = ctrl)
      
      
      
      # confirm predictive characteristics bf use
      stopifnot(glm.out$results$ROC >= .80)
      assign(paste0("glm.out.", n), glm.out)
      counter.o <- rbind(counter.o, n)
      
    }
  )
  
  # wrap train for glm in warning handling function
  rm(list=ls(pattern = "go1.coeffs"))
  counter.1o <- NA
  warning_handling(
    for (n in 1:min(3, dim(glmData)[2] - 1)) {
      
      glm.1out <- train(glmData[, n],
                        glmData$Class, 
                        method = "glm", 
                        metric = "ROC", 
                        na.action = "na.omit", 
                        trControl = ctrl)
      
      if (glm.1out$results$ROC >= .80) {
        assign(paste0("glm.1out.", n), glm.1out)
        go1.coeffs <- glm.1out$finalModel$coefficients
        assign(paste0("go1.coeffs.", n), go1.coeffs)
        counter.1o <- rbind(counter.1o, n)
      }
      
    }
  )
  
  # get coefficients to apply to means and SDs
  glmData <- glmData[!is.na(glmData), ]
  go.coeffs <- glm.out$finalModel$coefficients 
  
  
  # get dataset means and SDs
  go.mns <- apply(as.data.frame(glmData[complete.cases(glmData), 1:length(go.coeffs) - 1]), 2, function(x) 
    mean(x, na.rm = TRUE))
  
  go.sds <- apply(as.data.frame(glmData[!is.na(glmData), 
                                        1:length(go.coeffs) - 1]), 
                  2, function(x) 
                    
                    sd(x, na.rm = TRUE))
  
  go.mn_min_SD <- go.mns - go.sds
  
  # apply means and SDs to get profiles
  #=====================================*
  
  # balanced
  
  incr <- function(x, y) {
    
    seq(x, x + 2*y, by = 2*y/199)
    
  }
  
  go.mns.b <- mapply(incr, go.mns, go.sds)
  
  # set any dummy variables to best value
  for (d in 1:3) {
    if (class(glmData[, d]) == "integer" & 
          max(glmData[, d], na.rm = TRUE) == 1)
      go.mns.b[, d] <- 1
    
  }
  go.mns.b <- cbind(rep(1, 200), go.mns.b)
  
  # multiple coeffs by progression matrix and filter first above .75
  bal <- as.data.frame(cbind(go.mns.b, exp(go.mns.b %*% go.coeffs) / 
                               (1 + exp(go.mns.b %*% go.coeffs))))
  
  names(bal)[c(1, dim(bal)[2])] <- c("intercept", "probability")
  
  bal.f <- bal[bal$probability >= .66, ]
  bal.f <- bal.f[bal.f$probability == min(bal.f$probability), 
                 -(c(1, dim(bal.f)[2]))]
  
  assign(paste0("bal.f", p.grd, "th", p.grd + 1, "th"), bal.f)
  
  # multiple coeffs by progression matrix and filter first above .75
  bal <- as.data.frame(cbind(go.mns.b, exp(go.mns.b %*% go.coeffs) / 
                               (1 + exp(go.mns.b %*% go.coeffs))))
  
  names(bal)[c(1, dim(bal)[2])] <- c("intercept", "probability")
  
  bal.f <- bal[bal$probability >= .75, ]
  bal.f <- bal.f[bal.f$probability == min(bal.f$probability), 
                 -(c(1, dim(bal.f)[2]))]
  
  assign(paste0("bal.f", p.grd, "th", p.grd + 1, "th"), bal.f)
}


# loop to find values for individual predictors (if models are sufficient)
if(length(counter[complete.cases(counter)]) > 0) {
  
  for (o in counter[complete.cases(counter)]) {
    
    assign("coeffs", get(paste0("go1.coeffs.", o)))
    assign("mns", go.mns.b[, c(1, 1 + o)])
    
    bal.ind <- as.data.frame(cbind(mns, exp(mns %*% coeffs) / 
                                     (1 + exp(mns %*% coeffs))))
    
    names(bal.ind)[c(1, dim(bal.ind)[2])] <- c("intercept", "probability")
    bal.ind.f <- bal.ind[bal.ind$probability >= .75, ]
    bal.ind.f <- bal.ind.f[bal.ind.f$probability == min(bal.ind.f$probability), 
                           -(c(1, dim(bal.ind.f)[2]))]
    
    assign(paste0("bal.ind.f.", o), bal.ind.f)
    
    
  }
}










#           # one from mean
# 
#                 go.mns.m <- cbind(go.mns.b, rep(0, 200), rep(0, 200))
#                   go.mns.m <- go.mns.m[, 1:4]
# 
#                  for(n in 1:4) {
#                   
#                     go.mns.m[[, dim(go.mns.m)[2] + n]] <- go.mns.b[, n] * go.coeffs[n]
#                   }
#                   
#                 }
#                   
#                 go.mns.m.f <- mapply(incr1, go.mns.m, go.coeffs)
#                   
#                   
#                   
#                   len <- dim(go.mns.b)[2]
#                   
#                   for(n in 2:len) {
#                     
#                     inc.df <- cbind(x[, c(1, n)],  %*% go.coeffs[c(1, n)]
#                     
#                     
#                     
#                   
#                 }
# 
# 
#               for(n in 1:dim(go.mns.b)[2] - 1) {
#                 
#                                 
#                 
#               }
# 
#               first <- cbind(go.mns.b)
#             
#               mean1 <- as.data.frame(cbind(go.mns.b, 
#                                            exp(go.mns.b %*% go.coeffs) / 
#                                            (1 + exp(go.mns.b %*% go.coeffs)),
#                                            exp(go.mns.b %*% go.coeffs) / 
#                                            (1 + exp(go.mns.b %*% go.coeffs)),
#                                            exp(go.mns.b %*% go.coeffs) / 
#                                            (1 + exp(go.mns.b %*% go.coeffs)),))
# 
# 
# 
# 
# 
# 
# go.mns.f <- apply(as.data.frame(go.mns), 2, function(x) 
#               seq(x, x + 2*go.sds, by = 2*go.sds/199))
#             
#             go.mns.f <- cbind(rep(1, 200), go.mns.f)
#       
#                # multiple coeffs by progression matrix and filter first above .80
#               bal <- as.data.frame(cbind(go.mns.f, exp(go.mns.f %*% go.coeffs) / 
#                           (1 + exp(go.mns.f %*% go.coeffs))))
#             
#                 names(bal)[c(1, dim(bal)[2])] <- c("intercept", "probability")
#             
#                 bal.f <- bal[bal$probability >= .80, ]
#                 bal.f <- bal.f[bal.f$probability == min(bal.f$probability), 
#                                -(c(1, dim(bal.f)[2]))]
#             
#           # one from sd
#             
#             go.mns.f <- apply(as.data.frame(go.mns), 2, function(x) 
#               seq(x, x + 2*go.sds, by = 2*go.sds/199))
#             
#             go.mns.f <- cbind(rep(1, 200), go.mns.f)
#       
#                # multiple coeffs by progression matrix and filter first above .80
#               bal <- as.data.frame(cbind(go.mns.f, exp(go.mns.f %*% go.coeffs) / 
#                           (1 + exp(go.mns.f %*% go.coeffs))))
#             
#                 names(bal)[c(1, dim(bal)[2])] <- c("intercept", "probability")
#             
#                 bal.f <- bal[bal$probability >= .80, ]
#                 bal.f <- bal.f[bal.f$probability == min(bal.f$probability), 
#                                -(c(1, dim(bal.f)[2]))]
#   
#         
# 
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   lrCoeffs.data <- get(paste0(glmData[glmData$V1 == "glm", 2]))
#     lrCoeffs.data <- lrCoeffs.data[, which(names(get(data2[glmData$V1 == "glm", 2])) 
#                                    %in% names(lrCoeffs[2:4]))]
#   
#     lrCoeffs.mns <- c(1, apply(lrCoeffs.data[1:2], 2, mean), 
#                       max(lrCoeffs.data[3]))
#       lrCoeffs.mns_pSD <- c(0, apply(lrCoeffs.data[1:2], 2, sd), 
#                             max(lrCoeffs.data[3]))
#   
#         
#   
#     lrCoeffs.data <- cbind(rep(1, dim(lrCoeffs.data)[1]), lrCoeffs.data)
#   
#   
#   } 
#   
#                               
# 
#   
#   
#   # 3 profiles (very uneven, slightly uneven, balanced)
# 
# } 

#=============================================
# score ####
#=============================================

# steps: get object w/ deviations from bins; summarize and attach to approved models (those sent to rfe)
# get yes-probs only for models that are okay - also get actual outcomes
# aggregate to school likelihood and school actual percentage successful
# 

calDiffs <- as.data.frame(matrix(nrow = length(fs.models), ncol = 4))

for (z in 1:length(fs.models)) {
  
  calibr <- get(paste0(fs.models[z], ".calCurve"))
  
  calDiffs[z, 1] <- fs.models[z]
  calDiffs[z, 2] <- sum(abs(calibr$data$Percent - calibr$data$midpoint))
  calDiffs[z, 3] <- max(abs(calibr$data$Percent - calibr$data$midpoint))
  calDiffs[z, 4] <- abs(sum(calibr$data$Percent - calibr$data$midpoint))
  
  
}

# pick best models (lowest max (col 3) and lowest sum (col 2))

n <- length(fs.models)
minMax <- which(calDiffs[, 3] %in% c(min(calDiffs[, 3]), 
                                     sort(calDiffs[, 3],partial=2)[2]))

minSumAbs <- which(calDiffs[, 2] %in% c(min(calDiffs[, 2]), 
                                        sort(calDiffs[, 2],partial=2)[2], 
                                        sort(calDiffs[, 2],partial=3)[3]))


minAbsSum <- which(calDiffs[, 4] %in% c(min(calDiffs[, 4]), 
                                        sort(calDiffs[, 4],partial=n-1)[n-1]))

calMods <- minMax[which(minMax %in% c(minSumAbs, minAbsSum))]

if (is.na(calMods[1])) {
  
  calMods <- minMax[which(minMax %in% c(minAbsSum))]
  
  if (is.na(calMods[1])) {
    calMods <- which(calDiffs[, 2] %in% min(calDiffs[, 2]))
  }
  
}

# sink models used by grade
sink(file = paste0(maindir, "\\results\\calMods", p.grd, "th", 
                   p.grd + 1, "th.txt"), append = FALSE, split = TRUE)

print(fs.models[calMods])

sink()


# make final dataset for scoring

final <- testing[, which(names(testing) %in% c("id", "loc_H1", 
                                               "zoned_school_E", 
                                               "zoned_school_name_E", 
                                               "Class"))]
for (z in 1:length(calMods)) {
  auc <- aucs[aucs$V1 == fs.models[calMods[z]], 2]
  probs <- get(paste0(fs.models[calMods[z]], ".probs"))
  final <- cbind(final, as.numeric(as.character(rep(auc, nrow(final)))), 
                 probs)
}

class <- final$Class

final <- final[, -which(names(final) %in% c("Class", "No"))]
final <- cbind(final, class)

dimension <- dim(final)[2]

if(length(calMods) > 1) {
  # score each kid
  # get single score
  for(z in 1:length(calMods)) {
    final[, dim(final)[2]+1] <- 
      (final[, (z - 1)*2+5])*(final[, (z - 1)*2+6])
    
    
  }
  
  # calculate final weighted probability
  final[, dim(final)[2]+1] <- apply(final[, grep("V", 
                                                 names(final))], 1, 
                                    function(x) sum(x)/sum(final[1, grep("as.n", 
                                                                         names(final))]))
  names(final)[dim(final)[2]] <- "prob"
  
}  else {
  names(final)[dim(final)[2] - 1] <- "prob"
  
}
final <- final[, -(grep("V", names(final)))]
names(final)[grep("as.numeric", 
                  names(final))] 

# save copy of student scores
write.csv(final, 
          paste0(maindir, "\\results\\studentLevel_scoredProbs_", p.grd, "th", p.grd + 1, 
                 "th.csv"), 
          row.names = FALSE)

final <- final[, -(grep("as.numeric|Yes", names(final)))]

# aggregate by school
final$onPath <- ifelse(final$class == "Yes", 1, 0)
final.schl <- ddply(final, c("zoned_school_E", "zoned_school_name_E"), 
                    summarise, 
                    N = length(id), 
                    prob = mean(prob),
                    actual = mean(onPath))

# remove schools w/ too few students that won't be evaluated
final.schl <- final.schl[final.schl$N > 30, ]

# generate difference
final.schl$diff <- round((final.schl$actual - final.schl$prob)*100, 1)

write.csv(final.schl, 
          paste0(maindir, "\\results\\scoredProbs_", p.grd, "th", p.grd + 1, 
                 "th.csv"), 
          row.names = FALSE)

#=============================================
# test against F/R lunch; get % pts #####
#=============================================

# import lunch status // get from GADOE & merge w/ ODS queried loc code table


frpl12 <- read.csv(paste0("..\\student.success.factor\\data\\orig\\", 
                          "School ID File SY12(2-2011)- Pat.csv"), sep=",", 
                   header = TRUE)

frpl12 <- frpl12[, c(1, 11)]

final.mrg <- merge(final.schl, frpl12, by.x = "zoned_school_E", by.y = "loc_code", all.x = TRUE)

assign(paste0("final.mrg", p.grd, "th", p.grd + 1, "th"), final.mrg)

# plot %FRL & CCROTG %diff

jpeg(paste0("../student.success.factor/results/graphs/relToFRL_", p, "th", p + 1, "th.jpg"), 
     width = 800, height = 600)
plt <- ggplot(data = final.mrg, aes(final.mrg[, 7], final.mrg[, 6]))
plt <- plt + geom_point(size = 3, position = "Jitter", colour = "grey60")
plt <- plt + stat_smooth(span = 0.9, method = "loess", size = 1, colour = "black")
plt <- plt + stat_smooth(method = "lm", size = 1.5)
plt <- plt + ggtitle(paste0("CCROTG: ", p, "th to ", p + 1, "th Grade (r = ", 
                            round(cor(final.mrg[, 6], final.mrg[, 7]), 2), "; ", 
                            "R-squared = ", round(cor(final.mrg[, 6], 
                                                      final.mrg[, 7])**2, 2), ")"))
plt <- plt + ylab("Difference Score")
plt <- plt + xlab("% FRL")
plt <- plt + theme(plot.title = element_text(size = rel(2.5)), axis.text = 
                     element_text(size = rel(2)), 
                   axis.title = element_text(size = rel(2))) 
print(plt)

dev.off()

sys.time <- paste0(Sys.time())
sys.time <- gsub("-|:", "_", sys.time)


save.image(paste0("S:/Superintendent/Private/Strategy & Performance/", 
                  "ResearchEvaluation/RBES/WSA 2.0/student.success.factor/data/", 
                  "prep/", p.grd, "th", p.grd + 1, "th_", 
                  sys.time, ".RData"))
}
