# Scores based upon 2011-12 model
#   created on    2015.04.28 by James Appleton

#========================#
# Setup/Load Packages ####
#========================#
{
rm(list = ls())
gc()
####
packages <- c("plyr", "dplyr", "reshape", "reshape2", "ggplot2", "grid", "catspec",
              "RODBC", "foreign","ggthemes", "grid", "gridExtra", "doParallel", 
              "AppliedPredictiveModeling", "caret", "gbm", "data.table", "ROCR", 
              "RANN", "e1071")

lapply(packages, require, character.only=T)

#rm(list=ls()) 
path <- readLines("c:\\current_path.txt")

# set directories
setwd (paste(path,                        
             "\\RBES\\WSA 2.0\\student.success.factor",sep=""))
maindir <- paste(path,                        
                 "\\RBES\\WSA 2.0\\student.success.factor", sep = "")
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

yrs <- c(2013)

# set date and time of most recent R workspace
dtTime <- "_2015_05_15 12_50_47"

# set sample size and cross-validations 
# proportion of sample to use
sz      <- 1 # 1 = 100%
# number of cross-validations (suggest 10 for big datasets)
  # add "repeated cross-validation" for smaller datasets
crossVs <- 10 #  

#Setup Parallel processing ?
cl <- makeCluster(detectCores())
registerDoParallel(cl, cores = detectCores())

# matrix to deal with post-secondary naming conventions
ps.m <- data.frame(cbind(1:13, c(1:12, "PS"), c(paste0(1:12, 
                                                       c("st", "nd", "rd", rep("th", 9))), 
                                                       "PostSec")), stringsAsFactors = FALSE)
  ps.m[, 1] <- as.numeric(ps.m[, 1])
}
#============================================================#
# Load built model workspace ####
#============================================================#
# set directories
setwd (paste(path,                        
             "\\RBES\\WSA 2.0\\student.success.factor",sep=""))



#==============================================================================

for (yr in yrs) {
for (p in 9) {
  
  p.grd <- p # historic year grade

#load  models for scoring
            e2 <- new.env()
            ws <- paste0(p.grd, "th", ps.m[p.grd + 1, 3], dtTime, ".RData")
            # convert needed .RData -> .rdb/.rdx
            e = local({load(paste0(maindir, "/data/prep/", ws))
                       ; environment()})
            tools:::makeLazyLoadDB(e, ws)
            lazyLoad(ws, envir = e2)
    lapply(c("tuningPsSet", "fs.models", "keep", "evYr", "zoned", 
             "g8", "g9", "g10", "g11", "g12", "factors", "disc",
             "preProcValues1", "preProcValues2", "rmv", "trainX", "data", 
			"mod.m", "run", "grds",
            paste0("gbm.tuned.", p.grd, "th", ps.m[p.grd + 1, 3]), 
            paste0("glm.tuned.", p.grd, "th", ps.m[p.grd + 1, 3]),
            paste0("lda.tuned.", p.grd, "th", ps.m[p.grd + 1, 3]),
            paste0("rf.tuned.", p.grd, "th", ps.m[p.grd + 1, 3]),
            paste0("svmRadial.tuned.", p.grd, "th", ps.m[p.grd + 1, 3])), 
           function(x) {
      assign(x, get(x, envir = e2), envir = .GlobalEnv)
    })
  rm(e, e2)
  gc()

# load ap, ib, dual data
            e2 <- new.env()
            ws <- "NSC_outcome.RData"
            # convert needed .RData -> .rdb/.rdx
            e = local({load(paste0(path, "/Research Projects/", 
                                   "RaisngAchClsngGap/data/prep/", ws))
                       ; environment()})
            tools:::makeLazyLoadDB(e, ws)
            lazyLoad(ws, envir = e2)
    lapply(c("nsc.model", "ap.agg", "ib.agg"), function(x) {
      assign(x, get(x, envir = e2), envir = .GlobalEnv)
    })
  rm(e, e2)
  gc()


  
  # load and clean data

  df <- read.csv(paste0("..\\student.success.factor\\data\\prep\\", p.grd, "th", 
                        p.grd + 1, "th_model_only_", yr, ".csv"), sep=",", 
                 header = TRUE)
			
	names(df)[which(names(df) %in% c("fg", "fsl", "sei_all"))] <- c("fg_H1", "fsl_H1", "sei_all_H1")
	
	df[complete.cases(df$pabs_H1) & df$pabs_H1 > 1, "pabs_H1"] <- NA
	df[complete.cases(df$pabs_E) & df$pabs_E > 1, "pabs_E"] <- NA
	df[complete.cases(df$punex_H1) & df$punex_H1 > 1, "punex_H1"] <- NA
	df[complete.cases(df$punex_E) & df$punex_E > 1, "punex_E"] <- NA
	
# set NA for disciplinary incidents in evaluated year to 0
disc2 <- c("dsevmx_E", "dsevmn_E", "drate_E")
  df[, which(names(df) %in% disc2)][is.na(df[, which(names(df) %in% disc2)]) 
                                     & df$daysenrolled_E > 0] <- 0
  # convert from incidents per day to per 90 days
  df[, which(names(df) %in% c("drate_E", "mob_E"))] <- 
    df[, which(names(df) %in% c("drate_E", "mob_E"))]*90
  
if(p.grd != max(grds)) {run <- ps.m[p.grd + 1, 3]}

  
names(df)[which(names(df) %in% c("la", "ma", "sc", "ss"))] <- c("la_credits", "ma_credits", "sc_credits", "ss_credits")
			
  if(p.grd >= 11) {
  #create post-secondary-ready indicator (psr)
         # SAT M and CR >= 520 OR
  df$psr <- 
  
      ((!is.na(df[, which(names(df)=="scoreMA")]) & 
          df[, which(names(df)=="scoreMA")] >= 520 & 
          !is.na(df[, which(names(df)=="scoreVE")]) & 
          df[, which(names(df)=="scoreVE")] >= 520) |
       
      # ACT E >= 18 and M and R >= 22 OR
     (!is.na(df[, which(names(df)=="scoreaEN")]) & 
          df[, which(names(df)=="scoreaEN")] >= 18 & 
      !is.na(df[, which(names(df)=="scoreaMA")]) & 
          df[, which(names(df)=="scoreaMA")] >= 22 & 
      !is.na(df[, which(names(df)=="scoreaRD")]) & 
          df[, which(names(df)=="scoreaRD")] >= 22))
  
    df$psr <- ifelse(df$psr == TRUE, 1, 0)
  }

  # use best prior predictors if exist and convert to evalyr equivalents
  # remove unchangeable things (e.g., PSAT)

  # this allows us to run a single grade level without looping
	bal.f <- read.csv(paste0("..\\student.success.factor\\data\\prep\\bal.f", p.grd, "th_", ps.m[p.grd + 1, 2], 
		"th.csv"), header=TRUE, sep = ",")
	go.coeffs <- as.numeric(unlist(read.table(paste0("..\\student.success.factor\\data\\prep\\go.coeffs", p.grd, "th_", ps.m[p.grd + 1, 3], 
	".csv"), row.names = 1, sep = ",")))
  # this portion is built for looping
  if (exists("bal.f")) {
    
    bal.f.clr <- bal.f[(grep("dsev|drate|lacourse|macourse|pabs|punex|pex|fg|fsl|sei_all", 
                             names(bal.f)))] # keeping ones for which we hv E yr data
    
    
    names(bal.f.clr) <- gsub("CumulGPA_H1", "AnnualGPA_E", names(bal.f.clr))
    names(bal.f.clr) <- gsub("H1", "E", names(bal.f.clr))
	
	bal.f.clr <- cbind(bal.f.clr, bal.f[,-(c(grep("H1", names(bal.f))))])
 		names(bal.f.clr)[-(grep("_E", names(bal.f.clr)))] <- 
			names(bal.f)[-(grep("H1", names(bal.f)))]
			
 # create target variable
if (p.grd == 11) {
	df$target <- FALSE
# ontime Grad (includes summers via ceiling function) AND

pre.dim <- dim(df)[2]
	
for(i in 1:dim(bal.f.clr)[2]) {

	if((go.coeffs[-1])[i] >= 0) {
	df[,dim(dfm)[2] + 1] <- 
	df$ontimeGrad == 1 & 
				!is.na(df[, names(bal.f.clr)[i]]) & 
				df[, names(bal.f.clr)[i]] >= round(bal.f.clr[,i], 2)
	} else if ((go.coeffs[-1])[i] <= 0) {
	df[,dim(df)[2] + 1] <- 
	df$ontimeGrad == 1 &
				!is.na(df[, names(bal.f.clr)[i]]) & 
				df[, names(bal.f.clr)[i]] <= round(bal.f.clr[,i], 2)
	}
}

#df1 <- df[, c(names(bal.f.clr), "ontimeGrad")]
df[, "target"] <- rowSums(df[, (pre.dim+1):dim(df)[2]]) == (dim(df)[2] - pre.dim) & df$ontimeGrad == 1
	df <- df[,1:pre.dim]

  } # END IF p.grd == 11

if (p.grd < 11 & p.grd >= 8 & p.grd < max(grds)) {
      df$target <- FALSE
      # end of year grade is p.grd + 2
      
      pre.dim <- dim(df)[2]
      
      for(i in 1:dim(bal.f.clr)[2]) {
        
        #requires less than threshold for negative coeffs and more than for positive coeffs
        if((go.coeffs[-1])[i] >= 0) {
          df[,dim(df)[2] + 1] <- 
            df$startyear_grade_N == p.grd + 2 & 
            !is.na(df[, names(bal.f.clr)[i]]) & 
            df[, names(bal.f.clr)[i]] >= round(bal.f.clr[,i], 2)
        } else if ((go.coeffs[-1])[i] <= 0) {
          df[,dim(df)[2] + 1] <- 
            df$startyear_grade_N == p.grd + 2 & 
            !is.na(df[, names(bal.f.clr)[i]]) & 
            df[, names(bal.f.clr)[i]] <= round(bal.f.clr[,i], 2)
        }
      }
      
      #df1 <- df[, c(names(bal.f.clr), "ontimeGrad")]
      df[, "target"] <- rowSums(df[, (pre.dim+1):dim(df)[2]]) == (dim(df)[2] - pre.dim) & df$startyear_grade_N == p.grd + 2
      df <- df[,1:pre.dim]
      
    } # END IF p.grd < 11 & p.grd >= 8 & p.grd < max(grds))
    
  }
			
			
# keep only variables for modeling
dfm <- df[, which(names(df) %in% c(keep, zoned,  
 	get(paste("g", p.grd, sep = ""))))]

# check only removed intended variables
names(df)[-(which(names(df) %in% names(dfm)))]

# check all kids enrolled in E year
if (p.grd != 12) {
  dfm <- df[df$daysenrolled_E > 0, which(names(df) %in% c(keep, zoned,
                                     get(paste("g", p.grd, sep = ""))))]
  }

if (p.grd == 12) {
  dfm <- df[, which(names(df) %in% c(keep, zoned,
                                     get(paste("g", p.grd, sep = ""))))]
  }

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
                               WHERE t1.SCHOOL_YEAR <= ", yr - 1, " and
                               t2.LONGTITLE LIKE 'PS %'
                               "))
odbcClose(ma_ch)


dual <- case.cols("dual")
names(dual)[which(names(dual) == "permnum")] <- "id"
dual$mark.t <- dual$mark_numeric >=90

#dual <- data.table(dual)

#keycols  <- c("id", "school_year", "title")
#dual.agg <- dual[,.(mean(mark_numeric), sum(mark.t), sum(credvalue)), by = "id,school_year,title"]
dual.agg <- ddply(dual, c("id", "school_year", "title"), summarise, 
			course_avg = mean(mark_numeric),
			credits_earned = sum(mark.t),
			credits_att = sum(credvalue))
#setnames(dual.agg,c("V1","V2","V3"), c("course_avg", "credits_earned", "credits_att"))
#setkeyv(dual.agg, keycols)

dual.agg$ps <- dual.agg$credits_earned >=1
#dual.agg.t <-  dual.agg[,.(mean(course_avg), sum(credits_att), sum(credits_earned), mean(ps)), by = "id,school_year"]
dual.agg.t <- ddply(dual.agg, c("id", "school_year"), summarise, 
				cum_gpa = mean(course_avg),
				credits_att = sum(credits_att),
				credits_earned = sum(credits_earned),
				ps = mean(ps))
 			
#setnames(dual.agg.t,"V1", "cum_gpa")
dual.agg.t$ps.t <- dual.agg.t$ps >0

dual.enr <- dual.agg.t[,c("id", "school_year", "cum_gpa", "ps.t")]
#dual.enr <- data.frame(dual.enr)

apib <- merge(ap.agg, ib.agg, by = "id", all.x = TRUE, all.y = TRUE)

dfm.m <- merge(dfm, apib, by = "id", all.x = TRUE)

dfm <- merge(dfm.m, dual.enr, by = "id", all.x = TRUE)
dfm <- data.frame(dfm)

if(length(dfm$ap.t)>0) dfm$ap.t[is.na(dfm$ap.t)] <- FALSE
if(length(dfm$ib.t)>0) dfm$ib.t[is.na(dfm$ib.t)] <- FALSE
if(length(dfm$ps.t)>0) dfm$ps.t[is.na(dfm$ps.t)] <- FALSE

if(p.grd >= 10) {
dfm$ap_ib_dual <- dfm$ap.t == TRUE | dfm$ib.t == TRUE | dfm$ps.t == TRUE
}

# keep only variables for modeling
dfm <- dfm[, which(names(dfm) %in% c(keep, zoned, get(paste("g", p.grd, sep = ""))))]

# check only removed intended variables
names(df)[-which(names(df) %in% names(dfm))]





# assumes worst case if we don't know
dfm$target[is.na(dfm$target)] <- FALSE
	dfm$target <- ifelse(dfm$target == TRUE, 1, 0)


#=====================================================================================#

# check all kids enrolled in E year
if (p.grd != 12) {
  dfm <- df[df$daysenrolled_E > 0, which(names(df) %in% c(keep, zoned,
                                     get(paste("g", p.grd, sep = ""))))]
  }

if (p.grd == 12) {
  dfm <- df[, which(names(df) %in% c(keep, zoned,
                                     get(paste("g", p.grd, sep = ""))))]
  }

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



# drop all variables with 60% or more missing data
	rem <- dfm[ lapply( dfm, function(x) sum(is.na(x)) / length(x) ) > 0.60 ]
	if(length(rem) > 0) {
	rem2 <- lapply(names(rem), function(x) gsub("ss_", "schl_", x))
	dfm <- dfm[, -(which(names(dfm) %in% c(names(rem), unlist(rem2))))]
	}

# add polynomials
  for(i in c(which(names(dfm) %in% c("ss_totLA", "ss_totMA", "ss_totRD", 
                                    "ss_totSC", "schl_totLA", 
                                    "schl_totMA", "schl_totRD", 
                                    "schl_totSC")))) {
    
    dfm[[paste0(names(dfm)[i], "sq")]] <- (dfm[[i]])**2
      }

# subset numeric variables for graphing
nums <- names(dfm[complete.cases(dfm), sapply(dfm, function(x) class(x) %in% 
                             c("numeric", "integer", "logical") & 
                             sum(is.na(x))/length(x) < .34)])

if(p.grd != 12) {
	# save percentage meeting target						
	  sink(file = paste(maindir, "\\results\\ScoreOnly.perc.mtg.target_", p.grd, 
		"th_", ps.m[p.grd + 1, 3], ".txt", sep = ""),
		   append = FALSE, split = TRUE)
	  tbl <- table(df$target)
	  p.tbl <- prop.table(tbl)
	  if(p.grd == 11) {cat("Students must be on-time (4-year) graduates.\n ")}
      print(bal.f.clr)
	  print(tbl)
	  print(round(p.tbl*100, 1))
	  sink()
	# create percentage meeting target object
		assign(paste0("perc.mtg.target_", p.grd, ps.m[p.grd + 1, 3]), tbl)
}



save.image(paste0(maindir, "/data/prep/", p.grd, ps.m[p.grd + 1, 3], "wsa2pt0_ScoreOnly.RData"))


if (sz < 1) {
set.seed(721)
red <- createDataPartition(dfm$target, p = sz, list = FALSE)
dfm <- dfm[red,]
}



#=========
# EXPLORE ####
#=========

# ADD CORR PLOT, ADD CHECK HISTOGRAMS AFTER TRANSFORMATION (MAYBE MAKE FUNCTION?)

## The correlation matrix of the new data
dfm.c <- dfm[, nums]

dfm.c <- dfm.c[, -c(nearZeroVar(dfm.c), which(names(dfm.c) == "id"), 
                    grep("loc|school", 
                             names(dfm)))]
predCor <- cor(dfm.c, use = "pairwise.complete.obs")
library(corrplot)
pdf(paste0(maindir, "\\results\\graphs\\", p.grd, "th", ps.m[p.grd + 1, 3], 
          "_corrPlotScoreOnly.pdf"), 
    width = 17, height = 20)
corrplot(predCor,
         order = "hclust", 
         type = "upper")
dev.off()


#=============================================
# preProcess //center, scale, impute missing ####
#=============================================
# later remember to preProcess w/ each cross-validation

dfm$Class <- as.factor(ifelse(dfm$target == 1, "Yes", "No"))
test  <- dfm

predVars <- names(dfm)[!(names(dfm) %in% c("id",  "target",
                                           "loc_H1", "Class", 
                                           "zoned_school_E", 
                                           "zoned_school_name_E"))]


# check distribution of missing values across rows
hist(apply(test[, predVars], 1, function(x) sum(is.na(x))))

testX <- test[, predVars]

testing <- predict(preProcValues1, testX[, -(which(names(testX) %in% names(trainX)[rmv]))])
testing <- cbind(testing, test[, -which(names(test) %in% names(testing))])
testFinal <- cbind(testing[, c(predVars, "Class")])
testFinal$target <- ifelse(testFinal$Class == "Yes", 1, 0)

  #===============================================
  # Predicting New Samples and Confusion Matrices ####
  #===============================================
for(i in 1:length(unique(tuningPsSet[, 1]))) {
	
	ps <- tuningPsSet[tuningPsSet$model == unique(tuningPsSet[, 1])[i], ]
	assign("mFull", get(paste0(ps[1,1], ".tuned.", p.grd, "th", ps.m[p.grd + 1, 3])))
  tst <- testFinal[, which(names(testFinal) %in% names(get(data[i, 2])))]
  tst <- tst[, names(tst)[-which(names(tst) == "target")]]
  
  mPred <- predict(mFull, tst[, names(tst)[-which(names(tst) == "Class")]])
  
  sink(file = paste(maindir, "\\results\\", p.grd, "th_", ps.m[p.grd + 1, 3],
                    "_", as.character(ps[1,1]), "_Full", "confusionMatrix_ScoredOnly.txt", sep = ""),
       append = FALSE, split = TRUE)
  cm <- confusionMatrix(mPred, tst$Class, positive = "Yes")
  print(cm)
  sink()
  
  #==================================================
  # Predicting Class Probabilities and the ROC Curve ####
  #==================================================
  mProbs <- predict(mFull, 
                    testFinal[, names(tst)[-which(names(tst) %in% c("target", 
                                                                    "Class"))]], 
                    type = "prob")
  head(mProbs, 3)
  
  final <- cbind(tst[, names(tst)[which(names(tst) == "Class")]], mProbs)
  names(final)[1] <- "Class"
  
  # save probs for later use in comparing ROC curves
  assign(paste0(ps[1,1], ".probs"), final)
  
  # check calibration of probabilities
  pdf(file = paste(maindir, "\\results\\graphs\\", p.grd, "th_", ps.m[p.grd + 1, 3], 
                   "_", ps[1,1], "_Calibration_mFullScoredOnly.pdf", sep = ""),
      width = 7, height = 7)
  calCurve <- calibration(Class ~ No, data = final)
  c <- xyplot(calCurve, auto.key = list(columns = 2), 
              main = paste0("Scored Only - \n Calibration of ", ps[1,1], " Model-based Probabilities: \nFailing to ", 
                            "Persist in Post-Secondary Against True Probabilities \n Predicting ",
                            "from grade: ", p.grd, " to grade ", ps.m[p.grd + 1, 2]))
  print(c)
  dev.off()
  
  # save calCurve for later use in determining deviation from 45 degrees
  assign(paste0(ps[1,1], "ScoredOnly.calCurve"), calCurve)
  
  
  
  rocObject <- roc(predictor = mProbs$Yes,
                   response = tst$Class,
                   levels = rev(levels(tst$Class)))
  
  rocObject
  
  pdf(file = paste(maindir, "\\results\\graphs\\", p.grd, "th_", ps.m[p.grd + 1, 3],
                    "_", ps[1,1], "_ROCclassProbsScoredOnly.pdf", sep = ""),
      width = 7, height = 7)
  plot(rocObject, print.thres = 0.50, 
       main = paste0(ps[1,1], " ROC Curve and .50 Threshold: ", 
                     "from grade ", p.grd, " to grade ", ps.m[p.grd + 1, 2], "\n", 
                     paste0("Area under the curve = ", round(rocObject$auc, 2)))
  ) 
  print(rocObject)
  dev.off()
  
  #===================================================
  # A Histogram of Class Probabilities by True Class ####
  #===================================================
  png(filename = paste(maindir, "\\results\\graphs\\", p.grd, "th_", ps.m[p.grd + 1, 3], 
                       "_", ps[1,1], "_ProbSuccessScoredOnly.png", sep = ""),
      width = 700, height = 700)
  h <- histogram(~mProbs$Yes|tst$Class,
                 xlab = paste0(ps[1,1], " Probability of Success in ", ps.m[p.grd + 1, 3], 
                               " Grade"))
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

pdf(file = paste(maindir, "\\results\\graphs\\", p.grd, "th_", ps.m[p.grd + 1, 3], 
                 "_ROC_AUC_comparisonScoredOnly.pdf", sep = ""),
    width = 8, height = 6)

j <- 1L
coords <- as.double(c(.80, .50))
cols <- c("green", "red", "blue", "gray", "black", "orange")

tuned <- get(paste0(data[j,1], ".tuned.", p.grd, "th", ps.m[p.grd + 1, 3]))
aucs <- as.data.frame(matrix(ncol = 2, 
                             nrow = length(unique(tuningPsSet[, 1]))))
aucs[j, ] <- c(paste0(data[j, 1]), max(tuned$results$ROC))


pred <- prediction(get(paste0(data[j,1], ".probs"))[3], 
                   get(paste0(data[j,1], ".probs"))[1])
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf,  col = cols[j], 
     main = paste0("Scored Only - \n", p.grd, "th Grade Predicting ", ps.m[p.grd + 1, 3], 
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
  
  tuned <- get(paste0(data[j,1], ".tuned.", p.grd, "th", ps.m[p.grd + 1, 3]))
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

#=============================================
# score ####
#=============================================

# steps: get object w/ deviations from bins; summarize and attach to approved models (those sent to rfe)
# get yes-probs only for models that are okay - also get actual outcomes
# aggregate to school likelihood and school actual percentage successful
# 

calDiffs <- as.data.frame(matrix(nrow = length(fs.models), ncol = 4))

for (z in 1:length(fs.models)) {
  
  calibr <- get(paste0(fs.models[z], "ScoredOnly.calCurve"))
  
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
sink(file = paste0(maindir, "\\results\\calMods_", p.grd, "th_", 
                   ps.m[p.grd + 1, 3], "ScoredOnly.txt"), append = FALSE, split = TRUE)

print(fs.models[calMods])

sink()


# make final dataset for scoring
if (p.grd != 12) {
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
  if (p.grd == 12) {
    v <- 3
  } else {
    v <- 5
  }
  for(z in 1:length(calMods)) {
    final[, dim(final)[2]+1] <- 
      (final[, (z - 1)*2+v])*(final[, (z - 1)*2+v+1])
    
    
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
          paste0(maindir, "\\results\\stu_scoredProbs_", p.grd, "th_", ps.m[p.grd + 1, 3], 
                 "ScoredOnly.csv"), 
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
          paste0(maindir, "\\results\\scoredProbs_", p.grd, "th_", ps.m[p.grd + 1, 3], 
                 "ScoredOnly.csv"), 
          row.names = FALSE)

#=============================================
# test against F/R lunch; get % pts #####
#=============================================

# import lunch status // get from GADOE & merge w/ ODS queried loc code table


  
	
	
# ma_ch <- odbcConnect("ODS_Prod_MA", uid = "Research", pwd = "Research")
# 	  
# schIds <- sqlQuery(ma_ch, paste0("
# 									SELECT distinct [LOC]
# 													,[SCH_CODE]
# 									FROM [GSDR].[GEMS].[SDR_SCHOOL_B] 
# 									WHERE SCH_YR = 2011"))
#   odbcClose(ma_ch)
# 
#   schIds <- case.cols("schIds")
#   schIds$sch_code <- 667*10000 + as.numeric(schIds$sch_code)



frpl12 <- read.csv(paste0("..\\student.success.factor\\data\\orig\\", 
                          "School ID File SY12(2-2011)- Pat.csv"), sep=",", 
                   header = TRUE)

frpl12 <- frpl12[, c(1, 11)]

final.mrg <- merge(final.schl, frpl12, by.x = "zoned_school_E", by.y = "loc_code", all.x = TRUE)
		final.mrg <- final.mrg[final.mrg$zoned_school_E != 631, ]
		
assign(paste0("final.mrg", p.grd, "th_", ps.m[p.grd + 1, 3]), final.mrg)

# plot %FRL & CCROTG %diff

jpeg(paste0("../student.success.factor/results/graphs/relToFRL_", p, "th_", ps.m[p.grd + 1, 3], "ScoredOnly.jpg"), 
     width = 800, height = 600)
plt <- ggplot(data = final.mrg, aes(final.mrg[, 7], final.mrg[, 6]))
plt <- plt + geom_point(size = 3, position = "Jitter", colour = "grey60")
plt <- plt + stat_smooth(span = 0.9, method = "loess", size = 1, colour = "black")
plt <- plt + stat_smooth(method = "lm", size = 1.5)
plt <- plt + ggtitle(paste0("Scored Only - \n CCRG: ", p, "th to ", ps.m[p.grd + 1, 3], " Grade (r = ", 
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

  }


sys.time <- paste0(Sys.time())
sys.time <- gsub("-|:", "_", sys.time)

save.image(paste0("S:/Superintendent/Private/Strategy & Performance/", 
                  "ResearchEvaluation/RBES/WSA 2.0/student.success.factor/data/", 
                  "prep/scored", p.grd, ps.m[p.grd + 1, 3], ".RData"))
  }
}
