################################################################################
## PNOMICS 2022 ANALYSES - DATA IMPORT + CLEANING
################################################################################
## Author       : Taylor Curley
## Email        : tmc2737 "at" gmail.com
## Date         : 10/26/22
################################################################################
## Notes        : I pushed these operations to a different file to keep the main
##              : script clean. 
##              :
##              :
################################################################################

# Data import
oi_dat <- read.csv("./all_oi.csv")

# Filter out older adults
oi_dat <- subset(oi_dat, AgeGroup != "OA")

# Get rid of bad cases
oi_dat <- subset(oi_dat, Flag != 1)

# Ignore cases with 0% recall
all_recall <- data_summary(oi_dat, varname = "Recall_ACC", groupnames = c("Participant", "AgeGroup","Condition"))
good_cases <- all_recall$Participant[all_recall$Recall_ACC!=0.0]
oi_dat <- subset(oi_dat, oi_dat$Participant %in% good_cases)

# Only keep important columns
good_cols <- c("Participant","Condition","Category","Cue","Target","AltTarget1",
               "AltTarget2","AltTarget3","StudyTrial","Recall_ACC",
               "Recall_Err","RKN_RESP","FOK_RESP","RecallTrial","Recog_ACC",
               "RCJ_RESP","RecogTrial","Cycle","CycleTrial")
oi_dat <- subset(oi_dat, select = good_cols)

# Force to factor
oi_dat$Condition <- factor(oi_dat$Condition, levels = c("Related","Unrelated"))
oi_dat$Recall_Err <- as.factor(oi_dat$Recall_Err)
oi_dat$Participant <- as.factor(oi_dat$Participant)

# Put omission and comission rates into separate columns
oi_dat$Omission <- 0
oi_dat$Commission <- 0
oi_dat$Omission[oi_dat$Recall_Err=="Omm"] <- 1
oi_dat$Commission[oi_dat$Recall_Err=="Comm"] <- 1

# Put R/K/N rates into separate columns
oi_dat$R <- 0
oi_dat$K <- 0
oi_dat$N <- 0

oi_dat$R[oi_dat$RKN_RESP == 1] <- 1
oi_dat$K[oi_dat$RKN_RESP == 2] <- 1
oi_dat$N[oi_dat$RKN_RESP == 3] <- 1

oi_dat$RKN_GROUP <- NaN
oi_dat$RKN_GROUP[oi_dat$RKN_RESP == 1] <- "Remember"
oi_dat$RKN_GROUP[oi_dat$RKN_RESP == 2] <- "Know"
oi_dat$RKN_GROUP[oi_dat$RKN_RESP == 3] <- "No Memory"

oi_dat$RKN_GROUP <- as.factor(oi_dat$RKN_GROUP)
oi_dat$RKN_GROUP <- factor(oi_dat$RKN_GROUP, levels = c("Remember", "Know", "No Memory", "NaN"))
oi_dat$RKN_GROUP <- droplevels(oi_dat$RKN_GROUP, exclude = "NaN")

# Grand-mean centering
oi_dat$RCJ_gmcenter <- oi_dat$RCJ_RESP - mean(oi_dat$RCJ_RESP, na.rm = T)
oi_dat$FOK_gmcenter <- oi_dat$FOK_RESP - mean(oi_dat$FOK_RESP, na.rm = T)

# Person-mean centering
oi_dat$RCJ_personmean <- ave(oi_dat$RCJ_RESP, oi_dat$Participant, FUN = function(x) mean(x, na.rm = T))
oi_dat$RCJ_pcenter <- oi_dat$RCJ_RESP - oi_dat$RCJ_personmean
oi_dat$FOK_personmean <- ave(oi_dat$FOK_RESP, oi_dat$Participant, FUN = function(x) mean(x, na.rm = T))
oi_dat$FOK_pcenter <- oi_dat$FOK_RESP - oi_dat$FOK_personmean