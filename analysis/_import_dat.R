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

# Renumber Cycles and Cycle Trials (they are indexed at 0)
oi_dat$Cycle <- oi_dat$Cycle + 1
oi_dat$CycleTrial <- oi_dat$CycleTrial + 1

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

# Compute R/K/N cell means
r_means1 <- data_summary(oi_dat, varname = "R", groupnames = c("Participant","Condition"))
k_means1 <- data_summary(oi_dat, varname = "K", groupnames = c("Participant","Condition"))
n_means1 <- data_summary(oi_dat, varname = "N", groupnames = c("Participant","Condition"))

r_means1.2 <- melt(r_means1, id.vars = c("Participant","Condition","sd","se"), 
                   variable.name = "Rating", value.name = "Percent")
k_means1.2 <- melt(k_means1, id.vars = c("Participant","Condition","sd","se"), 
                   variable.name = "Rating", value.name = "Percent")
n_means1.2 <- melt(n_means1, id.vars = c("Participant","Condition","sd","se"), 
                   variable.name = "Rating", value.name = "Percent")

merge.1.1 <- merge(r_means1.2, k_means1.2, all = T)
merge.1.2 <- merge(merge.1.1, n_means1.2, all = T)
merge.1.2$Rating <- factor(merge.1.2$Rating, levels = c("R","K","N"))

r_means2 <- data_summary(r_means1, varname = "R", groupnames = c("Condition"))
k_means2 <- data_summary(k_means1, varname = "K", groupnames = c("Condition"))
n_means2 <- data_summary(n_means1, varname = "N", groupnames = c("Condition"))

r_means3 <- melt(r_means2, id.vars = c("Condition","sd","se"), variable.name = "Rating", value.name = "Percent")
k_means3 <- melt(k_means2, id.vars = c("Condition","sd","se"), variable.name = "Rating", value.name = "Percent")
n_means3 <- melt(n_means2, id.vars = c("Condition","sd","se"), variable.name = "Rating", value.name = "Percent")

# Merge RKN cell data
first_merge <- merge(r_means3,k_means3, all = T)
rkn_means_1 <- merge(first_merge, n_means3, all = T)
rkn_means_1$Rating <- factor(rkn_means_1$Rating, levels = c("R","K","N"))

# Calculate R/K/N means over trials
r_means4 <- data_summary(oi_dat, varname = "R", groupnames = c("Participant", "Condition","CycleTrial"))
k_means4 <- data_summary(oi_dat, varname = "K", groupnames = c("Participant", "Condition","CycleTrial"))
n_means4 <- data_summary(oi_dat, varname = "N", groupnames = c("Participant", "Condition","CycleTrial"))

r_means4.2 <- melt(r_means4, id.vars = c("Participant","Condition","CycleTrial","sd","se"), 
                   variable.name = "Rating", value.name = "Percent")
k_means4.2 <- melt(k_means4, id.vars = c("Participant","Condition","CycleTrial","sd","se"), 
                   variable.name = "Rating", value.name = "Percent")
n_means4.2 <- melt(n_means4, id.vars = c("Participant","Condition","CycleTrial","sd","se"), 
                   variable.name = "Rating", value.name = "Percent")

merge.4.1 <- merge(r_means4.2, k_means4.2, all = T)
merge.4.2 <- merge(merge.4.1, n_means4.2, all = T)
merge.4.2$Rating <- factor(merge.4.2$Rating, levels = c("R","K","N"))

r_means5 <- data_summary(oi_dat, varname = "R", groupnames = c("Condition","CycleTrial"))
k_means5 <- data_summary(oi_dat, varname = "K", groupnames = c("Condition","CycleTrial"))
n_means5 <- data_summary(oi_dat, varname = "N", groupnames = c("Condition","CycleTrial"))

r_means5.2 <- melt(r_means5, id.vars = c("Condition","CycleTrial","sd","se"), 
                   variable.name = "Rating", value.name = "Percent")
k_means5.2 <- melt(k_means5, id.vars = c("Condition","CycleTrial","sd","se"), 
                   variable.name = "Rating", value.name = "Percent")
n_means5.2 <- melt(n_means5, id.vars = c("Condition","CycleTrial","sd","se"), 
                   variable.name = "Rating", value.name = "Percent")

# Merge RKN means over trials
merge.5.1 <- merge(r_means5.2, k_means5.2, all = T)
rkn_means_2 <- merge(merge.5.1, n_means5.2, all = T)
rkn_means_2$Rating <- factor(rkn_means_2$Rating, levels = c("R","K","N"),
                             labels = c("Remember","Know","No Memory"))

# Extract unrecalled items
oi_dat_un <- subset(oi_dat, Recall_ACC == 0)