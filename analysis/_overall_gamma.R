################################################################################
## PNOMICS 22 ANALYSES - OVERALL GAMMA CALCULATIONS
################################################################################
## Author       : Taylor Curley
## Email        : tmc2737 "at" gmail.com
## Date         : 10/26/22
################################################################################
## Notes        : These are Goodman-Kruskal gamma correlations for all items. 
##              : The code is pushed to a separate file to keep the main script
##              : tidy.
##              :
##              :
################################################################################

# Instantiate data frame
gamma_dat <- data.frame(Participant = numeric(),
                        Condition = character(),
                        FOK_Gamma = numeric(),
                        FOK_Gamma_Unrecall = numeric(),
                        CJ_Gamma = numeric(),
                        CJ_Gamma_Unrecall = numeric(),
                        FOK_Recall_Gamma = numeric(),
                        FOK_CJ_Gamma = numeric()
)

# Cycle through all participants
for (sub in unique(oi_dat$Participant)){
  sub_dat <- subset(oi_dat, Participant == sub)
  sub_dat_un <- subset(sub_dat, Recall_ACC == 0)
  sub_dat_rec <- subset(sub_dat, Recall_ACC == 1)
  fok.gamma <- rcorr.cens(sub_dat$FOK_RESP, sub_dat$Recog_ACC, outx = T)[2]
  fok.gamma.un <- rcorr.cens(sub_dat_un$FOK_RESP, sub_dat_un$Recog_ACC, outx = T)[2]
  cj.gamma <- rcorr.cens(sub_dat$RCJ_RESP, sub_dat$Recog_ACC, outx = T)[2]
  cj.gamma.un <- rcorr.cens(sub_dat_un$RCJ_RESP, sub_dat_un$Recog_ACC, outx = T)[2]
  cj.gamma.rec <- rcorr.cens(sub_dat_rec$RCJ_RESP, sub_dat_rec$Recog_ACC, outx = T)[2]
  fok.recall.gamma <- rcorr.cens(sub_dat$Recall_ACC, sub_dat$FOK_RESP, outx = T)[2]
  fok.cj.gamma <- rcorr.cens(sub_dat$FOK_RESP, sub_dat$RCJ_RESP)[2]
  fok.cj.gamma.un <- rcorr.cens(sub_dat_un$FOK_RESP, sub_dat_un$RCJ_RESP)[2]
  fok.cj.gamma.rec <- rcorr.cens(sub_dat_rec$FOK_RESP, sub_dat_rec$RCJ_RESP)[2]
  
  # Merge data
  gamma_dat <- rbind(gamma_dat, 
                     data.frame(Participant = sub, 
                                Condition = as.character(sub_dat$Condition[1]), 
                                FOK_Gamma = as.numeric(fok.gamma),
                                FOK_Gamma_Unrecall = as.numeric(fok.gamma.un), 
                                CJ_Gamma = as.numeric(cj.gamma), 
                                CJ_Gamma_Unrecall = as.numeric(cj.gamma.un),
                                CJ_Gamma_Recall = as.numeric(cj.gamma.rec),
                                FOK_Recall_Gamma = as.numeric(fok.recall.gamma),
                                FOK_CJ_Gamma = as.numeric(fok.cj.gamma),
                                FOK_CJ_Gamma_Unrecall = as.numeric(fok.cj.gamma.un),
                                FOK_CJ_Gamma_Recall = as.numeric(fok.cj.gamma.rec)
                     )
  )
}
# Coerce factor
gamma_dat$Condition <- as.factor(gamma_dat$Condition)