################################################################################
## PNOMICS 22 ANALYSES - PART SET CORRELATIONS
################################################################################
## Author       : Taylor Curley
## Email        : tmc2737 "at" gmail.com
## Date         : 10/26/22
################################################################################
## Notes        : Part set correlations for judgments given during specific
##              : trials within test blocks.
##              :
##              :
################################################################################

# Create trial bins
oi_dat$CycleTrialBin <- "1-3"
oi_dat$CycleTrialBin[(oi_dat$CycleTrial>=3)&(oi_dat$CycleTrial<=6)] <- "4-7"
oi_dat$CycleTrialBin[oi_dat$CycleTrial>=7] <- "8-10"

# Instantiate blank data frame
within_set_gamma <- data.frame(Participant = numeric(),
                             Condition = character(),
                             CycleTrialBin = character(),
                             FOK_Gamma = numeric(),
                             FOK_Gamma_Un = numeric())

# Calculate gamma correlations within bins for each participant
for (sub in unique(oi_dat$Participant)){
  for (bin in unique(oi_dat$CycleTrialBin)){
    sub_dat <- subset(oi_dat, Participant == sub & CycleTrialBin == bin)
    sub_dat_un <- subset(sub_dat, Recall_ACC == 0)
    sub_out <- data.frame(Participant = sub,
                          Condition = sub_dat$Condition[1],
                          CycleTrialBin = bin,
                          FOK_Gamma = rcorr.cens(sub_dat$FOK_RESP,sub_dat$Recog_ACC, outx=T)[2],
                          FOK_Gamma_Un = rcorr.cens(sub_dat_un$FOK_RESP,sub_dat_un$Recog_ACC, outx=T)[2]
                          )
    within_set_gamma <- rbind(within_set_gamma,sub_out)
  }
}

# Calculate gamma correlations between bins for each participant
part_set_gamma <- data.frame(Participant = numeric(),
                             Condition = character(),
                             Comparison = character(),
                             FOK_Gamma = numeric(),
                             FOK_Gamma_Un = numeric())

for (sub in unique(oi_dat$Participant)){
  sub_dat <- subset(oi_dat, Participant == sub)
  sub_dat_un <- subset(sub_dat, Recall_ACC == 0)
  # First bin vs 2 + 3
  for (comp_bin in c("4-7","8-10")){
    sub_dat_sub <- subset(sub_dat, CycleTrialBin == comp_bin | CycleTrialBin == "1-3")
    sub_dat_sub_un <- subset(sub_dat_un, CycleTrialBin == comp_bin | CycleTrialBin == "1-3")
    sub_out <- data.frame(Participant = sub,
                          Condition = sub_dat$Condition[1],
                          Comparison = paste0("1-3 + ",comp_bin),
                          FOK_Gamma = rcorr.cens(sub_dat_sub$FOK_RESP,sub_dat_sub$Recog_ACC, outx=T)[2],
                          FOK_Gamma_Un = rcorr.cens(sub_dat_sub_un$FOK_RESP,sub_dat_sub_un$Recog_ACC, outx=T)[2]
    )
    part_set_gamma <- rbind(part_set_gamma,sub_out)
  }
  # Second bin vs 3
  sub_dat_sub <- subset(sub_dat, CycleTrialBin == "4-7" | CycleTrialBin == "8-10")
  sub_dat_sub_un <- subset(sub_dat_un, CycleTrialBin == "4-7" | CycleTrialBin == "8-10")
  sub_out <- data.frame(Participant = sub,
                        Condition = sub_dat$Condition[1],
                        Comparison = "4-7 + 8-10",
                        FOK_Gamma = rcorr.cens(sub_dat_sub$FOK_RESP,sub_dat_sub$Recog_ACC, outx=T)[2],
                        FOK_Gamma_Un = rcorr.cens(sub_dat_sub_un$FOK_RESP,sub_dat_sub_un$Recog_ACC, outx=T)[2]
  )
  part_set_gamma <- rbind(part_set_gamma,sub_out)
}
