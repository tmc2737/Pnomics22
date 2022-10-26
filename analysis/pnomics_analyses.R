################################################################################
## PNOMICS 2022 ANALYSES
################################################################################
## Author       : Taylor Curley
## Email        : tmc2737 "at" gmail.com
## Date         : 10/26/22
################################################################################
## Notes        :
##              :
##              :
##              :
################################################################################

################################################################################
## PRELIMINARY OPERATIONS
################################################################################

# Import packages
require(ggplot2)
require(ggthemes)
require(lme4)
require(lmerTest)
require(jtools)
require(tidyr)
require(reshape2)
require(Hmisc)

# Save figures? If TRUE, then figures will write to the figure path
save_fig <- FALSE

# Set custom color palette
gt_palette <- c("#0286ce","#eaaa00", "#AD4025","#545454")

# Default figure path
fig_path <- "../img/"

# Data summary function
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE),
      se = sd(x[[col]], na.rm=TRUE)/sqrt(length(x[[col]])))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

# Data import + clean
source("./_script_clean.R")

# Compute overall gammas
gamma_dat <- data.frame(Participant = numeric(),
                        Condition = character(),
                        FOK_Gamma = numeric(),
                        FOK_Gamma_Unrecall = numeric(),
                        CJ_Gamma = numeric(),
                        CJ_Gamma_Unrecall = numeric(),
                        FOK_Recall_Gamma = numeric(),
                        FOK_CJ_Gamma = numeric()
)

for (sub in unique(oi_dat$Participant)){
  sub_dat <- subset(oi_dat, Participant == sub)
  sub_dat_un <- subset(sub_dat, Recall_ACC == 0)
  fok.gamma <- rcorr.cens(sub_dat$FOK_RESP, sub_dat$Recog_ACC, outx = T)[2]
  fok.gamma.un <- rcorr.cens(sub_dat_un$FOK_RESP, sub_dat_un$Recog_ACC, outx = T)[2]
  cj.gamma <- rcorr.cens(sub_dat$RCJ_RESP, sub_dat$Recog_ACC, outx = T)[2]
  cj.gamma.un <- rcorr.cens(sub_dat_un$RCJ_RESP, sub_dat_un$Recog_ACC, outx = T)[2]
  fok.recall.gamma <- rcorr.cens(sub_dat$Recall_ACC, sub_dat$FOK_RESP, outx = T)[2]
  fok.cj.gamma <- rcorr.cens(sub_dat$FOK_RESP, sub_dat$RCJ_RESP)[2]
  
  gamma_dat <- rbind(gamma_dat, 
                     data.frame(Participant = sub, 
                                Condition = as.character(sub_dat$Condition[1]), 
                                FOK_Gamma = as.numeric(fok.gamma),
                                FOK_Gamma_Unrecall = as.numeric(fok.gamma.un), 
                                CJ_Gamma = as.numeric(cj.gamma), 
                                CJ_Gamma_Unrecall = as.numeric(cj.gamma.un),
                                FOK_Recall_Gamma = as.numeric(fok.recall.gamma),
                                FOK_CJ_Gamma = as.numeric(fok.cj.gamma)
                     )
  )
}
gamma_dat$Condition <- as.factor(gamma_dat$Condition)

################################################################################
## SECTION 1: CUED RECALL ACC
################################################################################

# MEAN BY CONDITION
recall_means1 <- data_summary(oi_dat, varname = "Recall_ACC", 
                              groupnames = c("Participant","Condition"))
recall_means2 <- data_summary(recall_means1, varname = "Recall_ACC", 
                              groupnames = c("Condition"))

## Plot the means
ggplot(recall_means2, aes(x=Condition, y=Recall_ACC, fill=Condition)) + 
  labs(title="Average Recall Performance", x = "Condition", y = "% Recalled") + 
  ylim(0,1) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=Recall_ACC-se, ymax=Recall_ACC+se), width=.2, position=position_dodge(.9)) +
  theme(plot.title = element_text(hjust=0.5), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(), 
        axis.line.y = element_line(), 
        legend.title = element_blank(),
        legend.position = c(.9,.8), 
        panel.background = element_rect(fill='white', color = "black"),
        strip.text.x = element_text(size = 11, face = "bold")) +
  scale_fill_manual(values=gt_palette)+
  geom_hline(yintercept = 0) +
  guides(fill = "none")

## Save figure
if (save_fig){
  ggsave(paste0(fig_path,"recall_cond.png"), height = 4, width = 3, units = "in",
         dpi = 300)
}

# MEANS BY TRIAL
recall_means3 <- data_summary(oi_dat, varname = "Recall_ACC", 
                              groupnames = c("Participant","Condition","CycleTrial"))
recall_means3$Recall_Cent <- 0
for (p in unique(oi_dat$Participant)){
  recall_means3$Recall_Cent[recall_means3$Participant == p] <- 
    scale(recall_means3$Recall_ACC[recall_means3$Participant == p], 
          center = T, scale = F)
}
recall_means4 <- data_summary(oi_dat, varname = "Recall_ACC", 
                              groupnames = c("Condition","CycleTrial"))
recall_means4$CycleTrial <- recall_means4$CycleTrial+1
recall_means5 <- data_summary(recall_means3, varname = "Recall_Cent", 
                              groupnames = c("Condition","CycleTrial"))

## Plot the means
ggplot(recall_means4, aes(x = CycleTrial, y = Recall_ACC, color = Condition)) +
  geom_point() +
  geom_errorbar(aes(ymin = Recall_ACC - se, ymax = Recall_ACC + se)) +
  #geom_smooth(method = "loess", se = F, linetype = 2, alpha = 0.1) +
  geom_smooth(method = "lm") +
  xlab("Trial (Within Recall Cycle)") +
  ylab("% Recalled") +
  ylim(0.25,0.55) +
  theme(plot.title = element_text(hjust=0.5), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(), 
        axis.line.y = element_line(), 
        legend.position = c(.8,.8), 
        panel.background = element_rect(fill='white', color = "black"),
        strip.text.x = element_text(size = 11, face = "bold")) +
  scale_color_manual(values=gt_palette) +
  scale_x_continuous(breaks = seq(1,10))

## Save figure
if (save_fig){
  ggsave(paste0(fig_path,"recall_trial.png"), height = 4, width = 4, units = "in",
         dpi = 300)
}