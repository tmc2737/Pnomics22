################################################################################
## PNOMICS 2022 ANALYSES - MAIN DOCUMENT
################################################################################
## Author       : Taylor Curley
## Email        : tmc2737 "at" gmail.com
## Date         : 10/26/22
################################################################################
## Notes        : Any supporting R scripts have an underscore (_) prepended to
##              : the title.
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
require(interactions)

# Save figures? If TRUE, then figures will write to the figure path
save_fig <- FALSE

# Set custom color palette
gt_palette <- c("#0286ce","#eaaa00", "#AD4025","#545454")

# Default figure path
fig_path <- "../img/"

# Import custom functions
source("./_helper_functions.R")

# Data import + clean
source("./_import_dat.R")

# Compute overall gammas
source("./_overall_gamma.R")

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
  ggsave(paste0(fig_path,"recall_cond.png"), height = 4, width = 3, 
         units = "in", dpi = 600)
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
recall_means4 <- data_summary(recall_means3, varname = "Recall_ACC", 
                              groupnames = c("Condition","CycleTrial"))
recall_means4$CycleTrial <- recall_means4$CycleTrial+1
recall_means5 <- data_summary(recall_means3, varname = "Recall_Cent", 
                              groupnames = c("Condition","CycleTrial"))

## Plot the means
ggplot(recall_means4, aes(x = CycleTrial, y = Recall_ACC, color = Condition)) +
  geom_point() +
  geom_errorbar(aes(ymin = Recall_ACC - se, ymax = Recall_ACC + se)) +
  geom_smooth(method = "loess", se = F, linetype = 2, alpha = 0.1) +
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
  ggsave(paste0(fig_path,"recall_trial.png"), height = 5, width = 4, 
         units = "in", dpi = 600)
}

# MEANS BY TRIAL WITHIN RECALL BLOCK


# MEANS BY TRIAL - LOGIT MLM - RANDOM INTERCEPTS

## Intercept model
recall.glmer.0 <- glmer(Recall_ACC ~ 1 + (1|Participant), 
                        family = binomial(link="logit"), data = oi_dat)

## Model with condition + trial across blocks
recall.glmer.1 <- glmer(Recall_ACC ~ Condition*CycleTrial + (1|Participant),
                         family = binomial(link="logit"), data = oi_dat)

## Model with condition + trial within blocks
recall.glmer.2 <- glmer(Recall_ACC ~ Condition*Cycle*CycleTrial + (1|Participant),
                        family = binomial(link="logit"), data = oi_dat)

## Plot random intercepts
if (save_fig){
  ggCaterpillar(ranef(recall.glmer.0), QQ=F)
  ggsave(paste0(fig_path,"recall_ranef.png"), height = 6, width = 4, 
         units = "in", dpi = 600)
}

################################################################################
## SECTION X: PART-SET CORRELATIONS
################################################################################

# Call external script
source("./_partset_corr.R")