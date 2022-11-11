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
require(effectsize)
require(patchwork)

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
## 4AFC ACC
################################################################################

# MEAN BY CONDITION
recog.means.1 <- data_summary(oi_dat, varname = "Recog_ACC", 
                             groupnames = c("Participant","Condition"))
recog.means.2 <- data_summary(recog.means.1, varname = "Recog_ACC", 
                             groupnames = c("Condition"))

# t-test + Cohen's d
recog.t.1 <- t.test(Recog_ACC ~ Condition, data = recog.means.1)
recog.d.1 <- cohens_d(Recog_ACC ~ Condition, data = recog.means.1)

## Plot the means
recog.plot.1 <-
  ggplot(recog.means.2, aes(x=Condition, y=Recog_ACC, fill=Condition)) + 
    labs(title="Average 4AFC Performance", x = "Condition", y = "% Correct") + 
    ylim(0,1) +
    geom_bar(stat="identity", color="black", position=position_dodge()) +
    geom_errorbar(aes(ymin=Recog_ACC-se, ymax=Recog_ACC+se), width=.2, position=position_dodge(.9)) +
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
  ggsave(filename = paste0(fig_path,"recog_cond.png"), plot = recog.plot.1, 
         height = 4, width = 3.5, units = "in", dpi = 600)
}

################################################################################
## CUED RECALL ACC
################################################################################

# MEAN BY CONDITION
recall.means.1 <- data_summary(oi_dat, varname = "Recall_ACC", 
                              groupnames = c("Participant","Condition"))
recall.means.2 <- data_summary(recall.means.1, varname = "Recall_ACC", 
                              groupnames = c("Condition"))

# t-test + Cohen's d
recall.t.1 <- t.test(Recall_ACC ~ Condition, data = recall.means.1)
recall.d.1 <- cohens_d(Recall_ACC ~ Condition, data = recall.means.1)

## Plot the means
recall.plot.1 <-
  ggplot(recall.means.2, aes(x=Condition, y=Recall_ACC, fill=Condition)) + 
    labs(title="Average Recall Performance", x = "Condition", y = "% Correct") + 
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
  ggsave(filename = paste0(fig_path,"recall_cond.png"), plot = recall.plot.1, 
         height = 4, width = 3.5, units = "in", dpi = 600)
}

# MEANS BY TRIAL
recall.means.3 <- data_summary(oi_dat, varname = "Recall_ACC", 
                              groupnames = c("Participant","Condition","CycleTrial"))
recall.means.3$Recall_Cent <- 0
for (p in unique(oi_dat$Participant)){
  recall.means.3$Recall_Cent[recall.means.3$Participant == p] <- 
    scale(recall.means.3$Recall_ACC[recall.means.3$Participant == p], 
          center = T, scale = F)
}
recall.means.4 <- data_summary(recall.means.3, varname = "Recall_ACC", 
                              groupnames = c("Condition","CycleTrial"))
recall.means.4$CycleTrial <- recall.means.4$CycleTrial+1
recall.means.5 <- data_summary(recall.means.3, varname = "Recall_Cent", 
                              groupnames = c("Condition","CycleTrial"))

## Plot the means
recall.plot.2 <- 
  ggplot(recall.means.4, aes(x = CycleTrial, y = Recall_ACC, color = Condition)) +
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
  ggsave(filename = paste0(fig_path,"recall_trial.png"), plot = recall.plot.2,
         height = 5, width = 4, units = "in", dpi = 600)
}

## Plot the Loess regression
recall.plot.3 <- 
  ggplot(recall.means.4, aes(x = CycleTrial, y = Recall_ACC, color = Condition)) +
  geom_smooth(method = "loess", se = F) +
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
  ggsave(filename = paste0(fig_path,"recall_trial_loess.png"), plot = recall.plot.3,
         height = 5, width = 4, units = "in", dpi = 600)
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
## PART-SET CORRELATIONS
################################################################################

# Call external script
source("./_partset_corr.R")

################################################################################
## EXTRA STUFF
################################################################################

# Combine recall + recog figures
# Have shared x and y labels
recall.plot.1.alt <-
  recall.plot.1 +
  xlab(" ")

recog.plot.1.alt <-
  recog.plot.1 + 
  xlab(" ") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

recall.recog.plot <-
  (recall.plot.1.alt + recog.plot.1.alt) +
  plot_annotation(subtitle = "Condition",
                  theme = theme(plot.subtitle = element_text(hjust=0.525, vjust = -105,
                                                              size = 12)))

## Save figure
if (save_fig){
  ggsave(filename = paste0(fig_path,"recall_recog_means.png"), plot = recall.recog.plot)
}

# Combine linear and Loess recall figures
recall.plot.2.alt <- 
  recall.plot.2 + 
  ggtitle("Linear") +
  xlab(" ") +
  theme(legend.position = "none")

recall.plot.3.alt <-
  recall.plot.3 +
  ggtitle("Loess") +
  xlab(" ") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = c(0.5,0.8))

recall.lin.loess.plot <-
  (recall.plot.2.alt + recall.plot.3.alt) +
  plot_annotation(subtitle = "Trial (Within Block)",
                  theme = theme(plot.subtitle = element_text(hjust=0.525, vjust = -138, size = 12)))

## Save figure
if (save_fig){
  ggsave(filename = paste0(fig_path,"recall_lin_loess.png"), plot = recall.lin.loess.plot,
         height = 5, width = 7, units = "in")
}