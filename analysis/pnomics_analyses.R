################################################################################
## PNOMICS 2022 ANALYSES - MAIN DOCUMENT
################################################################################
## Author       : Taylor Curley
## Email        : tmc2737 "at" gmail.com
## Date         : 10/26/22
################################################################################
## Notes        : Any supporting R scripts have an underscore (_) prepended to
##              : the title. Check the README file for more information.
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

# Save figures? If TRUE, then figures will write to the figure path by calling
# the "./_save_figs.R" script
save_fig <- F

# Set custom color palette
gt_palette <- c("#0286ce","#eaaa00","#D6DBD4","#AD4025","#545454")

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
    guides(fill = "none") + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) 

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
    guides(fill = "none") + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) 

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
recall.means.5 <- data_summary(recall.means.3, varname = "Recall_Cent", 
                              groupnames = c("Condition","CycleTrial"))

## Plot the means
recall.plot.2 <- 
  ggplot(recall.means.4, aes(x = CycleTrial, y = Recall_ACC, color = Condition)) +
    geom_point(alpha = 0.4) +
    geom_errorbar(aes(ymin = Recall_ACC - se, ymax = Recall_ACC + se), alpha = 0.4) +
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

################################################################################
## FOKS 
################################################################################

# Means across trials (all items)

fok.means.1 <- data_summary(oi_dat, varname = "FOK_RESP", 
                            groupnames = c("Participant","Condition","CycleTrial"))
fok.means.2 <- data_summary(fok.means.1, varname = "FOK_RESP",
                            groupnames = c("Condition","CycleTrial"))
fok.means.2$Type <- "All Items"

## Plot (linear regression)
fok.plot.1 <- 
  ggplot(fok.means.2, aes(x = CycleTrial, y = FOK_RESP, color = Condition)) +
    geom_point(alpha = 0.4) +
    geom_errorbar(aes(ymin = FOK_RESP - se, ymax = FOK_RESP + se), alpha = 0.4) +
    geom_smooth(method = "lm") +
    xlab("Trial (Within Recall Cycle)") +
    ylab("FOK") +
    ylim(50,70) +
    theme(plot.title = element_text(hjust=0.5), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line.x = element_line(), 
          axis.line.y = element_line(), 
          legend.position = c(.8,.9), 
          panel.background = element_rect(fill='white', color = "black"),
          strip.text.x = element_text(size = 11, face = "bold")) +
    scale_color_manual(values=gt_palette) +
    scale_x_continuous(breaks = seq(1,10))

## Plot (Loess regression)
fok.plot.2 <- 
  ggplot(fok.means.2, aes(x = CycleTrial, y = FOK_RESP, color = Condition)) +
    geom_smooth(method = "loess", se = F) +
    xlab("Trial (Within Recall Cycle)") +
    ylab("FOK") +
    ylim(50,70) +
    theme(plot.title = element_text(hjust=0.5), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line.x = element_line(), 
          axis.line.y = element_line(), 
          legend.position = c(.8,.9), 
          panel.background = element_rect(fill='white', color = "black"),
          strip.text.x = element_text(size = 11, face = "bold")) +
    scale_color_manual(values=gt_palette) +
    scale_x_continuous(breaks = seq(1,10))


# Means across trials (unrecalled items)

fok.means.3 <- data_summary(oi_dat_un, varname = "FOK_RESP", 
                            groupnames = c("Participant","Condition","CycleTrial"))
fok.means.4 <- data_summary(fok.means.3, varname = "FOK_RESP",
                            groupnames = c("Condition","CycleTrial"))
fok.means.4$Type <- "Unrecalled Items"

## Plot (linear regression)
fok.plot.3 <- 
  ggplot(fok.means.4, aes(x = CycleTrial, y = FOK_RESP, color = Condition)) +
    geom_point(alpha = 0.4) +
    geom_errorbar(aes(ymin = FOK_RESP - se, ymax = FOK_RESP + se), alpha = 0.4) +
    geom_smooth(method = "lm") +
    xlab("Trial (Within Recall Cycle)") +
    ylab("FOK") +
    ylim(30,50) +
    theme(plot.title = element_text(hjust=0.5), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line.x = element_line(), 
          axis.line.y = element_line(), 
          legend.position = c(.8,.9), 
          panel.background = element_rect(fill='white', color = "black"),
          strip.text.x = element_text(size = 11, face = "bold")) +
    scale_color_manual(values=gt_palette) +
    scale_x_continuous(breaks = seq(1,10))

## Plot (Loess regression)
fok.plot.4 <- 
  ggplot(fok.means.4, aes(x = CycleTrial, y = FOK_RESP, color = Condition)) +
    geom_smooth(method = "loess", se = F) +
    xlab("Trial (Within Recall Cycle)") +
    ylab("FOK") +
    ylim(30,50) +
    theme(plot.title = element_text(hjust=0.5), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line.x = element_line(), 
          axis.line.y = element_line(), 
          legend.position = c(.8,.9), 
          panel.background = element_rect(fill='white', color = "black"),
          strip.text.x = element_text(size = 11, face = "bold")) +
    scale_color_manual(values=gt_palette) +
    scale_x_continuous(breaks = seq(1,10))

# Plot all items + unrecalled items together
fok.means.5 <- rbind(fok.means.2,fok.means.4)

fok.plot.5 <- 
  ggplot(fok.means.5, aes(x = CycleTrial, y = FOK_RESP, color = Condition)) +
    geom_point(alpha = 0.4) +
    geom_errorbar(aes(ymin = FOK_RESP - se, ymax = FOK_RESP + se), alpha = 0.4) +
    geom_smooth(method = "lm") +
    xlab("Trial (Within Recall Cycle)") +
    ylab("FOK") +
    facet_grid(.~Type) +
    theme_base() +
    theme(plot.title = element_text(hjust=0.5), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line.x = element_line(), 
          axis.line.y = element_line(), 
          #legend.position = c(.8,.8), 
          panel.background = element_rect(fill='white', color = "black"),
          strip.text.x = element_text(size = 14, face = "bold"),
          legend.title = element_text(size=12),
          legend.text = element_text(size=10),
          plot.background = element_blank()) +
    scale_color_manual(values=gt_palette) +
    scale_x_continuous(breaks = seq(1,10))

# Means by condition - All Items
fok.means.6 <- data_summary(oi_dat, varname = "FOK_RESP", 
                            groupnames = c("Participant","Condition"))
fok.means.7 <- data_summary(fok.means.6, varname = "FOK_RESP", 
                            groupnames = c("Condition"))
fok.means.7$Type <- "All Items"

# Means by condition - Unrecalled Items
fok.means.8 <- data_summary(oi_dat_un, varname = "FOK_RESP", 
                           groupnames = c("Participant","Condition"))
fok.means.9 <- data_summary(fok.means.8, varname = "FOK_RESP", 
                            groupnames = c("Condition"))
fok.means.9$Type <- "Unrecalled Items"

## Plot the means
fok.means.10 <- rbind(fok.means.7,fok.means.9)

fok.plot.6 <-
  ggplot(fok.means.10, aes(x=Condition, y=FOK_RESP, fill=Condition)) + 
  labs(x = "Condition", y = "FOK") + 
  ylim(0,100) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=FOK_RESP-se, ymax=FOK_RESP+se), width=.2, position=position_dodge(.9)) +
  facet_grid(.~Type) +
  theme_base() +
  theme(plot.title = element_text(hjust=0.5), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(), 
        axis.line.y = element_line(), 
        legend.position = "none", 
        panel.background = element_rect(fill='white', color = "black"),
        strip.text.x = element_text(size = 14, face = "bold"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10),
        plot.background = element_blank()) +
  scale_fill_manual(values=gt_palette) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) 

# Means by recall outcome
fok.means.11 <- data_summary(oi_dat, varname = "FOK_RESP",
                             groupnames = c("Participant","Condition","Recall_Err"))
fok.means.12 <- data_summary(fok.means.11, varname = "FOK_RESP",
                             groupnames = c("Condition","Recall_Err"))
fok.means.12$Recall_Err <- factor(fok.means.12$Recall_Err, 
                                  levels = c("Correct","Comm","Omm"),
                                  labels = c("Correct","Commission","Omission"))

## Plot the means
fok.plot.7 <-
  ggplot(fok.means.12, aes(x=Recall_Err, y=FOK_RESP, fill=Recall_Err)) + 
  labs(x = "Recall Outcome", y = "FOK") + 
  ylim(0,100) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=FOK_RESP-se, ymax=FOK_RESP+se), width=.2, position=position_dodge(.9)) +
  facet_grid(.~Condition) +
  theme_base() +
  theme(plot.title = element_text(hjust=0.5), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(), 
        axis.line.y = element_line(), 
        legend.position = "none", 
        panel.background = element_rect(fill='white', color = "black"),
        strip.text.x = element_text(size = 14, face = "bold"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10),
        plot.background = element_blank()) +
  scale_fill_manual(values=gt_palette) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) 

################################################################################
## FOK X RECOG GAMMAS
################################################################################

# All Items

## Compute means

fok.gamma.means.1 <- data_summary(gamma_dat, varname = "FOK_Gamma", 
                                  groupnames = c("Condition"))
colnames(fok.gamma.means.1) <- c("Condition","Gamma","sd","se")
fok.gamma.means.1$Type <- "All Items"

## t-tests + Cohen's d
fok.gamma.t.1 <- t.test(FOK_Gamma ~ Condition, data = gamma_dat)
fok.gamma.d.1 <- cohens_d(FOK_Gamma ~ Condition, data = gamma_dat)

# Unrecalled items
## Compute means
fok.gamma.means.2 <- data_summary(gamma_dat, varname = "FOK_Gamma_Unrecall", 
                                  groupnames = c("Condition"))
colnames(fok.gamma.means.2) <- c("Condition","Gamma","sd","se")
fok.gamma.means.2$Type <- "Unrecalled Items"

# Means plots
fok.gamma.means.3 <- rbind(fok.gamma.means.1,fok.gamma.means.2)
fok.gamma.plot.1 <-
  ggplot(fok.gamma.means.3, aes(x=Condition,y=Gamma,fill=Type)) +
    geom_bar(stat = "identity", color="black", width=0.8) +
    geom_errorbar(aes(ymin = Gamma - se, ymax = Gamma + se), width = 0.5) +
    geom_hline(yintercept = 0.0, lty=3) +
    facet_grid(.~Type) +
    ylim(-0.25,0.5) +
    scale_fill_manual(values=gt_palette) +
    theme_base() +
    theme(legend.position = "none",
          strip.text.x = element_text(face = "bold"),
          plot.background = element_blank())
  
################################################################################
## CJ X RECOG GAMMAS
################################################################################

# All Items

## Means
cj.gamma.means.1 <- data_summary(gamma_dat, varname = "CJ_Gamma", groupnames = "Condition")
colnames(cj.gamma.means.1) <- c("Condition","Gamma","sd","se")
cj.gamma.means.1$Type <- "All Items"

## t-tests + Cohen's D
cj.gamma.t.1 <- t.test(CJ_Gamma ~ Condition, data = gamma_dat)
cj.gamma.d.1 <- cohens_d(CJ_Gamma ~ Condition, data = gamma_dat)

# Unrecalled Items

## Means
cj.gamma.means.2 <- data_summary(gamma_dat, varname = "CJ_Gamma_Unrecall", groupnames = "Condition")
colnames(cj.gamma.means.2) <- c("Condition","Gamma","sd","se")
cj.gamma.means.2$Type <- "Unrecalled Items"

## t-tests + Cohen's D
cj.gamma.t.2 <- t.test(CJ_Gamma_Unrecall ~ Condition, data = gamma_dat)
cj.gamma.d.2 <- cohens_d(CJ_Gamma_Unrecall ~ Condition, data = gamma_dat)

# Recalled Items

## Means
cj.gamma.means.3 <- data_summary(gamma_dat, varname = "CJ_Gamma_Recall", groupnames = "Condition")
colnames(cj.gamma.means.3) <- c("Condition","Gamma","sd","se")
cj.gamma.means.3$Type <- "Recalled Items"

# Means plots
cj.gamma.means.4 <- rbind(cj.gamma.means.1,cj.gamma.means.2,cj.gamma.means.3)
cj.gamma.plot.1 <-
  ggplot(cj.gamma.means.4, aes(x=Condition,y=Gamma,fill=Type)) +
    geom_bar(stat = "identity", color="black", width=0.8) +
    geom_errorbar(aes(ymin = Gamma - se, ymax = Gamma + se), width = 0.5) +
    geom_hline(yintercept = 0.0, lty=3) +
    facet_grid(.~Type) +
    ylim(-0.2,1.0) +
    scale_fill_manual(values=gt_palette) +
    theme_base() +
    theme(legend.position = "none",
          strip.text.x = element_text(face = "bold"),
          plot.background = element_blank())

################################################################################
## FOK X RECALL GAMMAS
################################################################################

## Means
fok.gamma.means.4 <- data_summary(gamma_dat, varname = "FOK_Recall_Gamma", 
                                  groupnames = "Condition")

## t-test + Cohen's d
fok.gamma.t.2 <- t.test(FOK_Recall_Gamma ~ Condition, data = gamma_dat)
fok.gamma.d.2 <- cohens_d(FOK_Recall_Gamma ~ Condition, data = gamma_dat)

################################################################################
## FOK X CJ GAMMAS
################################################################################

# All items

## Means
fok.cj.means.1 <- data_summary(gamma_dat, varname = "FOK_CJ_Gamma",
                               groupnames = "Condition")
colnames(fok.cj.means.1) <- c("Condition","Gamma","sd","se")
fok.cj.means.1$Type <- "All Items"

# Unrecalled Items

## Means
fok.cj.means.2 <- data_summary(gamma_dat, varname = "FOK_CJ_Gamma_Unrecall",
                               groupnames = "Condition")
colnames(fok.cj.means.2) <- c("Condition","Gamma","sd","se")
fok.cj.means.2$Type <- "Unrecalled Items"

# Recalled Items

## Means
fok.cj.means.3 <- data_summary(gamma_dat, varname = "FOK_CJ_Gamma_Recall",
                               groupnames = "Condition")
colnames(fok.cj.means.3) <- c("Condition","Gamma","sd","se")
fok.cj.means.3$Type <- "Recalled Items"

# Means plots
fok.cj.means.4 <- rbind(fok.cj.means.1,fok.cj.means.2,fok.cj.means.3)
fok.cj.plot.1 <-
  ggplot(fok.cj.means.4, aes(x=Condition,y=Gamma,fill=Type)) +
    geom_bar(stat = "identity", color="black", width=0.8) +
    geom_errorbar(aes(ymin = Gamma - se, ymax = Gamma + se), width = 0.5) +
    geom_hline(yintercept = 0.0, lty=3) +
    facet_grid(.~Type) +
    ylim(-0.25,0.5) +
    scale_fill_manual(values=gt_palette) +
    theme_base() +
    theme(legend.position = "none",
          strip.text.x = element_text(face = "bold"),
          plot.background = element_blank())

################################################################################
## PART-SET CORRELATIONS
################################################################################

# Call external script
source("./_partset_corr.R")

# Within-bin gammas - All items

## Means
part.gamma.means.1 <- data_summary(within_set_gamma, varname = "FOK_Gamma", 
                                   groupnames = c("CycleTrialBin","Condition"))
colnames(part.gamma.means.1) <- c("CycleTrialBin","Condition","Gamma","sd","se")
part.gamma.means.1$Type <- "All Items"

# Within-bin gammas - Unrecalled items

## Means
part.gamma.means.2 <- data_summary(within_set_gamma, varname = "FOK_Gamma_Un",
                                   groupnames = c("CycleTrialBin","Condition"))
colnames(part.gamma.means.2) <- c("CycleTrialBin","Condition","Gamma","sd","se")
part.gamma.means.2$Type <- "Unrecalled Items"

## Figure
part.gamma.means.3 <- rbind(part.gamma.means.1,part.gamma.means.2)
part.gamma.figure.1 <-
  ggplot(part.gamma.means.3, aes(x = CycleTrialBin, y = Gamma,group = Condition, color = Condition)) +
    geom_line(stat = "identity") +
    geom_point(stat = "identity") +
    geom_errorbar(aes(ymin = Gamma - se, ymax = Gamma + se), width=0.25) +
    geom_hline(yintercept = 0.0, lty=3) +
    facet_grid(.~Type) +
    xlab("Trial Bin (Within Block)") +
    ylim(-0.25,0.75) +
    scale_color_manual(values = gt_palette) +
    theme_base() +
    theme(plot.title = element_text(hjust=0.5), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line.x = element_line(), 
          axis.line.y = element_line(), 
          legend.position = c(.8,.8), 
          panel.background = element_rect(fill='white', color = "black"),
          strip.text.x = element_text(size = 14, face = "bold"),
          legend.title = element_text(size=12),
          legend.text = element_text(size=10),
          plot.background = element_blank())

# Part-set gammas - All items
part.gamma.means.4 <- data_summary(part_set_gamma, varname = "FOK_Gamma",
                                   groupnames = c("Condition","Comparison"))
colnames(part.gamma.means.4) <- c("Condition","Comparison","Gamma","sd","se")
part.gamma.means.4$Type <- "All Items"

# Part-set gammas - Unrecalled items
part.gamma.means.5 <- data_summary(part_set_gamma, varname = "FOK_Gamma_Un",
                                   groupnames = c("Condition","Comparison"))
colnames(part.gamma.means.5) <- c("Condition","Comparison","Gamma","sd","se")
part.gamma.means.5$Type <- "Unrecalled Items"

## Plot results
part.gamma.means.6 <- rbind(part.gamma.means.4,part.gamma.means.5)
part.gamma.figure.2 <-
  ggplot(part.gamma.means.6, aes(x=Comparison,y=Gamma,fill=Condition)) +
  geom_bar(stat = "identity", color="black", width=0.8) +
  geom_errorbar(aes(ymin = Gamma - se, ymax = Gamma + se), width = 0.5) +
  geom_hline(yintercept = 0.0, lty=3) +
  facet_grid(Condition~Type) +
  #ylim(-0.25,0.5) +
  scale_fill_manual(values=gt_palette) +
  theme_base() +
  theme(legend.position = "none",
        strip.text.x = element_text(face = "bold"),
        plot.background = element_blank())

################################################################################
## R/K/N
################################################################################



################################################################################
## SAVE DATA
################################################################################

# Save figures
if (save_fig) source("./_save_figs.R")
