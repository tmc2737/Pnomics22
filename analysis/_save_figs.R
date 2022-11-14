################################################################################
## PNOMICS 22 ANALYSES - FIGURES
################################################################################
## Author       : Taylor Curley
## Email        : tmc2737 "at" gmail.com
## Date         : 11/12/22
################################################################################
## Notes        : The figure code was taking up a lot of room, so I pushed the
##              : code to a separate file. 
##              :
##              :
################################################################################

################################################################################
## DECLARATIONS
################################################################################

# Default figure path
fig_path <- "../img/"

################################################################################
## MAIN FIGURES
################################################################################

# 4AFC ACC MEANS
ggsave(filename = paste0(fig_path,"recog_cond.png"), plot = recog.plot.1, 
       height = 4, width = 3.5, units = "in", dpi = 600)

# RECALL MEANS - BY CONDITION
ggsave(filename = paste0(fig_path,"recall_cond.png"), plot = recall.plot.1, 
       height = 4, width = 3.5, units = "in", dpi = 600)

# RECALL MEANS - BY TRIAL
ggsave(filename = paste0(fig_path,"recall_trial.png"), plot = recall.plot.2,
       height = 5, width = 4, units = "in", dpi = 600)

# RECALL MEANS - LOESS REGRESSION
ggsave(filename = paste0(fig_path,"recall_trial_loess.png"), plot = recall.plot.3,
       height = 5, width = 4, units = "in", dpi = 600)

# RECALL MLM - RANDOM INTERCEPTS
ggCaterpillar(ranef(recall.glmer.0), QQ=F)
ggsave(paste0(fig_path,"recall_ranef.png"), height = 6, width = 4, 
       units = "in", dpi = 600)

# FOK MEANS - TRIALS (LINEAR)
ggsave(filename = paste0(fig_path,"fok_trial_1.png"), plot = fok.plot.1,
       height = 5, width = 4, units = "in", dpi = 600)

# FOK MEANS - TRIALS (LOESS)
ggsave(filename = paste0(fig_path,"fok_trial_2.png"), plot = fok.plot.2,
       height = 5, width = 4, units = "in", dpi = 600)

# FOK MEANS - TRIALS (ALL + UNRECALLED ITEMS)
ggsave(filename = paste0(fig_path,"fok_trial_3.png"), plot = fok.plot.5,
       height = 4, width = 6, units = "in", dpi = 600)

# FOK MEANS - CONIDITON (ALL + UNRECALLED ITEMS)
ggsave(filename = paste0(fig_path,"fok_means_1.png"), plot = fok.plot.6,
       height = 4, width = 6, units = "in", dpi = 600)

# FOK MEANS - RECALL OUTCOMES
ggsave(filename = paste0(fig_path,"fok_means_2.png"), plot = fok.plot.7,
       height = 4, width = 7, units = "in", dpi = 600)

# FOK-RECOG GAMMA MEANS
ggsave(filename = paste0(fig_path,"fok_gamma_1.png"), plot = fok.gamma.plot.1,
       height = 4, width = 5.5, units = "in", dpi = 600)

# CJ-RECOG GAMMA MEANS
ggsave(filename = paste0(fig_path,"cj_gamma_1.png"), plot = cj.gamma.plot.1,
       height = 4, width = 8, units = "in", dpi = 600)

# FOK-CJ GAMMA MEANS
ggsave(filename = paste0(fig_path,"fok_cj_gamma.png"), plot = fok.cj.plot.1,
       height = 4, width = 8, units = "in", dpi = 600)

# WITHIN-SET FOK-RECOG GAMMA MEANS
ggsave(filename = paste0(fig_path,"part_gamma_1.png"), plot = part.gamma.figure.1,
       height = 5, width = 6, units = "in", dpi = 600)

# BETWEEN-SET FOK-RECOG GAMMA MEANS
ggsave(filename = paste0(fig_path, "part_gamma_2.png"), plot = part.gamma.figure.2,
       height = 5, width = 8, units = "in", dpi = 600)

# RKN MEANS - BY CONDITION
ggsave(filename = paste0(fig_path, "rkn_1.png"), plot = rkn.plot.1,
       height = 4, width = 5, units = "in", dpi = 600)

################################################################################
## EXTRA FIGURES
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
                  theme = theme(plot.subtitle = element_text(hjust=0.525, vjust = -140,
                                                             size = 12)))

## Save figure
ggsave(filename = paste0(fig_path,"recall_recog_means.png"), plot = recall.recog.plot,
       height=5,width=6,units="in", dpi = 600)

# Combine linear and Loess recall figures
recall.plot.2.alt <- 
  recall.plot.2 + 
  ggtitle("Linear") +
  xlab(" ") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

recall.plot.3.alt <-
  recall.plot.3 +
  ggtitle("Loess") +
  xlab(" ") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "right")

recall.lin.loess.plot <-
  (recall.plot.2.alt + recall.plot.3.alt) +
  plot_annotation(subtitle = "Trial (Within Block)",
                  theme = theme(plot.subtitle = element_text(hjust=0.525, vjust = -138, size = 12)))

## Save figure
ggsave(filename = paste0(fig_path,"recall_lin_loess.png"), plot = recall.lin.loess.plot,
       height = 5, width = 7, units = "in", dpi = 600)

# Combine linear and Loess FOK figures

fok.plot.1.alt <- 
  fok.plot.1 +
  ggtitle("Linear") +
  xlab(" ") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "bold"))

fok.plot.2.alt <-
  fok.plot.2 +
  ggtitle("Loess") +
  xlab(" ") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "right",
        plot.title = element_text(face = "bold"))

fok.lin.loess.plot <-
  (fok.plot.1.alt + fok.plot.2.alt) +
  plot_annotation(subtitle = "Trial (Within Block)",
                  theme = theme(plot.subtitle = element_text(hjust=0.525, vjust = -138, size = 12)))

## Save figure
ggsave(filename = paste0(fig_path,"fok_lin_loess.png"), plot = fok.lin.loess.plot,
       height = 5, width = 7, units = "in", dpi = 600)
