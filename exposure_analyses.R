# exposure_analyses
# Written by Stephanie DeCross, June 2022

# SOURCE, LOAD, & FORMAT DATA -----------------------------------------------------------

rm(list=ls())

# source packages
require(pacman)
p_load("data.table", "MASS", "MANOVA.RM", "kableExtra", "stats", "tidyverse")

# load data

# of note:  subj2 and subj11 non-numerical values (infinity/constant) for rater-scored hab & act rituals (4 missing values),
#           and subj11 did not provide value for act_rituals_p (1 missing value).
#           manually exclude subj11 hab_rituals_p to allow repeated-measures/within-subjects tests to run with no missing values  

data <- read.csv("data/data_all.csv") # load orig data

# remove unnecessary variables for these analyses
data <- data %>%
  dplyr::select(-date_hab, -date_act, -sex, -age, -race, -ethnicity,
                -hab_acceptability1_p, -hab_acceptability2_p, -hab_acceptability3_p, -hab_acceptability4_p,
                -hab_acceptability5_p, -hab_acceptability6_p, -hab_acceptability7_p, -hab_acceptability8_p,
                -act_acceptability1_p, -act_acceptability2_p, -act_acceptability3_p, -act_acceptability4_p,
                -act_acceptability5_p, -act_acceptability6_p, -act_acceptability7_p, -act_acceptability8_p)

# reorder data (36 cols; subject + 17 hab + 17 ACT + preference)
data <- data %>%
  dplyr::select(subject,
                hab_distress_p, hab_rituals_p, 
                hab_acceptability_p, 
                hab_effort_p, hab_trouble_p, hab_willing_p,
                hab_defusion_p, hab_acceptance_p, hab_values_p,
                hab_distress_r, hab_rituals_r,
                hab_compliance_r,
                hab_effort_r, hab_trouble_r,
                hab_defusion_r, hab_acceptance_r, hab_values_r,
                act_distress_p, act_rituals_p, 
                act_acceptability_p, 
                act_effort_p, act_trouble_p, act_willing_p,
                act_defusion_p, act_acceptance_p, act_values_p,
                act_distress_r, act_rituals_r,
                act_compliance_r,
                act_effort_r, act_trouble_r,
                act_defusion_r, act_acceptance_r, act_values_r,
                preference_p)
dim(data) #29 x 36

# check and format data
sum(is.na(data)) # 5 NA, identified above, no other missing data
data[11, 3] <- NA # exclude hab_rituals_p for subj11
sum(is.na(data)) # 6 now
str(data)

data[ , "subject"] <- factor(data[ , "subject"]) # convert to factor

# replace numbers with characters; 1=habituation, 2=ACT, 3=no preference
data <- data %>%
  mutate(preference_p = ifelse(preference_p == 1, "habituation",
                               ifelse(preference_p == 2, "ACT", "no preference")))

data[ , "preference_p"] <- factor(data[ , "preference_p"]) # convert to factor, levels are 1=ACT, 2=hab, 3=no pref

str(data)

# reverse code variables that would aid in ease of interpretation
# defusion & acceptance: originally 0=total defusion/acceptance, 100=total fusion/nonacceptance
# want higher scores = more defusion/acceptance. REVERSE CODE.
# for trouble getting started: 0=no trouble, 100=impossible. this variable will be visualized
# on same plot with others where higher scores = better/preferable outcomes, so REVERSE CODE here.

data <- data %>%
  mutate(hab_defusion_p = 100 - hab_defusion_p,
         hab_defusion_r = 100 - hab_defusion_r,
         act_defusion_p = 100 - act_defusion_p,
         act_defusion_r = 100 - act_defusion_r,
         hab_acceptance_p = 100 - hab_acceptance_p,
         hab_acceptance_r = 100 - hab_acceptance_r,
         act_acceptance_p = 100 - act_acceptance_p,
         act_acceptance_r = 100 - act_acceptance_r,
         hab_trouble_p = 100 - hab_trouble_p,
         hab_trouble_r = 100 - hab_trouble_r,
         act_trouble_p = 100 - act_trouble_p,
         act_trouble_r = 100 - act_trouble_r)

# re-format for analysis
data_long <- data %>%
  dplyr::select(-starts_with("preference")) %>% # take out preference data, store in data_else 
  gather(key = "variable", value = "rating", 2:35) # cols 2 through end, leaving out "subject" col

data_long <- data_long %>%
  mutate(condition = 
           if_else(substr(variable, 1, 3) == "hab", "Habituation", "ACT")) 

data_long[ , "condition"] <- factor(data_long[ , "condition"], levels = c("Habituation", "ACT"))  # convert to factor
# levels Habituation=1, ACT=2
str(data_long)

# rename variables
unique(data_long$variable) # 34 total
data_long$variable <- substring(data_long$variable, 5)
unique(data_long$variable) # 17 total

data_wide <- data_long %>%
  pivot_wider(names_from = "variable", values_from = "rating")

str(data_wide) 
data_final <- data_wide

# analyses will necessitate wide format; therefore use data_final
# data_final, data_wide, data_long are all interchangeable final wrangling of data (no further processing)


# FOR .RMD FILE ----------------------------------------------------------------------------

# create table of vars
Construct <- c("Psychological flexibility", "Psychological flexibility", "Psychological flexibility",
               "Treatment engagement", "Treatment engagement", "Treatment engagement",
               "Distress", "Rituals", "Treatment preference", "Treatment acceptability", "Treatment compliance")
Variables <- c("Defusion", "Acceptance", "Values", "Effort to refrain from rituals", "Trouble beginning",
               "Willingness to repeat", "Distress", "Rituals", "Preference", "Acceptability", "Compliance")
Source <- c("Participant, rater", "Participant, rater", "Participant, rater", 
            "Participant, rater", "Participant, rater", "Participant",
            "Participant, rater", "Participant, rater", "Participant", "Participant", "Rater")

data_org <- data.frame(Construct, Variables, Source)

# DESCRIPTIVE STATISTICS ---------------------------------------------------------------------
# prep descriptives
data_hab <- data %>%
  dplyr::select(starts_with("hab")) %>% # take habituation only
  dplyr::select(ends_with(c("p", "r"))) # all participant ratings first, followed by all rater ratings

data_act <- data %>%
  dplyr::select(starts_with("act")) %>%
  dplyr::select(ends_with(c("p", "r"))) # all participant ratings first, followed by all rater ratings

data_else <- data %>%
  dplyr::select(-starts_with(c("act", "hab"))) # treatment preference data

# hab descriptives ---- 
data_hab_sum <- data_hab %>%
  summarise_all(funs(mean = mean(., na.rm=T),
                     sd = sd(., na.rm=T),
                     se = sd(., na.rm=T)/sqrt(sum(!is.na(.))),
                     min = min(., na.rm=T),
                     max = max(., na.rm=T)))

means_hab <- data_hab_sum[,1:17]
sds_hab <- data_hab_sum[,18:34]
ses_hab <- data_hab_sum[,35:51]
mins_hab <- data_hab_sum[,52:68]
maxs_hab <- data_hab_sum[,69:85]

means_hab <- t(means_hab)
sds_hab <- t(sds_hab)
ses_hab <- t(ses_hab)
mins_hab <- t(mins_hab)
maxs_hab <- t(maxs_hab)

colnames(means_hab) <- c("Mean")
colnames(sds_hab) <- c("SD")
colnames(ses_hab) <- c("SE")
colnames(mins_hab) <- c("Min")
colnames(maxs_hab) <- c("Max")

sum_hab_mat <- cbind(means_hab, sds_hab, ses_hab, mins_hab, maxs_hab) # saved as list
sum_hab <- as.data.frame(sum_hab_mat)
rownames(sum_hab) <- c()
sum_hab$"Variable" <- c("distress_p", "rituals_p", "acceptability_p", "effort_p", "trouble_p",
                        "willingness_p", "defusion_p", "acceptance_p", "values_p",
                        "distress_r", "rituals_r", "compliance_r", "effort_r", "trouble_r",
                        "defusion_r", "acceptance_r", "values_r")
# reorder
sum_hab <- sum_hab %>%
  dplyr::select(Variable, Mean, SD, SE, Min, Max)

sum_hab


# act descriptives ---- 
data_act_sum <- data_act %>%
  summarise_all(funs(mean = mean(., na.rm=T),
                     sd = sd(., na.rm=T),
                     se = sd(., na.rm=T)/sqrt(sum(!is.na(.))),
                     min = min(., na.rm=T),
                     max = max(., na.rm=T)))

means_act <- data_act_sum[,1:17]
sds_act <- data_act_sum[,18:34]
ses_act <- data_act_sum[,35:51]
mins_act <- data_act_sum[,52:68]
maxs_act <- data_act_sum[,69:85]

means_act <- t(means_act)
sds_act <- t(sds_act)
ses_act <- t(ses_act)
mins_act <- t(mins_act)
maxs_act <- t(maxs_act)

colnames(means_act) <- c("Mean")
colnames(sds_act) <- c("SD")
colnames(ses_act) <- c("SE")
colnames(mins_act) <- c("Min")
colnames(maxs_act) <- c("Max")

sum_act_mat <- cbind(means_act, sds_act, ses_act, mins_act, maxs_act) # saved as list
sum_act <- as.data.frame(sum_act_mat)
rownames(sum_act) <- c()
sum_act$"Variable" <- c("distress_p", "rituals_p", "acceptability_p", "effort_p", "trouble_p",
                        "willingness_p", "defusion_p", "acceptance_p", "values_p",
                        "distress_r", "rituals_r", "compliance_r", "effort_r", "trouble_r",
                        "defusion_r", "acceptance_r", "values_r")
# reorder
sum_act <- sum_act %>%
  dplyr::select(Variable, Mean, SD, SE, Min, Max)

sum_act

# other descriptives ----
# (frequency table for exposure type preference)

preftable <- table(data_else$preference_p)
preftable

actpref <- preftable[[1]]
habpref <- preftable[[2]]
nopref <- preftable[[3]]

paste0("Preference for ACT: ", round((actpref/29)*100,1), "%")
paste0("Preference for Habituation: ", round((habpref/29)*100,1), "%")
paste0("No preference: ", round((nopref/29)*100,1), "%")


# PSCYHOLOGICAL FLEXIBILITY FACTORS -----------------------------------------------------------

# Multivariate analysis using these methods are robust to normality violations and heteroskedasticity

# Omnibus MANOVA, participant ratings ---------------------------------------------------------
modelpsychflex_p <- multRM(cbind(defusion_p, acceptance_p, values_p) ~ condition, data = data_final, 
                         subject = "subject", within = "condition", seed = 123) # 10,000 iterations default
summary(modelpsychflex_p) 

#   condition  n defusion_p  acceptance_p  values_p
# 1 Habituation 29     45.517        64.793    70.483
# 2         ACT 29     53.414        61.138    87.931
# 
# Wald-Type Statistic (WTS):
#           Test statistic df  p-value
# condition "13.325"       "3" "0.004"
# 
# modified ANOVA-Type Statistic (MATS):
#                     Test statistic
# condition           8.34
# 
# p-values resampling:
#           paramBS (WTS) paramBS (MATS)
# condition "0.015"       "0.024" 

# Posthoc tests, participant ratings ----------------------------------------------------------

# univariate calculations with parametrically bootstrapped ATS, then correct for multiple comparisons
psychflexp1 <- RM(defusion_p ~ condition, data = data_final, subject = "subject", within = "condition", resampling = "paramBS", seed = 123)
summary(psychflexp1) # 0.304
psychflexp2 <- RM(acceptance_p ~ condition, data = data_final, subject = "subject", within = "condition", resampling = "paramBS", seed = 123)
summary(psychflexp2) # 0.538
psychflexp3 <- RM(values_p ~ condition, data = data_final, subject = "subject", within = "condition", resampling = "paramBS", seed = 123)
summary(psychflexp3) # 0.006

# FDR correction
pvals <- c(0.304, 0.538, 0.006)
round(stats::p.adjust(pvals, "fdr"), 6)
# 0.456 0.538 0.018
# driven by participant-rated values


# Omnibus MANOVA, rater ratings ---------------------------------------------------------
modelpsychflex_r <- multRM(cbind(defusion_r, acceptance_r, values_r) ~ condition, data = data_final, 
                          subject = "subject", within = "condition", seed = 123) # 10,000 iterations default
summary(modelpsychflex_r) 

#     condition  n defusion_r  acceptance_r  values_r
# 1 Habituation 29     40.345        51.034    92.931
# 2         ACT 29     36.379        54.483    98.966
# 
# Wald-Type Statistic (WTS):
#           Test statistic df  p-value
# condition "8.944"        "3" "0.03" 
# 
# modified ANOVA-Type Statistic (MATS):
#                   Test statistic
# condition          5.464
# 
# p-values resampling:
#           paramBS (WTS) paramBS (MATS)
# condition "0.056"       "0.051"  


# Posthoc tests, rater ratings ----------------------------------------------------------

# univariate calculations with parametrically bootstrapped ATS, then correct for multiple comparisons
psychflexr1 <- RM(defusion_r ~ condition, data = data_final, subject = "subject", within = "condition", resampling = "paramBS", seed = 123)
summary(psychflexr1) # 0.434
psychflexr2 <- RM(acceptance_r ~ condition, data = data_final, subject = "subject", within = "condition", resampling = "paramBS", seed = 123)
summary(psychflexr2) # 0.468
psychflexr3 <- RM(values_r ~ condition, data = data_final, subject = "subject", within = "condition", resampling = "paramBS", seed = 123)
summary(psychflexr3) # 0.026

# FDR correction
pvals <- c(0.434, 0.468, 0.026)
round(stats::p.adjust(pvals, "fdr"), 6)
# 0.468 0.468 0.078
# trend is driven by rater-rated values


# Figures -------------------------------------------------------------------------------

dataplot_p <- data_long %>% 
  dplyr::filter(variable == "defusion_p" | variable == "acceptance_p" | variable == "values_p")

ggplot(dataplot_p, aes(x=variable, y=rating)) + # keep color out of main aes, will do for each geom
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  labs(x = "Psychology flexibility factor", 
       y = "Rating (0-100)",
       title = "Participant-reported psychological flexibility \ndiffers by exposure type") +
  scale_x_discrete(labels= c("Acceptance", "Defusion", "Values")) +
  geom_point(size=0, stroke=0, shape=15, alpha = 0.8, aes(color=condition)) + # stroke is border
  scale_color_manual(values=c("#47ab5b", "#5a3b97")) + # controls colors for points
  geom_boxplot(aes(fill = condition), 
               alpha = 0.8, outlier.color = "black", outlier.size = 1, show.legend=FALSE,
               width=.5) +
  scale_fill_manual(values=c("#47ab5b", "#5a3b97")) + # controls colors for boxplot
  guides(color=guide_legend(override.aes=list(size=5), reverse = TRUE, title="Exposure type")) +
  stat_summary(aes(fill=condition), fun.y ="mean", geom="point", color="black", size = 3, shape=15,
               position=position_dodge(width=.5), show.legend=FALSE) +
  theme(axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black")) + # only wants to work if added last
  theme(plot.title = element_text(margin=margin(b = 25, unit = "pt")))

ggsave(filename="plots/psychflex_p.pdf", width=7.5, height=5.5, units="in", dpi=600)


dataplot_r <- data_long %>% 
  dplyr::filter(variable == "defusion_r" | variable == "acceptance_r" | variable == "values_r")

ggplot(dataplot_r, aes(x=variable, y=rating)) + # keep color out of main aes, will do for each geom
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  labs(x = "Psychology flexibility factor", 
       y = "Rating (0-100)",
       title = "Rater-reported psychological flexibility \ndiffers by exposure type at a trend level") +
  scale_x_discrete(labels= c("Acceptance", "Defusion", "Values")) +
  geom_point(size=0, stroke=0, shape=15, alpha = 0.8, aes(color=condition)) + # stroke is border
  scale_color_manual(values=c("#47ab5b", "#5a3b97")) + # controls colors for points
  geom_boxplot(aes(fill = condition), 
               alpha = 0.8, outlier.color = "black", outlier.size = 1, show.legend=FALSE,
               width=.5) +
  scale_fill_manual(values=c("#47ab5b", "#5a3b97")) + # controls colors for boxplot
  guides(color=guide_legend(override.aes=list(size=5), reverse = TRUE, title="Exposure type")) +
  stat_summary(aes(fill=condition), fun.y ="mean", geom="point", color="black", size = 3, shape=15,
               position=position_dodge(width=.5), show.legend=FALSE) +
  theme(axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black")) + # only wants to work if added last
  theme(plot.title = element_text(margin=margin(b = 25, unit = "pt")))

ggsave(filename="plots/psychflex_r.pdf", width=7.5, height=5.5, units="in", dpi=600)


# TREATMENT ENGAGEMENT FACTORS ----------------------------------------------------------------

# Multivariate analysis using these methods are robust to normality violations and heteroskedasticity

# Omnibus MANOVA, participant ratings ---------------------------------------------------------
modeltreateng_p <- multRM(cbind(effort_p, trouble_p, willing_p) ~ condition, data = data_final, 
                           subject = "subject", within = "condition", seed = 123) # 10,000 iterations default
summary(modeltreateng_p) #no

# Omnibus MANOVA, rater ratings ---------------------------------------------------------------
modeltreateng_r <- multRM(cbind(effort_r, trouble_r) ~ condition, data = data_final, 
                          subject = "subject", within = "condition", seed = 123) # 10,000 iterations default
summary(modeltreateng_r) 

#     condition  n effort_r  trouble_r
# 1 Habituation 29   70.517     66.897
# 2         ACT 29   80.690     69.828
# 
# Wald-Type Statistic (WTS):
#           Test statistic df  p-value
# condition "7.986"        "2" "0.018"
# 
# modified ANOVA-Type Statistic (MATS):
#                   Test statistic
# condition          4.944
# 
# p-values resampling:
#           paramBS (WTS) paramBS (MATS)
# condition "0.033"       "0.009"


# Posthoc tests, rater ratings ----------------------------------------------------------------
# univariate calculations with parametrically bootstrapped ATS, then correct for multiple comparisons
treatengr1 <- RM(effort_r ~ condition, data = data_final, subject = "subject", within = "condition", resampling = "paramBS", seed = 123)
summary(treatengr1) # 0.01
treatengr2 <- RM(trouble_r ~ condition, data = data_final, subject = "subject", within = "condition", resampling = "paramBS", seed = 123)
summary(treatengr2) # 0.36

# FDR correction
pvals <- c(0.01, 0.36)
round(stats::p.adjust(pvals, "fdr"), 6)
# 0.02 0.36
# driven by rater-rated effort


# Figure -------------------------------------------------------------------------------------

dataplot_te_r <- data_long %>% 
  dplyr::filter(variable == "effort_r" | variable == "trouble_r")

ggplot(dataplot_te_r, aes(x=variable, y=rating)) + # keep color out of main aes, will do for each geom
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  labs(x = "Treatment engagement factor", 
       y = "Rating (0-100)",
       title = "Rater-reported treatment engagement \ndiffers by exposure type") +
  scale_x_discrete(labels= c("Effort to resist", "Trouble initiating")) +
  geom_point(size=0, stroke=0, shape=15, alpha = 0.8, aes(color=condition)) + # stroke is border
  scale_color_manual(values=c("#47ab5b", "#5a3b97")) + # controls colors for points
  geom_boxplot(aes(fill = condition), 
               alpha = 0.8, outlier.color = "black", outlier.size = 1, show.legend=FALSE,
               width=.5) +
  scale_fill_manual(values=c("#47ab5b", "#5a3b97")) + # controls colors for boxplot
  guides(color=guide_legend(override.aes=list(size=5), reverse = TRUE, title="Exposure type")) +
  stat_summary(aes(fill=condition), fun.y ="mean", geom="point", color="black", size = 3, shape=15,
               position=position_dodge(width=.5), show.legend=FALSE) +
  theme(axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black")) + # only wants to work if added last
  theme(plot.title = element_text(margin=margin(b = 25, unit = "pt"))) +
  scale_y_continuous(limits=c(0,100)) 
  

ggsave(filename="plots/treateng_r.pdf", width=6.5, height=5.5, units="in", dpi=600)


# ASSUMPTION CHECKING FOR OTHER VARIABLES -----------------------------------------------

# compliance (ordinal variable)
with(data, boxplot(cbind(hab_compliance_r, act_compliance_r)))
hist(data$hab_compliance_r, breaks = 10) 
hist(data$act_compliance_r, breaks = 10) 
diff <- data$hab_compliance_r - data$act_compliance_r
hist(diff, breaks = 10, main = "Difference histogram")  
qqnorm(diff, main = "Difference Q-Q Plot")
qqline(diff) 

# acceptability
with(data, boxplot(cbind(hab_acceptability_p, act_acceptability_p)))
hist(data$hab_acceptability_p, breaks = 10) 
hist(data$act_acceptability_p, breaks = 10) 
diff <- data$hab_acceptability_p - data$act_acceptability_p
hist(diff, breaks = 10, main = "Difference histogram")  
qqnorm(diff, main = "Difference Q-Q Plot")
qqline(diff) 

# distress participant
with(data, boxplot(cbind(hab_distress_p, act_distress_p)))
hist(data$hab_distress_p, breaks = 10) 
hist(data$act_distress_p, breaks = 10) 
diff <- data$hab_distress_p - data$act_distress_p
hist(diff, breaks = 10, main = "Difference histogram") 
qqnorm(diff, main = "Difference Q-Q Plot")
qqline(diff) 

# distress rater
with(data, boxplot(cbind(hab_distress_r, act_distress_r)))
hist(data$hab_distress_r, breaks = 10) 
hist(data$act_distress_r, breaks = 10) 
diff <- data$hab_distress_r - data$act_distress_r
hist(diff, breaks = 10, main = "Difference histogram") 
qqnorm(diff, main = "Difference Q-Q Plot")
qqline(diff) 

# rituals participant
with(data, boxplot(cbind(hab_rituals_p, act_rituals_p))) 
hist(data$hab_rituals_p, breaks = 10) 
hist(data$act_rituals_p, breaks = 10) 
diff <- data$hab_rituals_p - data$act_rituals_p
hist(diff, breaks = 10, main = "Difference histogram")  
qqnorm(diff, main = "Difference Q-Q Plot")
qqline(diff) 

# rituals rater
with(data, boxplot(cbind(hab_rituals_r, act_rituals_r)))
hist(data$hab_rituals_r, breaks = 10) 
hist(data$act_rituals_r, breaks = 10) 
diff <- data$hab_rituals_r - data$act_rituals_r
hist(diff, breaks = 10, main = "Difference histogram")  
qqnorm(diff, main = "Difference Q-Q Plot")
qqline(diff) 


# RITUALS ------------------------------------------------------------------------
rit_p <- t.test(rituals_p ~ condition, data = data_final, paired = TRUE)
rit_p # t = -1.5067, df = 27, p-value = 0.1435

rit_r <- t.test(rituals_r ~ condition, data = data_final, paired = TRUE)
rit_r # t = -0.17756, df = 26, p-value = 0.8604

# DISTRESS -----------------------------------------------------------------------
dis_p <- t.test(distress_p ~ condition, data = data_final, paired = TRUE)
dis_p # t = -1.2258, df = 28, p-value = 0.2305

dis_r <- t.test(distress_r ~ condition, data = data_final, paired = TRUE)
dis_r # t = -2.0379, df = 28, p-value = 0.05111

# Figure
dataplot_dis_r <- data_long %>% 
  dplyr::filter(variable == "distress_r")

ggplot(dataplot_dis_r, aes(x = variable, y = rating)) +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 14), 
        axis.ticks.x = element_blank()) + 
  labs(x = "Distress", 
       y = "Rating (0-100)",
       title = "Rater-reported distress \ndiffers by exposure type at a trend level") +
  scale_x_discrete(labels= c("Distress")) +
  geom_point(aes(color=condition), size=0, stroke=0, shape=15, alpha = 0.8) + # stroke is border
  scale_color_manual(values=c("#47ab5b", "#5a3b97")) + # controls colors for points
  geom_boxplot(aes(fill=condition), alpha = 0.8, width = 0.5,
               outlier.color = "black", outlier.size=1, show.legend = FALSE) + # show outliers
  guides(color=guide_legend(override.aes=list(size=5), title="Exposure type", reverse=T)) + 
  scale_fill_manual(values=c("#47ab5b", "#5a3b97")) +
  guides(color=guide_legend(override.aes=list(size=5), reverse = TRUE, title="Exposure type")) +
  stat_summary(aes(fill=condition), fun.y ="mean", geom="point", color="black", 
               size = 3, shape=15, position=position_dodge(width=.5), show.legend=FALSE) +
  theme(plot.title = element_text(margin=margin(b = 25, unit = "pt"))) +
  scale_y_continuous(limits=c(0,100)) 


ggsave(filename="plots/distress_r.pdf", width=5.5, height=5.5, units="in", dpi=600)

# COMPLIANCE ---------------------------------------------------------------------
# Wilcoxon signed-rank test, for ordinal data
comp_r_wilcox <- wilcox.test(data$hab_compliance_r, data$act_compliance_r, paired=TRUE) 
comp_r_wilcox # V=56, p-value = 0.3025
zstat <- qnorm(comp_r_wilcox$p.value/2) #-1.031079
zstat
# (z=-1.03, p=.30)

# ACCEPTABILITY ------------------------------------------------------------------
acceptability_p <- t.test(acceptability_p ~ condition, data = data_final, paired = TRUE)
acceptability_p # t = -2.5789, df = 28, p-value = 0.01546

# Figure
dataplot_acpt <- data_long %>% 
  dplyr::filter(variable == "acceptability_p")

ggplot(dataplot_acpt, aes(x = variable, y = rating)) +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 14), 
        axis.ticks.x = element_blank()) + 
  labs(x = "Acceptability", 
       y = "Rating (8-40)",
       title = "Treatment acceptability \ndiffers by exposure type") +
  scale_x_discrete(labels= c("Acceptability")) +
  geom_point(aes(color=condition), size=0, stroke=0, shape=15, alpha = 0.8) + # stroke is border
  scale_color_manual(values=c("#47ab5b", "#5a3b97")) + # controls colors for points
  geom_boxplot(aes(fill=condition), alpha = 0.8, width = 0.5,
               outlier.color = "black", outlier.size=1, show.legend = FALSE) + # show outliers
  guides(color=guide_legend(override.aes=list(size=5), title="Exposure type", reverse=T)) + 
  scale_fill_manual(values=c("#47ab5b", "#5a3b97")) +
  guides(color=guide_legend(override.aes=list(size=5), reverse = TRUE, title="Exposure type")) +
  stat_summary(aes(fill=condition), fun.y ="mean", geom="point", color="black", 
               size = 3, shape=15, position=position_dodge(width=.5), show.legend=FALSE) +
  scale_y_continuous(limits=c(8,40)) +
  theme(plot.title = element_text(margin=margin(b = 25, unit = "pt")))

ggsave(filename="plots/acceptability.pdf", width=5.5, height=5.5, units="in", dpi=600)


# PREFERENCE ----------------------------------------------------------------------
# frequency table/percentage
pref <- table(data_else$preference_p)
pref 
chisq.test(pref)
# X-squared = 27.586, df = 2, p-value = 1.023e-06
# X^2(2, N=29)=27.59, p<.001

# ACT   habituation no preference 
# 23             3             3 

23/29 # 0.7931034 ~ 79.3%
3/29 # 0.1034483 ~ 10.3 %

# Figure
df <- data.frame(
  type = c("Habituation", "ACT", "No preference"),
  count = c(3, 23, 3))
df$fraction = df$count / sum(df$count)
df$ymax <- cumsum(df$fraction) # cumulative percentages
df$ymin <- c(0, head(df$ymax, n=-1))
df$labelPosition <- (df$ymax + df$ymin) / 2
df$label <- paste0(df$`Exposure type`, "\n value: ", df$count)

ggplot(df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=type)) +
  theme_void(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_rect(alpha = 0.8) +
  scale_fill_manual(values=c("#5a3b97", "#47ab5b", "gray")) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  ggtitle("Treatment preference") +
  geom_text(x=3.5, aes(y=labelPosition, label=paste0(round(fraction*100), "%", sep=""))) +
  guides(fill=guide_legend(title="Exposure type")) 

ggsave(filename="plots/preference.pdf", width=7, height=5.5, units="in", dpi=600)




