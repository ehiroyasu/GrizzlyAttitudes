# Here I work through weighting code using my dataframe for Grizzly data #

setwd("/Users/cmiljanich/Desktop/Grizzly Data")

grizz <- read.csv("GRIZZLY_DATA_USE.csv", header = T)

library(survey)
library(weights)
library(anesrake)
library(lmtest)
library(sandwich)
library(car)

# Create dummy for highet_degree and urban_rural so I can weight according to whether 
# someone is (not) rural and whether they (don't) have college degree. This is jsut a 
# formality type thing to make the weighting process go more smoothly. 

grizz$college <- grizz$highest_degree>=5 
grizz$rural <- grizz$urban_rural==3

# Here I isolate percent of respondents falling into each category. This is so I have 
# a rough estimate of distribution of respondents. 

wpct(grizz$gender)
wpct(grizz$urban_rural)
wpct(grizz$latino)
wpct(grizz$highest_degree)
wpct(grizz$college)
wpct(grizz$rural)

# wpct produces a table of the proportion of data in each category for any variable.

# Here I use US Census data to make variables with distribution of each data for all of 
# California. 

gender <- c(.50, .50)
rural <- c(.13, .87) # Rural; non-rural.
latino <- c(.39, .61) # Latino; not-latino.
college <- c(.32, .68) # BA/higher; no college degree.  

# Here I put the census variables into a list and name them. This is so they can be called 
# upon later and used for weighting. Essentially, they are population-level estiamtes for 
# all of California and are what we weight to. 

targets <- list(gender, rural, latino, college)
names(targets) <- c("gender", "rural", "latino", "college")

# Here I create unique ids for all complete observations in the dataframe, and then determine
# which variables should be used to weight based on the ones I provide in "target" list. 

grizz$caseid <- 1:length(grizz$good_complete) # Create unique id for each complete obs. 
anesrakefinder(targets, grizz, choosemethod="total") 

# Here I rake weights data. This process determines how variables should be weighted and then rakes the
# data to develop weights for the variables so that they match the target values I provided. 

newgrizz <- anesrake(targets, grizz, caseid = grizz$caseid,
                     verbose=F, cap=5, choosemethod = "total",
                     type="pctlim", pctlim=.05, nlim=5, iterate=T, force1=T)

# Here I create a new dataframe with weighted data #

wt_grizz_df <- svydesign(ids = ~1, data = grizz, weights = newgrizz$weightvec) 

# List of what the function's arguments do:
# ids = formula/dataframe identifying cluster ids from largest level to smallest level (-0) and -1 is a 
# formula for no clusters. 
# data = dataframe
# weights = out weights to be used in the data. Select weights from anesrake object we created 
# earlier and use sigil to select numeric column title "weightvec" from it. 

summary(wt_grizz_df) # Need to use this and other functions from survey package with weighted data. 

# See weights 

sup_unwt <-  wpct(grizz$griz_support) 
sup_wt  <-  wpct(grizz$griz_support, newgrizz$weightvec) # See ** 
sup_tab <- data.frame(sup_unwt, sup_wt)
sup_tab

# ** You can weight your table by providing a vector of weights in wpct function, which I do
# by calling the weightvector from newgrizz. 

table(grizz$griz_support)/length(grizz$X)*100

# Here I produce a table to check that wpct calculated the unweighted proportion correctly. 

grizz$weights <- newgrizz$weightvec

# Here I create a csv file for my weights. 

write.csv(print(newgrizz), "GRIZZ_WT_USE.csv")

# Regression using SURVEY package # 

# DV treated as continuous # 

sup_ols_svyglm <-  svyglm(griz_support ~ 
                            as.factor(college) # TRUE = College degree; FALSE = No college degree.
                          + as.factor(rural) # TRUE = Rural; FALSE = Not Rural. 
                          + as.factor(gender) # 1 = Male; 2 = Female.
                          + age
                          + hhincome
                          + as.factor(latino) # 1 = Latino; 2 = Not Latino. 
                          + ideology # Increase corresponds to being more conservative
                          + as.factor(partyid) # 1 = Rep.; 2 = Dem.; 3 = Ind.; 4 = Other. 
                          + as.factor(votechoice) # 1 = Trump; 2 = Clinton; 3 = Other; 4 = Didn't Vote. 
                          + as.factor(species_griz) # 1 = Yes; 2 = No; 3 = Don't Know. 
                          + Q72_16
                          + Q72_17
                          + Q72_18
                          + Q72_19
                          + Q72_21
                          + Q72_22
                          + Q72_23
                          + Q72_24
                          + Q72_25
                          + Q72_27
                          + Q72_28
                          + Q72_29
                          + Q72_30
                          + Q72_31
                          + Q72_32
                          + Q66 # Unit increase corresponds to increased likelihood to discontinue.
                          + as.factor(natpark_ca) # 1 = Yes; 2 = No; 3 = Don't Know.
                          + as.factor(gtreat1num), # 1 = Treated; 0 = Control.
                          family="gaussian",
                          design = wt_grizz_df)

summary(sup_ols_svyglm)

# OLS regression using LM #

# DV treated as continuous with weights # 

sup_ols_lm_wts <- lm(griz_support ~ 
                       as.factor(college) # TRUE = College degree; FALSE = No college degree.
                     + as.factor(rural) # TRUE = Rural; FALSE = Not Rural. 
                     + as.factor(gender) # 1 = Male; 2 = Female. 
                     + age
                     + hhincome 
                     + as.factor(latino) # 1 = Latino; 2 = Not Latino. 
                     + ideology # Increase corresponds to being more conservative
                     + as.factor(partyid) # 1 = Rep.; 2 = Dem.; 3 = Ind.; 4 = Other. 
                     #+ as.factor(votechoice) # 1 = Trump; 2 = Clinton; 3 = Other; 4 = Didn't Vote. 
                     + as.factor(species_griz) # 1 = Yes; 2 = No; 3 = Don't Know. 
                     + Factor1
                     + Factor2
                     + Factor3
                     + Factor4
                     + Factor5
                     +biospherism
                     +egoism
                     +altruism
                     + Q66 # Unit increase corresponds to increased likelihood to discontinue.
                     + as.factor(natpark_ca) # 1 = Yes; 2 = No; 3 = Don't Know.
                     + as.factor(gtreat1num), # 1 = Treated; 0 = Control.
                     weights = weights,
                     data = grizz)

summary(sup_ols_lm_wts)

# OLS regression using LM WITHOUT weights # SEs wrong so use SURVEY 

# DV treated as continuous #

sup_ols_lm_nwts <- lm(griz_support ~ 
                        as.factor(college) # TRUE = College degree; FALSE = No college degree.
                      + as.factor(rural) # TRUE = Rural; FALSE = Not Rural. 
                      + as.factor(gender) # 1 = Male; 2 = Female. 
                      + age
                      + hhincome 
                      + as.factor(latino) # 1 = Latino; 2 = Not Latino. 
                      + ideology # Unit increase corresponds to being more conservative.
                      + as.factor(partyid) # 1 = Rep.; 2 = Dem.; 3 = Ind.; 4 = Other. 
                      + as.factor(votechoice) # 1 = Trump; 2 = Clinton; 3 = Other; 4 = Didn't Vote. 
                      + as.factor(species_griz) # 1 = Yes; 2 = No; 3 = Don't Know. 
                      + Q72_16
                      + Q72_17
                      + Q72_18
                      + Q72_19
                      + Q72_21
                      + Q72_22
                      + Q72_23
                      + Q72_24
                      + Q72_25
                      + Q72_27
                      + Q72_28
                      + Q72_29
                      + Q72_30
                      + Q72_31
                      + Q72_32
                      + Q66 # Unit increase corresponds to increased likelihood to discontinue.
                      + natpark_ca # 1 = Yes; 2 = No; 3 = Don't Know.
                      + as.factor(gtreat1num), # 1 = Treated; 0 = Control.
                      data = grizz)

summary(sup_ols_lm_nwts)

# Ordered Logit NOT using survey package, but WITH weights # Don't use

library(MASS)
library(Hmisc)
library(reshape2)

sup_olog <- (polr(as.ordered(griz_support) ~ 
                    as.factor(college) # TRUE = College degree; FALSE = No college degree.
                  + as.factor(rural) # TRUE = Rural; FALSE = Not Rural. 
                  + as.factor(gender) # 1 = Male; 2 = Female. 
                  + age
                  + hhincome 
                  + as.factor(latino) # 1 = Latino; 2 = Not Latino. 
                  + ideology # Unit increase corresponds to being more conservative.
                  + as.factor(partyid) # 1 = Rep.; 2 = Dem.; 3 = Ind.; 4 = Other. 
                  + as.factor(votechoice) # 1 = Trump; 2 = Clinton; 3 = Other; 4 = Didn't Vote. 
                  + as.factor(species_griz) # 1 = Yes; 2 = No; 3 = Don't Know. 
                  + Q72_16
                  + Q72_17
                  + Q72_18
                  + Q72_19
                  + Q72_21
                  + Q72_22
                  + Q72_23
                  + Q72_24
                  + Q72_25
                  + Q72_27
                  + Q72_28
                  + Q72_29
                  + Q72_30
                  + Q72_31
                  + Q72_32
                  + Q66 # Unit increase corresponds to increased likelihood to discontinue.
                  + as.factor(natpark_ca) # 1 = Yes; 2 = No; 3 = Don't Know.
                  + as.factor(gtreat1num), # 1 = Treated; 0 = Control.
                  weights = weights,
                  method = c("logistic"),
                  Hess = TRUE,
                  data = grizz))

summary(sup_olog)

# Extract p values. 

sup_olog.coef <- data.frame(coef(summary(sup_olog))) # Put coeff. summary for model into df.
sup_olog.coef$pval <- round((pnorm(abs(sup_olog.coef$t.value), lower.tail = FALSE) * 2),3) # Create p-values.
sup_olog.coef

# Ordered Logit NOT using survey package, but WITH NO weights # Don't use

sup_olog_nw <- (polr(as.ordered(griz_support) ~ 
                       as.factor(college) # TRUE = College degree; FALSE = No college degree.
                     + as.factor(rural) # TRUE = Rural; FALSE = Not Rural. 
                     + as.factor(gender) # 1 = Male; 2 = Female. 
                     + age
                     + hhincome 
                     + as.factor(latino) # 1 = Latino; 2 = Not Latino. 
                     + ideology # Unit increase corresponds to being more conservative.
                     + as.factor(partyid) # 1 = Rep.; 2 = Dem.; 3 = Ind.; 4 = Other. 
                     + as.factor(votechoice) # 1 = Trump; 2 = Clinton; 3 = Other; 4 = Didn't Vote. 
                     + as.factor(species_griz) # 1 = Yes; 2 = No; 3 = Don't Know. 
                     + Q72_16
                     + Q72_17
                     + Q72_18
                     + Q72_19
                     + Q72_21
                     + Q72_22
                     + Q72_23
                     + Q72_24
                     + Q72_25
                     + Q72_27
                     + Q72_28
                     + Q72_29
                     + Q72_30
                     + Q72_31
                     + Q72_32
                     + Q66 # Unit increase corresponds to increased likelihood to discontinue.
                     + as.factor(natpark_ca) # 1 = Yes; 2 = No; 3 = Don't Know.
                     + as.factor(gtreat1num), # 1 = Treated; 0 = Control.
                     method = c("logistic"),
                     Hess = TRUE,
                     data = grizz))

# Extract P values # 

sup_olog_nw.coef <- data.frame(coef(summary(sup_olog_nw)))
sup_olog_nw.coef$pval <- round((pnorm(abs(sup_olog_nw.coef$t.value), lower.tail = FALSE) * 2),3)
sup_olog_nw.coef

# Create dataframe to check whether weights have an impact on findings

olog_sup_tab <- data.frame(sup_olog.coef, sup_olog_nw.coef)
olog_sup_tab

# Suggests that there are some slight differences. I'll just continue with calculations for both datums. 

# Odds ratios for both

# Weighted data

sup_olog_or <- exp(coef(sup_olog))
sup_olog_or

# Unweighted data

sup_olog_nw_or <- exp(coef(sup_olog_nw))
sup_olog_nw_or

# Predicted probabilities for both

# Weighted data

sup_olog_pr <- predict(sup_olog, type = "probs")
summary(sup_olog_pr)

# Unweighted data

sup_olog_nw_pr <- predict(sup_olog_nw, type = "probs")
summary(sup_olog_nw_pr)

# Mean values indicate predicted probabilities of each category when all values are held at their means. 

# Heteroscedasticity

# Both tests reject null of no heteroscedasticity, so I should use robust errors. 

bptest(sup_ols_svyglm, studentize = F)
ncvTest(sup_ols_lm_wts)

# But, this is for two of the linear regressions and there is more I should do to parse
# this out. One being, just calculate robust standard errors. 

cbind(grizz$rural, grizz$urban_rural)

# Ologit with survey package

library(MASS) 

# Below I use "withReplicates" to generate an Ologit using weighted data from the survey object "wt_grzz_df"
# created with "svydesign". To do this I need to use as.svrepdesign to get it into correct format. The code
# is pretty self-explanatory from there. 

sup_olog_svypckg <- withReplicates(as.svrepdesign(wt_grizz_df),
                                   quote(coef(polr(as.ordered(griz_support) ~ 
                                                     as.factor(college) # TRUE = College degree; FALSE = No college degree.
                                                   + as.factor(rural) # TRUE = Rural; FALSE = Not Rural. 
                                                   + as.factor(gender) # 1 = Male; 2 = Female. 
                                                   + age
                                                   + hhincome 
                                                   + as.factor(latino) # 1 = Latino; 2 = Not Latino. 
                                                   + ideology # Unit increase corresponds to being more conservative.
                                                   + as.factor(partyid) # 1 = Rep.; 2 = Dem.; 3 = Ind.; 4 = Other. 
                                                   + as.factor(votechoice) # 1 = Trump; 2 = Clinton; 3 = Other; 4 = Didn't Vote. 
                                                   + as.factor(species_griz) # 1 = Yes; 2 = No; 3 = Don't Know. 
                                                   + Q72_16
                                                   + Q72_17
                                                   + Q72_18
                                                   + Q72_19
                                                   + Q72_21
                                                   + Q72_22
                                                   + Q72_23
                                                   + Q72_24
                                                   + Q72_25
                                                   + Q72_27
                                                   + Q72_28
                                                   + Q72_29
                                                   + Q72_30
                                                   + Q72_31
                                                   + Q72_32
                                                   + Q66 # Unit increase corresponds to increased likelihood to discontinue.
                                                   + as.factor(natpark_ca) # 1 = Yes; 2 = No; 3 = Don't Know.
                                                   + as.factor(gtreat1num),
                                                   weights = .weights))))

# Now I check to see how it is different from ologit using "polr" with weights.

# First I get our new sup_olog_svypckg into dataframe form

sup_olog_svypckg_df <- as.data.frame(sup_olog_svypckg) 

# Now I create pvalues for new dataframe object for ologit with svypckg

# t-value is coefficient/st. error. 

sup_olog_svypckg_df$tvalue <- sup_olog_svypckg_df$theta/sup_olog_svypckg_df$SE # Get t-values first

sup_olog_svypckg_df$pval <- round((pnorm(abs(sup_olog_svypckg_df$theta/sup_olog_svypckg_df$SE), # Plug t-values or formula for them into code
                                         lower.tail = FALSE) * 2),3)

# Now I compare results with sup_olog.

cbind(sup_olog_svypckg_df$pval, sup_olog.coef$pval)

# Seems as though some values do change in significance. SEs should be correct here #

#### Figures and Plots #### Not prettiest code at all, but just need to hammer it out. 

# Do Grizzly Bears Exist in CA #

ggplot(grizz, aes(species_griz)) + # I use geom_bar, rather than geom_histogram, here because it is categorical. Doesn't really matter though. 
  
  geom_bar(bins = 3, binwidth = .25, fill = "royalblue3", colour = "black") +
  
  labs(x = "Response", 
       y = "Frequency",
       title = "Do Grizzly Bears Exist in California") +
  
  scale_x_continuous(breaks = 1:3, labels = c("Yes", "No", "Don't Know")) + 
  
  theme(text = element_text(family = "Trebuchet MS"),
        plot.title = element_text(size = 14, face = "bold"), 
        axis.title.x = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 10, face = "bold"),
        panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))

ggsave("g_exist.jpg", height = 7, width = 7)

# Support for Reintroduction # 

sup_labs <- c("Strongly Oppose", "Oppose", "Somewhat Oppose", "Neither", "Somewhat Support", "Support", "Strongly Support")

ag_dag_labs <- c("Strongly Disagree", "Somewhat Disagree", "Neither", "Somewhat Agree", "Strongly Agree")

# All folks # 

ggplot(grizz, aes(griz_support)) + 
  
  geom_histogram(bins = 7, binwidth = .5, aes(fill = ..count..), colour = "black") + 
  
  scale_fill_gradient("Count", low = "blue", high = "red") + 
  
  labs(x = "Response",
       y = "Frequency",
       title = "Support for Reintroduction") + 
  
  scale_x_continuous(breaks = 1:7, labels = sup_labs) + 
  
  geom_vline(xintercept = mean(grizz$griz_support, na.rm = T), 
             colour = "black",
             size = .65, linetype = "dashed") +
  
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(family="Trebuchet MS"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))

ggsave("g_sup_all.jpg", height = 7, width = 7)


# Believers (Incorrect) #

ggplot(grizz[which(grizz$species_griz==1),], aes(griz_support)) + 
  
  geom_histogram(bins = 7, binwidth = .5, aes(fill = ..count..), colour = "black") + 
  
  scale_fill_gradient("Count", low = "blue", high = "red") + 
  
  labs(x = "Response",
       y = "Frequency",
       title = "Support for Reintroduction (Believers Only)") + 
  
  scale_x_continuous(breaks = 1:7, labels = sup_labs) + 
  
  geom_vline(xintercept = mean(grizz$griz_support[grizz$species_griz==1], na.rm = T), 
             colour = "black",
             size = .65, linetype = "dashed") +
  
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(family="Trebuchet MS"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))

ggsave("g_sup_believers.jpg", height = 7, width = 7)

# Non-believers (Correct) # 

ggplot(grizz[which(grizz$species_griz==2),], aes(griz_support)) + 
  
  geom_histogram(bins = 7, binwidth = .5, aes(fill = ..count..), color = "black") + 
  
  scale_fill_gradient("Count", low = "blue", high = "red") + 
  
  labs(x = "Response",
       y = "Frequency",
       title = "Support for Reintroduction (Non-Believers Only)") + 
  
  scale_x_continuous(breaks = 1:7, labels = sup_labs) + 
  scale_y_continuous(lim = c(0,60)) + 
  
  geom_vline(xintercept = mean(grizz$griz_support[grizz$species_griz==2], na.rm = T), 
             colour = "black",
             size = .65, linetype = "dashed") +
  
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(family="Trebuchet MS"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))

ggsave("g_sup_nonbelievers.jpg", height = 7, width = 7)

# Agree - Disagree Ranchers #

ranch <- ggplot(grizz, aes(Q72_28)) + 
  
  geom_histogram(bins = , binwidth = .5, aes(fill = ..count..), colour = "black") + 
  
  scale_fill_gradient("Count", low = "blue", high = "red") + 
  
  labs(x = "Response",
       y = "Frequency",
       title = "... would negatively impact ranchers?") + 
  
  scale_x_continuous(breaks = 1:5, labels = ag_dag_labs) + 
  scale_y_continuous(lim = c(0,400)) + 
  
  geom_vline(xintercept = mean(grizz$Q72_28, na.rm = T), 
             colour = "black",
             size = .65, linetype = "dashed") +
  
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(family="Trebuchet MS"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))

ranch

ggsave("ranch.jpg", height = 7, width = 7)

# Agree - Disagree Forests # 

forests <- ggplot(grizz, aes(Q72_17)) + 
  
  geom_histogram(bins = , binwidth = .5, aes(fill = ..count..), colour = "black") + 
  
  scale_fill_gradient("Count", low = "blue", high = "red") + 
  
  labs(x = "Response",
       y = "Frequency",
       title = "... would make CA forests healthier?") + 
  
  scale_x_continuous(breaks = 1:5, labels = ag_dag_labs) + 
  scale_y_continuous(lim = c(0,400)) + 
  
  geom_vline(xintercept = mean(grizz$Q72_17, na.rm = T), 
             colour = "black",
             size = .65, linetype = "dashed") +
  
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(family="Trebuchet MS"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))

ggsave("forests.jpg", height = 7, width = 7)

install.packages("cowplot")

library(cowplot)

plot_grid(ranch, forests)

ggsave("ranch_forests.jpg", height = 6, width = 8)

# Regressions # I use these for the table. 

# Treatment # - Randomly assigned so no need to condition on anything else.

trt_ols_w <- svyglm(griz_support ~ 
                      gtreat1num,
                    family="gaussian",
                    design = wt_grizz_df)

summary(trt_ols_w) 

# Demos, treatment, political stuff, exist, recreation #

dtper_ols_w <- svyglm(griz_support ~ 
                        gtreat1num # 1 = Treated; 0 = Control.
                      + as.factor(college) # TRUE = College degree; FALSE = No college degree.
                      + as.factor(rural) # TRUE = Rural; FALSE = Not Rural. 
                      + as.factor(gender) # 1 = Male; 2 = Female. 
                      + age
                      + hhincome 
                      + as.factor(latino) # 1 = Latino; 2 = Not Latino. 
                      + ideology # Unit increase corresponds to being more conservative.
                      + as.factor(partyid) # 1 = Rep.; 2 = Dem.; 3 = Ind.; 4 = Other. 
                      + as.factor(votechoice) # 1 = Trump; 2 = Clinton; 3 = Other; 4 = Didn't Vote. 
                      + Q66 
                      + as.factor(species_griz), # 1 = Yes; 2 = No; 3 = Don't Know.,
                      family="gaussian",
                      design = wt_grizz_df)

summary(dtper_ols_w)

# Full Beans #

ols_full_w <- svyglm(griz_support ~ 
                       gtreat1num # 1 = Treated; 0 = Control.
                     + as.factor(college) # TRUE = College degree; FALSE = No college degree.
                     + as.factor(rural) # TRUE = Rural; FALSE = Not Rural. 
                     + as.factor(gender) # 1 = Male; 2 = Female. 
                     + age
                     + hhincome 
                     + as.factor(latino) # 1 = Latino; 2 = Not Latino. 
                     + ideology # Increase corresponds to being more conservative
                     + as.factor(partyid) # 1 = Rep.; 2 = Dem.; 3 = Ind.; 4 = Other. 
                     + as.factor(votechoice) # 1 = Trump; 2 = Clinton; 3 = Other; 4 = Didn't Vote. 
                     + Q66 
                     + as.factor(species_griz) # 1 = Yes; 2 = No; 3 = Don't Know. 
                     + Q72_16
                     + Q72_17
                     + Q72_18
                     + Q72_19
                     + Q72_21
                     + Q72_22
                     + Q72_23
                     + Q72_24
                     + Q72_25
                     + Q72_27
                     + Q72_28
                     + Q72_29
                     + Q72_30
                     + Q72_31
                     + Q72_32
                     + as.factor(natpark_ca), # 1 = Yes; 2 = No; 3 = Don't Know.
                     family="gaussian",
                     design = wt_grizz_df)

summary(ols_full_w)

# Full Beans - BELIEVERS ONLY # 

ols_believers_w <- svyglm(griz_support ~ 
                            gtreat1num # 1 = Treated; 0 = Control.
                          + as.factor(college) # TRUE = College degree; FALSE = No college degree.
                          + as.factor(rural) # TRUE = Rural; FALSE = Not Rural. 
                          + as.factor(gender) # 1 = Male; 2 = Female. 
                          + age
                          + hhincome 
                          + as.factor(latino) # 1 = Latino; 2 = Not Latino. 
                          + ideology # Increase corresponds to being more conservative
                          + as.factor(partyid) # 1 = Rep.; 2 = Dem.; 3 = Ind.; 4 = Other. 
                          + as.factor(votechoice) # 1 = Trump; 2 = Clinton; 3 = Other; 4 = Didn't Vote. 
                          + Q66 
                          + Q72_16
                          + Q72_17
                          + Q72_18
                          + Q72_19
                          + Q72_21
                          + Q72_22
                          + Q72_23
                          + Q72_24
                          + Q72_25
                          + Q72_27
                          + Q72_28
                          + Q72_29
                          + Q72_30
                          + Q72_31
                          + Q72_32
                          + as.factor(natpark_ca), # 1 = Yes; 2 = No; 3 = Don't Know.
                          family="gaussian",
                          design = subset(wt_grizz_df, species_griz==1))

summary(ols_believers_w)

# Full Beans - NON-BELIEVERS ONLY # 

ols_nonbelievers_w <- svyglm(griz_support ~ 
                               gtreat1num # 1 = Treated; 0 = Control.
                             + as.factor(college) # TRUE = College degree; FALSE = No college degree.
                             + as.factor(rural) # TRUE = Rural; FALSE = Not Rural. 
                             + as.factor(gender) # 1 = Male; 2 = Female. 
                             + age
                             + hhincome 
                             + as.factor(latino) # 1 = Latino; 2 = Not Latino. 
                             + ideology # Increase corresponds to being more conservative
                             + as.factor(partyid) # 1 = Rep.; 2 = Dem.; 3 = Ind.; 4 = Other. 
                             + as.factor(votechoice) # 1 = Trump; 2 = Clinton; 3 = Other; 4 = Didn't Vote. 
                             + Q66 
                             + Q72_16
                             + Q72_17
                             + Q72_18
                             + Q72_19
                             + Q72_21
                             + Q72_22
                             + Q72_23
                             + Q72_24
                             + Q72_25
                             + Q72_27
                             + Q72_28
                             + Q72_29
                             + Q72_30
                             + Q72_31
                             + Q72_32
                             + as.factor(natpark_ca), # 1 = Yes; 2 = No; 3 = Don't Know.
                             family="gaussian",
                             design = subset(wt_grizz_df, species_griz==2))

summary(ols_nonbelievers_w)












