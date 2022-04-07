#Plot TWFEDD and IW event study estimates
library(tidyverse)
library(fixest)

df <- read_csv("Data/Clean/mergedBirth.csv")


# sunAB prep --------------------------------------------------------------
#create a time to treatment variable. Set to 0 for never treated units
regDF <- df %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #shorten the window
  #filter(EVENTt >= -11 & EVENTt <= 6 & EDUCATIONi == 0 & MARSTi == 0) %>%
  filter(YEAR > 1981 & YEAR < 2011 & EDUCATIONi == 0 & MARSTi == 0)

#make a subset of treated with values for the desired event time window
treated <- regDF %>%
  filter(TREAT == 1 & EVENTt >= -11 & EVENTt <= 6)

untreated <- regDF %>%
  filter(TREAT == 0)

regDF <- rbind(treated, untreated) %>%
  # Following Sun and Abraham, we give our never-treated units a fake "treatment" date far outside the relevant study period.
  mutate(YEARCAPPED = case_when(
    TREAT == 1 ~ YEARCAPPED,
    TREAT == 0 ~ 10000)) %>%
  #sunab function needs never-treated to always have negative relative periods. 
  mutate(EVENTt = case_when(
    TREAT == 1 ~ EVENTt,
    TRUE ~ YEAR - YEARCAPPED))


# sunab estimates ---------------------------------------------------------
sunAB <- feols(BIRTHi ~ sunab(YEARCAPPED, EVENTt) + AGE + RACE_ETH + NCHILD + TIMELIMITi + femUnemployment + MAXBENEFIT + MEDIANWAGE|
                STATECENSUS + YEAR,
              cluster = ~STATECENSUS,                          ## Clustered SEs
              data = regDF)


# TWFEDD prep -------------------------------------------------------------
#create a time to treatment variable. Set to 0 for never treated units
regDF <- df %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #shorten the window
  #filter(EVENTt >= -11 & EVENTt <= 6 & EDUCATIONi == 0 & MARSTi == 0) %>%
  filter(YEAR > 1981 & YEAR < 2011 & EDUCATIONi == 0 & MARSTi == 0)

#make a subset of treated with values for the desired event time window
treated <- regDF %>%
  filter(TREAT == 1 & EVENTt >= -11 & EVENTt <= 6)
#note that time to treatment for the never treated is set to 0. 
untreated <- regDF %>%
  filter(TREAT == 0)

regDF <- rbind(treated, untreated) %>%
  # Following Sun and Abraham, we give our never-treated units a fake "treatment" date far outside the relevant study period.
  mutate(YEARCAPPED = case_when(
    TREAT == 1 ~ YEARCAPPED,
    TREAT == 0 ~ 0))


# TWFEDD estimate ---------------------------------------------------------
twfeDD <- feols(BIRTHi ~ i(EVENTt, TREAT, ref = -1) + AGE + RACE_ETH + NCHILD + TIMELIMITi + femUnemployment + MAXBENEFIT + MEDIANWAGE|
                STATECENSUS + YEAR,
              cluster = ~STATECENSUS,                          ## Clustered SEs
              data = regDF)

# Plot --------------------------------------------------------------------
iplot(list(sunAB, twfeDD))

#extract estimates 
coef <- c(sunAB$coeftable[1:17], 0)
se <- c(sunAB$coeftable[1:17,2],NA)
type <- rep("IW Estimate", 18)
eventT <- c(seq(-11, -2, 1), seq(0,6,1), -1)
sunab <- cbind(coef, se, type, eventT) %>% 
  as.data.frame()

coef <- c(twfeDD$coeftable[1:17], 0)
se <- c(twfeDD$coeftable[1:17,2], NA)
type <- rep("TWFEDD Estimate", 18)

twfedd <- cbind(coef, se, type, eventT) %>% 
  as.data.frame()

eventStudy <- rbind(sunab, twfedd) %>%
  mutate(coef = as.numeric(coef), 
         se = as.numeric(se),
         eventT = as.numeric(eventT))

eventBirth <- ggplot(eventStudy, aes(x = eventT, group = type, colour = type, shape = type)) +
  geom_point(aes(y = coef), position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymax = coef + 2*se, ymin = coef-2*se), width = 0.5, position = "dodge") + 
  scale_colour_manual(values = c("red", "blue")) +
  scale_x_continuous(breaks = seq(-11, 6, 1), labels = seq(-11, 6, 1)) + 
  scale_y_continuous(breaks = c(-0.01, 0.00, 0.01, 0.02), labels = c(-0.01, 0.00, 0.01, 0.02)) + 
  geom_hline(yintercept = 0) + 
  ylab("Estimate and 95% confidence interval") + 
  xlab("Time to treatment") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank())

ggsave("Output/Figures/eventStudyBirth.png", eventBirth, height = 4, width = 5)
