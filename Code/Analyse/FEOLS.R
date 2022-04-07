#manual event study
#Source: https://mixtape.scunning.com/difference-in-differences.html?panelset4=r-code5&panelset5=r-code6&panelset6=r-code7&panelset7=r-code8#bacon-decomposition

library(tidyverse)
library(lfe)
library(stargazer)

df <- read_csv("Data/Clean/studyDat.csv")

df$STATECENSUS <- as.factor(df$STATECENSUS)
df$YEAR <- as.factor(df$YEAR)
df$STATEL <- as.factor(df$STATEL)
df$TIMELIMITi <- as.factor(df$TIMELIMITi)

regDF <- df %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #shorten the window
  filter(EVENTt >= -12 & EVENTt <= 5 & EDUCATIONi == 0) %>%
  #create lags and leads
  mutate(lead12 = case_when(EVENTt == -12 ~ 1,TRUE ~ 0),
         lead11 = case_when(EVENTt == -11 ~ 1,TRUE ~ 0),
         lead10 = case_when(EVENTt == -10 ~ 1,TRUE ~ 0),
         lead9 = case_when(EVENTt == -9 ~ 1,TRUE ~ 0),
         lead8 = case_when(EVENTt == -8 ~ 1,TRUE ~ 0),
         lead7 = case_when(EVENTt == -7 ~ 1,TRUE ~ 0),
         lead6 = case_when(EVENTt == -6 ~ 1,TRUE ~ 0),
         lead5 = case_when(EVENTt == -5 ~ 1,TRUE ~ 0),
         lead4 = case_when(EVENTt == -4 ~ 1,TRUE ~ 0),
         lead3 = case_when(EVENTt == -3 ~ 1,TRUE ~ 0),
         lead2 = case_when(EVENTt == -2 ~ 1,TRUE ~ 0),
         lead1 = case_when(EVENTt == -1 ~ 1,TRUE ~ 0),
         lag0 = case_when(EVENTt == 0 ~ 1,TRUE ~ 0),
         lag1 = case_when(EVENTt == 1 ~ 1,TRUE ~ 0),
         lag2 = case_when(EVENTt == 2 ~ 1,TRUE ~ 0),
         lag3 = case_when(EVENTt == 3 ~ 1,TRUE ~ 0),
         lag4 = case_when(EVENTt == 4 ~ 1,TRUE ~ 0),
         lag5 = case_when(EVENTt == 5 ~ 1,TRUE ~ 0)) %>%
  #make a variable "post" = 1 if cap is active 
  mutate(post = as.factor(case_when(as.numeric(as.character(YEAR)) >= YEARCAPPED ~ 1, TRUE ~ 0)))

#regression order:
#1: only state and year FE
#2" state and year FE, individual controls
#3: state and year FE, state level controls
#4: state and year FE, individual controls, state level controls


# State and Year FE -------------------------------------------------------
reg1 <- felm(BIRTHi ~ lead12 + lead11 + lead10 + lead9 + lead8 + lead7 + lead6 + lead5 + lead4 + lead3 + lead2 + lead1 + lag1 + lag2 + lag3 + lag4 + lag5 | STATECENSUS + YEAR | 0 | STATECENSUS, data = regDF)

summary(reg1)

plot_order <- c("lead12", "lead11", "lead10", "lead9", "lead8", "lead7", 
                "lead6", "lead5", "lead4", "lead3", 
                "lead2", "lead1", "lag1", 
                "lag2", "lag3", "lag4", "lag5")

leadslags_plot <- tibble(
  sd = c(reg1$cse[plot_order], 0),
  mean = c(coef(reg1)[plot_order], 0),
  label = c(-12, -11, -10, -9,-8,-7,-6, -5, -4, -3, -2, -1, 1,2,3,4,5, 0)
)

plot1 <- leadslags_plot %>%
  ggplot(aes(x = label, y = mean,
             ymin = mean-1.96*sd, 
             ymax = mean+1.96*sd)) +
  #geom_hline(yintercept = 0.035169444, color = "red") +
  geom_pointrange() +
  theme_minimal() +
  xlab("Time to cap implementation") +
  ylab("p(birth)") +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  geom_vline(xintercept = 0,
             linetype = "dashed")

# Individual controls -----------------------------------------------------
reg2 <- felm(BIRTHi ~ AGE + RACE + NCHILD + YNGCH + ELDCH + lead12 + lead11 + lead10 + lead9 + lead8 + lead7 + lead6 + lead5 + lead4 + lead3 + lead2 + lead1 + lag1 + lag2 + lag3 + lag4 + lag5 | STATECENSUS + YEAR | 0 | STATECENSUS, data = regDF)

summary(reg2)

plot_order <- c("lead12", "lead11", "lead10", "lead9", "lead8", "lead7", 
                "lead6", "lead5", "lead4", "lead3", 
                "lead2", "lead1", "lag1", 
                "lag2", "lag3", "lag4", "lag5")

leadslags_plot <- tibble(
  sd = c(reg2$cse[plot_order], 0),
  mean = c(coef(reg2)[plot_order], 0),
  label = c(-12, -11, -10, -9,-8,-7,-6, -5, -4, -3, -2, -1, 1,2,3,4,5, 0)
)

plot2 <- leadslags_plot %>%
  ggplot(aes(x = label, y = mean,
             ymin = mean-1.96*sd, 
             ymax = mean+1.96*sd)) +
  #geom_hline(yintercept = 0.035169444, color = "red") +
  geom_pointrange() +
  theme_minimal() +
  xlab("Time to cap implementation") +
  ylab("p(birth)") +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  geom_vline(xintercept = 0,
             linetype = "dashed")


# State controls ----------------------------------------------------------
reg3 <- felm(BIRTHi ~  TIMELIMITi + femUnemployment + MAXBENEFIT + MEDIANWAGE + lead12 + lead11 + lead10 + lead9 + lead8 + lead7 + lead6 + lead5 + lead4 + lead3 + lead2 + lead1 + lag1 + lag2 + lag3 + lag4 + lag5 | STATECENSUS + YEAR | 0 | STATECENSUS, data = regDF)

summary(reg3)

plot_order <- c("lead12", "lead11", "lead10", "lead9", "lead8", "lead7", 
                "lead6", "lead5", "lead4", "lead3", 
                "lead2", "lead1", "lag1", 
                "lag2", "lag3", "lag4", "lag5")

leadslags_plot <- tibble(
  sd = c(reg3$cse[plot_order], 0),
  mean = c(coef(reg3)[plot_order], 0),
  label = c(-12, -11, -10, -9,-8,-7,-6, -5, -4, -3, -2, -1, 1,2,3,4,5, 0)
)

plot3 <- leadslags_plot %>%
  ggplot(aes(x = label, y = mean,
             ymin = mean-1.96*sd, 
             ymax = mean+1.96*sd)) +
  #geom_hline(yintercept = 0.035169444, color = "red") +
  geom_pointrange() +
  theme_minimal() +
  xlab("Time to cap implementation") +
  ylab("p(birth)") +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  geom_vline(xintercept = 0,
             linetype = "dashed")


# All controls ------------------------------------------------------------
reg4 <- felm(BIRTHi ~  AGE + RACE + NCHILD + YNGCH + ELDCH + TIMELIMITi + femUnemployment + MAXBENEFIT + MEDIANWAGE + lead12 + lead11 + lead10 + lead9 + lead8 + lead7 + lead6 + lead5 + lead4 + lead3 + lead2 + lead1 + lag1 + lag2 + lag3 + lag4 + lag5 | STATECENSUS + YEAR | 0 | STATECENSUS, data = regDF)

summary(reg4)

plot_order <- c("lead12", "lead11", "lead10", "lead9", "lead8", "lead7", 
                "lead6", "lead5", "lead4", "lead3", 
                "lead2", "lead1", "lag1", 
                "lag2", "lag3", "lag4", "lag5")

leadslags_plot <- tibble(
  sd = c(reg4$cse[plot_order], 0),
  mean = c(coef(reg4)[plot_order], 0),
  label = c(-12, -11, -10, -9,-8,-7,-6, -5, -4, -3, -2, -1, 1,2,3,4,5, 0)
)

plot4 <- leadslags_plot %>%
  ggplot(aes(x = label, y = mean,
             ymin = mean-1.96*sd, 
             ymax = mean+1.96*sd)) +
  #geom_hline(yintercept = 0.035169444, color = "red") +
  geom_pointrange() +
  theme_minimal() +
  xlab("Time to cap implementation") +
  ylab("p(birth)") +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  geom_vline(xintercept = 0,
             linetype = "dashed")

#Fixed effect regression using post as treatment variable 
dd1 <- felm(BIRTHi ~ AGE + RACE + NCHILD + YNGCH + ELDCH + TIMELIMITi + femUnemployment + MAXBENEFIT + MEDIANWAGE + post | STATECENSUS + YEAR | 0 | STATECENSUS, data = regDF)
summary(dd1)


# Make table --------------------------------------------------------------
stargazer(reg1, reg2, reg3, reg4, 
          se=list(reg1$cse,reg2$cse, reg3$cse, reg4$cse), 
          title="Regression Results", type="text",
          df=FALSE, digits=3)





