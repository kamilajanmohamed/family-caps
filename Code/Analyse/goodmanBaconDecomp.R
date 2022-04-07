library(bacondecomp)
library(tidyverse)
library(lfe)
library(fixest)
library(patchwork)

df <- read_csv("Data/Clean/stateAgg.csv")

#Make plots of cohorts and outcome variable over time
#Source: https://asjadnaqvi.github.io/DiD/docs/code/06_bacon/

df %>%
  mutate(YEARCAPPED = case_when(
    is.na(YEARCAPPED) == T ~ 0,
          TRUE ~  YEARCAPPED)) %>%
  filter(YEAR < 2005 & YEAR > 1981) %>%
  ggplot(aes(x = YEAR, y = birthrate, group = STATECENSUS, colour = as.factor(YEARCAPPED))) +
  geom_line()
#this looks like complete chaos
#what if i plotted gorup means?
df %>%
  mutate(YEARCAPPED = case_when(
    is.na(YEARCAPPED) == T ~ 0,
    TRUE ~  YEARCAPPED)) %>%
  filter(YEAR < 2005 & YEAR > 1981) %>%
  group_by(YEAR, YEARCAPPED) %>%
  summarise(means = mean(birthrate)) %>%
  ggplot(aes(x = YEAR, y = means, group = YEARCAPPED, colour = as.factor(YEARCAPPED))) +
  geom_line()
#also looks chaotic, no clear trend

# sort data -------------------------------
#Source: https://mixtape.scunning.com/difference-in-differences.html?panelset4=r-code5&panelset5=r-code6&panelset6=r-code7&panelset7=r-code8#twoway-fixed-effects-with-differential-timing

regDF <- df %>%
  #shorten the window to exclude repeal era observations
  filter(YEAR < 2005 & YEAR > 1981) %>%
  #make a group variable
  mutate(GROUP = case_when(
    is.na(YEARCAPPED) == T ~ 0,
    is.na(YEARCAPPED) == F ~ YEARCAPPED),
    #make uncapped states = 0 to indicate no treatment
    TREAT = case_when(
      is.na(CAPi) == T ~ 0,
      TRUE ~ CAPi
    )) %>%
  #add lags and leads - we go from -22 to 10. 
  mutate(lead22 = case_when(EVENTt == -22 ~ 1,TRUE ~ 0),
         lead21 = case_when(EVENTt == -21 ~ 1,TRUE ~ 0),
         lead20 = case_when(EVENTt == -20 ~ 1,TRUE ~ 0),
         lead19 = case_when(EVENTt == -19 ~ 1,TRUE ~ 0),
         lead18 = case_when(EVENTt == -18 ~ 1,TRUE ~ 0),
         lead17 = case_when(EVENTt == -17 ~ 1,TRUE ~ 0),
         lead16 = case_when(EVENTt == -16 ~ 1,TRUE ~ 0),
         lead15 = case_when(EVENTt == -15 ~ 1,TRUE ~ 0),
         lead14 = case_when(EVENTt == -14 ~ 1,TRUE ~ 0),
         lead13 = case_when(EVENTt == -13 ~ 1,TRUE ~ 0),
         lead12 = case_when(EVENTt == -12 ~ 1,TRUE ~ 0),
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
         lag5 = case_when(EVENTt == 5 ~ 1,TRUE ~ 0),
         lag6 = case_when(EVENTt == 6 ~ 1,TRUE ~ 0),
         lag7 = case_when(EVENTt == 7 ~ 1,TRUE ~ 0),
         lag8 = case_when(EVENTt == 8 ~ 1,TRUE ~ 0),
         lag9 = case_when(EVENTt == 9 ~ 1,TRUE ~ 0),
         lag10 = case_when(EVENTt == 10 ~ 1,TRUE ~ 0),
         lag11 = case_when(EVENTt == 11 ~ 1,TRUE ~ 0))



# Implement TWFEDD event study --------------------------------------------
eventStudy <- felm(birthrate ~ lead22 + lead21 + lead20 + lead19 + lead18 + lead17 + lead16 + lead15 + lead14 + lead13 + lead12 + lead11 + lead10 + lead9 + lead8 + lead7 + lead6 + lead5 + lead4 + lead3 + lead2 + lead1 + lag1 + lag2 + lag3 + lag4 + lag5 +lag6 + lag7 + lag8 + lag9 + lag10 + lag11 | STATECENSUS + YEAR | 0 | STATECENSUS, data = regDF)

summary(eventStudy)

plot_order <- c("lead22", "lead21", "lead20", "lead19", "lead18", "lead17", "lead16", "lead15", "lead14", "lead13", "lead12", "lead11", "lead10", "lead9", "lead8", "lead7", "lead6", "lead5", "lead4", "lead3", "lead2", "lead1", "lag1", "lag2", "lag3", "lag4", "lag5", "lag6", "lag7", "lag8", "lag9", "lag10", "lag11")

leadslags_plot <- tibble(
  sd = c(eventStudy$cse[plot_order], 0),
  mean = c(coef(eventStudy)[plot_order], 0),
  label = c(-22, -21, -20, -19, -18, -17, -16, -15, -14, -13, -12, -11, -10, -9,-8,-7,-6, -5, -4, -3, -2, 0, 1,2,3,4,5, 6, 7, 8, 9, 10,11, -1)
)

#calculate the average of post treatment estimates for comparison to TWFEDD estimate
(postMean <- leadslags_plot %>%
  filter(label > 0) %>%
  summarise(means = mean(mean),
            sd = sd(mean)))

plot1 <- leadslags_plot %>%
  ggplot(aes(x = label, y = mean,
             ymin = mean-1.96*sd, 
             ymax = mean+1.96*sd)) +
  #geom_hline(yintercept = 0.035169444, color = "red") +
  geom_pointrange() +
  theme_minimal() +
  xlab("Time to cap implementation") +
  ylab("Births per 1000 women") +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  geom_vline(xintercept = 0,
             linetype = "dashed")

eventPlot <- leadslags_plot %>%
  ggplot(aes(x = label)) + 
  geom_line(aes(y = mean)) +
  geom_line(aes(y = mean - 1.96*sd), linetype = "dotted") +
  geom_line(aes(y = mean + 1.96*sd), linetype = "dotted") +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 0.97, colour = "red") +
  scale_x_continuous(breaks = seq(-21, 11, 2), labels = seq(-21, 11, 2)) +
  scale_y_continuous(breaks = seq(-40, 70, 10), labels = seq(-40, 70, 10)) +
  labs(y = "Births per 1000 women", x = "Time to treatment", caption = "DD Coefficient = 0.97, se = 2.70 \n The black line corresponds to event time estimates, the dotted lines to 95% confidence intervals and the red line to the TWFEDD estimate.") +
  theme_minimal()

ggsave("Output/Figures/goodmanBaconEventStudy.png", eventPlot, height = 6, width = 9)
  

ddreg <-felm(birthrate ~ TREAT | STATECENSUS + YEAR | 0 | STATECENSUS, data = regDF)

# Implement goodman bacon decomp ------------------------------------------
#Source: https://edjeeongithub.github.io/2019-12-24/Goodman-Bacon-Decomposition-Package
 
df_bacon <- bacon(birthrate ~ TREAT,
                  data = regDF,
                  id_var = "STATECENSUS",
                  time_var = "YEAR")

coef_bacon <- sum(df_bacon$estimate * df_bacon$weight)
print(paste("Weighted sum of decomposition =", round(coef_bacon, 4)))

fit_tw <- lm(birthrate ~ TREAT + factor(STATECENSUS) + factor(YEAR), 
             data = regDF)
print(paste("Two-way FE estimate =", round(fit_tw$coefficients[2], 4)))

#explore weights and estimates
#find 10 highest weights
df_bacon %>%
 top_n(10, weight) %>%
  summarise(sumW = sum(weight))

#omit later treated vs earlier treated
df_bacon %>%
  filter(type != "Later vs Earlier Treated") %>%
  summarise(VWATT = sum(estimate*weight))

#omit earlier treated vs later treated
df_bacon %>%
  filter(type != "Earlier vs Later Treated") %>%
  summarise(VWATT = sum(estimate*weight))

#find number of comparisons by type
df_bacon %>%
  group_by(type) %>%
  summarise(counts = n())

#plot to see if influential estimates differ from VWATT
bubbles <- df_bacon %>% 
  mutate(untreated = case_when(
    untreated == 99999 ~ "0000",
    TRUE ~ as.character(untreated))) %>%
  mutate(subgroup = paste0(treated, "_", untreated),
         subgroup = factor(subgroup),
         subgroup = forcats::fct_reorder(subgroup, estimate)) %>% 
  ggplot(aes(x = estimate, 
             y = subgroup,
             size = weight, 
             colour = type)) +
  geom_point() +
  geom_vline(xintercept = weighted.mean(df_bacon$estimate, df_bacon$weight), colour = "red") +
  scale_size_continuous(breaks = seq(0.05, 0.3, 0.05), labels = seq(0.05, 0.3, 0.05)) +
  scale_x_continuous(breaks = seq(-25, 45, 5), labels = seq(-25, 45, 5)) +
  scale_colour_manual(values = c("#999999", "#E69F00", "#56B4E9"))+
  labs(size = "Weight",
       y = "Subgroup comparisons",
       x = "Estimate",
       #title = "Goodman-Bacon diff in diff decomposition",
       caption = "Red line indicates TWFEDD estimate. \nSubgroups 0000 correspond to never treated groups.") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.box = "vertical")

ggsave("Output/Figures/gbComparisons.png", bubbles, width = 6, height = 9)

#plot weight against estimate

wBe <- df_bacon %>%
  ggplot(aes(x = weight,  y = estimate, shape = type, colour = type)) +
  geom_point() +
  geom_hline(yintercept = weighted.mean(df_bacon$estimate, df_bacon$weight), colour = "red") +
  labs(x = "Weight", y = "2X2 DD Estimate", shape = "Comparison")+
  scale_colour_manual(name = "Comparison", values = c("#999999", "#E69F00", "#56B4E9"))+
  scale_x_continuous(breaks = seq(0, 0.35, 0.05), labels = seq(0, 0.35, 0.05)) +
  scale_y_continuous(breaks = seq(-50, 25, 10), labels = seq(-50, 25, 10)) +
  annotate("text", x =  0.225, y = 3, label = "DD estimate = 0.97", colour = "red", size = 3.5) +
  theme_bw() +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.box = "vertical")

ggsave("Output/Figures/weightByEst.png", wBe, width = 6.25, height = 4)

# Apply sun abraham to state aggregates -----------------------------------
#regDF <- df %>%
  #shorten the window
 # filter(EVENTt >= -12 & EVENTt <= 5) 

# Following Sun and Abraham, we give our never-treated units a fake "treatment" date far outside the relevant study period.
sunAB <- regDF %>%
  mutate(YEARCAPPED = case_when(
    is.na(YEARCAPPED) == T ~ 10000,
    TRUE ~ YEARCAPPED)) %>%
  mutate(EVENTt = YEAR - YEARCAPPED)
  
sunabReg <- feols(birthrate ~ sunab(YEARCAPPED, EVENTt, ref.p = 0) | STATECENSUS + YEAR + YEARCAPPED, cluster = ~STATECENSUS, data = sunAB)

#sunabReg <- feols(birthrate ~ sunab(YEARCAPPED+1, YEAR, ref.p = 0) | STATECENSUS + YEAR + YEARCAPPED, cluster = ~STATECENSUS, data = sunAB)

#make a table
etable(sunabReg)
#total ATT
summary(sunabReg, agg = "ATT")

#ATT by cohort
summary(sunabReg, agg = "cohort")

#plot
iplot(sunabReg, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

#plot sunab1 as a ggplot event study
#get coefficients and standard errors to make event study style plots
coef <- c(sunabReg$coeftable[1:33], 0)
se <- c(sunabReg$coeftable[1:33, 2], 0)
eventT <- c(seq(-22, -1, 1), seq(1,11, 1), 0)
estimate <- rep("IW Estimate", 34)

plotDF <- cbind(coef, se, eventT, estimate) %>% 
  as.data.frame() %>%
  mutate(coef = as.numeric(coef),
         se = as.numeric(se),
         eventT = as.numeric(eventT))

sunabplot <- ggplot(plotDF) +
  geom_line(aes(x = eventT, y = coef)) +
  geom_line(aes(x = eventT, y = coef - 1.96*se), linetype = "dotted") +
  geom_line(aes(x = eventT, y = coef + 1.96*se), linetype = "dotted") +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 3.41, colour = "red") +
  scale_x_continuous(breaks = seq(-21, 11, 2), labels = seq(-21, 11, 2)) +
  scale_y_continuous(breaks = seq(-60, 70, 10), labels = seq(-60, 70, 10)) +
  labs(y = "Births per 1000 women", x = "Time to treatment", caption = "IW estimate = 3.41, se = 5.84 \n The black line corresponds to event time estimates, the dotted lines to 95% confidence intervals and the red line to the IW estimate." ) +
  theme_minimal()

events <- eventPlot / sunabplot + plot_annotation(tag_levels = 'A')


ggsave("Output/Figures/sunabAgg.png", sunabplot, width = 9, height = 6)

# Combine event study plots into the same figure --------------------------
coef <- c(coef(eventStudy)[plot_order], 0)
se <- c(eventStudy$cse[plot_order], 0)
eventT <- c(seq(-22, -1, 1), seq(1,11, 1), 0)
estimate <- rep("TWFEDD Estimate", 34)

twfeEst <- cbind(coef, se, eventT, estimate) %>% 
  as.data.frame() %>%
  mutate(coef = as.numeric(coef),
         se = as.numeric(se),
         eventT = as.numeric(eventT))

eventStudyPlot <- rbind(plotDF,
                        twfeEst)

eventBirth <- ggplot(eventStudyPlot, aes(x = eventT, group = estimate, colour = estimate, shape = estimate)) +
  geom_point(aes(y = coef), position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymax = coef + 2*se, ymin = coef-2*se), width = 0.5, position = "dodge") + 
  scale_colour_manual(values = c("red", "blue")) +
  scale_x_continuous(breaks = seq(-22, 11, 2), labels = seq(-22, 11, 2)) + 
  scale_y_continuous(breaks = seq(-60, 70, 10), labels = seq(-60, 70, 10)) + 
  geom_hline(yintercept = 0) + 
  ylab("Estimate and 95% confidence interval") + 
  xlab("Time to treatment") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(), 
        legend.position = "none")

#plot overall estimates with error bars
coef <- c(4.0, 0.78)
se <- c(6.01, 1.67)
type <- c("IW Estimate", "TWFEDD estimate")
x <- c(0,0)

att <- cbind(coef, se, type, x) %>%
  as.data.frame() %>%
  mutate(coef = as.numeric(coef),
         se = as.numeric(se),
         x = as.numeric(x))

attPlot <- ggplot(att, aes(x = x, group = type, colour = type, shape = type)) +
  geom_point(aes(y = coef), position = position_dodge(width=0.1)) +
  geom_errorbar(aes(ymax = coef + 2*se, ymin = coef-2*se), width = 0.1, position = "dodge") + 
  scale_colour_manual(values = c("red", "blue")) +
  scale_x_continuous(breaks = seq(0,1,1)) + 
  scale_y_continuous(breaks = seq(-10, 15, 5), labels = seq(-10, 15, 5)) + 
  geom_hline(yintercept = 0) + 
  ylab("ATT estimate and 95% confidence interval") + 
  xlab("") +
  theme_bw() +
  theme(legend.position = "right",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank())

events <- eventBirth / ( plot_spacer() + attPlot + plot_spacer())


ggsave("Output/Figures/TWFEvsSunabAggregates.png", events, width = 9, height = 6)

eventBirthLabs <- ggplot(eventStudyPlot, aes(x = eventT, group = estimate, colour = estimate, shape = estimate)) +
  geom_point(aes(y = coef), position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymax = coef + 2*se, ymin = coef-2*se), width = 0.5, position = "dodge") + 
  scale_colour_manual(values = c("red", "blue")) +
  annotate("text", x = 8, y = -63, colour = "red", label = "IW estimate: 3.42 (se=5.84)", size = 2) +
  annotate("text", x = 8, y = -68, colour = "blue", label = "TWFEDD estimate: 0.97 (se=2.70)", size = 2) +
  scale_x_continuous(breaks = seq(-22, 11, 2), labels = seq(-22, 11, 2)) + 
  scale_y_continuous(breaks = seq(-60, 70, 10), labels = seq(-60, 70, 10)) + 
  geom_hline(yintercept = 0) + 
  ylab("Estimate and 95% confidence interval") + 
  xlab("Time to treatment") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(), 
        legend.position = "none")

ggsave("Output/Figures/TWFEvsSunabAggregateES.png", eventBirthLabs, width = 6, height = 3)

#ggplot(combined, aes(x = eventT, y = coefficients, group = type, colour = type, shape = type)) +
#  geom_point() +
#  geom_errorbar(aes(ymin=coefficients-sd, ymax=coefficients+sd), width=.2,
#                position=position_dodge(0.05)) +
#  scale_x_continuous(breaks = seq(-21, 11, 2), labels = seq(-21, 11, 2)) +
#  scale_y_continuous(breaks = seq(-30, 50, 10), labels = seq(-30, 50, 10))
  
  

