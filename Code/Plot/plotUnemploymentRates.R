#Plot female unemployment rates over time
library(ggplot2)

df <- read_csv("Data/Clean/mergedEmployment.csv")
#create a time to treatment variable. Set to 0 for never treated units
regDF <- df %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #shorten the window
  #filter(EVENTt >= -11 & EVENTt <= 6 & EDUCATIONi == 0 & MARSTi == 0) %>%
  filter(YEAR > 1981 & YEAR < 2011 & EDUCATIONi == 0 & MARSTi == 0) %>%
  mutate(TREAT = case_when(
    TREAT == 0 ~ "Untreated states",
    TRUE~ "Treated states"
  ))


# Unemployment rate - state aggregate -------------------------------------
sumState <- regDF %>%
  group_by(YEAR, STATECENSUS) %>%
  summarise(treat = max(TREAT),
            femUnemp = max(femUnemployment),
            maxBene = max(MAXBENEFIT),
            maxWage = max(MEDIANWAGE)) %>%
  group_by(YEAR, treat) %>%
  summarise(unemp = mean(femUnemp),
            maxBene = mean(maxBene),
            maxWage = mean(maxWage)) %>%
  mutate(treat = as.factor(treat))

sumYear <- regDF %>%
  group_by(YEAR, STATECENSUS) %>%
  summarise(treat = max(TREAT),
            femUnemp = max(femUnemployment),
            maxBene = max(MAXBENEFIT),
            maxWage = max(MEDIANWAGE)) %>%
  mutate(STATECENSUS = as.factor(STATECENSUS),
         treat = as.factor(treat)) %>%
  ungroup()


femUnemp <- ggplot() +
  geom_line(data = sumYear,  aes(x = YEAR, y = femUnemp, group = STATECENSUS, colour = treat), alpha = 0.2) +
  geom_line(data = sumState, aes(x = YEAR, y = unemp, group = treat, colour = treat), size = 1) +
  scale_colour_manual(values = c("red", "blue")) +
  geom_vline(xintercept = 1992) + 
  ylab("Female unemployment rate") +
  xlab("Year") + 
  ggtitle("A: Female unemployment rate (All women)") +
  scale_x_continuous(breaks = seq(1982, 2010, 3), labels = seq(1982, 2010, 3)) +
  scale_y_continuous(breaks = seq(2, 14, 2), labels = seq(2,14,2)) +
  theme_bw() + 
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

#ggsave("Output/Figures/Unemployment.png", femUnemp, width = 4.5, height = 3)


# P(employment) -----------------------------------------------------------
pEmp <- regDF

stateMean <- regDF %>%
  group_by(YEAR, STATECENSUS) %>%
  summarise(treat = max(TREAT), 
            emp = mean(EMPLi)) %>%
  mutate(STATECENSUS = as.factor(STATECENSUS),
         treat = as.factor(treat))

groupMean <- regDF %>%
  group_by(YEAR, TREAT) %>%
  summarise(emp = mean(EMPLi)) %>%
  mutate(treat = as.factor(TREAT)) %>%
  ungroup()

unweighted <- ggplot() +
  geom_line(data = stateMean,  aes(x = YEAR, y = emp, group = STATECENSUS, colour = treat), alpha = 0.2) +
  geom_line(data = groupMean, aes(x = YEAR, y = emp, group = TREAT, colour = TREAT), size = 1) +
  scale_colour_manual(values = c("red", "blue")) +
  ylab("Female unemployment rate") +
  xlab("Year") + 
  ggtitle("Average probability of employment (Women aged 18-45)") +
  scale_x_continuous(breaks = seq(1982, 2010, 3), labels = seq(1982, 2010, 3)) +
  scale_y_continuous(breaks = seq(0.3, 0.9, 0.1), labels = seq(0.3, 0.9, 0.1)) +
  theme_bw() + 
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# State employment rate ---------------------------------------------------
pEmp <- regDF
pEmp$empl <- ifelse(pEmp$EMPLi==1,  pEmp$WTFINL, 0)

agg <- pEmp %>%
  mutate(YEARCAPPED = as.character(YEARCAPPED)) %>%
  mutate(YEARCAPPED = case_when(
    is.na(YEARCAPPED) == T ~ "Never treated",
    TRUE ~ YEARCAPPED)) %>%
  group_by(STATECENSUS, YEAR) %>%
  summarise(nF = sum(WTFINL), 
            nEmp = sum(empl),
            TREAT = max(TREAT),
            YEARCAPPED = unique(YEARCAPPED)) %>%
  #express birth rate as births per 1000 women
  mutate(empRate = 100*nEmp/nF)


stateMean <- agg %>%
  group_by(YEAR, STATECENSUS) %>%
  summarise(treat = max(TREAT), 
            emp = mean(empRate)) %>%
  mutate(STATECENSUS = as.factor(STATECENSUS),
         treat = as.factor(treat))

groupMean <- agg %>%
  group_by(YEAR, TREAT) %>%
  summarise(emp = mean(empRate)) %>%
  mutate(treat = as.factor(TREAT)) %>%
  ungroup()

cohortMean <- agg %>%
  group_by(YEAR, YEARCAPPED) %>%
  summarise(emp = mean(empRate)) %>%
  mutate(YEARCAPPED = as.factor(YEARCAPPED))
  

weighted <- ggplot() +
  geom_line(data = stateMean,  aes(x = YEAR, y = emp, group = STATECENSUS, colour = treat), alpha = 0.2) +
  geom_line(data = groupMean, aes(x = YEAR, y = emp, group = TREAT, colour = TREAT), size = 1) +
  scale_colour_manual(values = c("red", "blue")) +
  geom_vline(xintercept = 1992) + 
  ylab("Female employment rate") +
  xlab("Year") + 
  ggtitle("B: Female employment rate (Women aged 18-45)") + 
  scale_x_continuous(breaks = seq(1982, 2010, 3), labels = seq(1982, 2010, 3)) +
  scale_y_continuous(breaks = seq(30, 85, 10), labels = seq(30, 85, 10)) +
  theme_bw() + 
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

fig <- femUnemp/weighted

ggsave("Output/Figures/employmentSeries.png", fig, height = 7, width = 6)

