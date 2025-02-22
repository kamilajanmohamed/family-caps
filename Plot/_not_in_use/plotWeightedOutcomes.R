#Plot weighted means for p birth, p emp and ahrsworkt
library(tidyverse)
library(patchwork)


# p birth -------------------------------------------------------------------
df <- read_csv("Data/Clean/mergedBirth.csv")
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

regDF$birth <- ifelse(regDF$BIRTHi==1,  regDF$WTFINL, 0)

agg <- regDF %>%
  mutate(YEARCAPPED = as.character(YEARCAPPED)) %>%
  mutate(YEARCAPPED = case_when(
    is.na(YEARCAPPED) == T ~ "Never treated",
    TRUE ~ YEARCAPPED)) %>%
  group_by(STATECENSUS, YEAR) %>%
  summarise(nF = sum(WTFINL), 
            nBirth = sum(birth),
            TREAT = max(TREAT),
            YEARCAPPED = unique(YEARCAPPED)) %>%
  #express birth rate as births per 1000 women
  mutate(pBirth = nBirth/nF)


stateMean <- agg %>%
  group_by(YEAR, STATECENSUS) %>%
  summarise(treat = max(TREAT), 
            birth = mean(pBirth)) %>%
  mutate(STATECENSUS = as.factor(STATECENSUS),
         treat = as.factor(treat))

groupMean <- agg %>%
  group_by(YEAR, TREAT) %>%
  summarise(birth = mean(pBirth)) %>%
  mutate(treat = as.factor(TREAT)) %>%
  ungroup()

cohortMean <- agg %>%
  group_by(YEAR, YEARCAPPED) %>%
  summarise(birth = mean(pBirth)) %>%
  mutate(YEARCAPPED = as.factor(YEARCAPPED))


birth <- ggplot() +
  geom_line(data = stateMean,  aes(x = YEAR, y = birth, group = STATECENSUS, colour = treat), alpha = 0.2) +
  geom_line(data = groupMean, aes(x = YEAR, y = birth, group = TREAT, colour = TREAT), size = 1) +
  scale_colour_manual(values = c("red", "blue")) +
  geom_vline(xintercept = 1992) + 
  ylab("Probability of birth") +
  xlab("Year") + 
  ggtitle("A: Probability of birth") + 
  scale_x_continuous(breaks = seq(1982, 2010, 4), labels = seq(1982, 2010, 4)) +
  scale_y_continuous(breaks = seq(0, 0.2, 0.05), labels = seq(0, 0.2, 0.05)) +
  theme_bw() + 
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


# empl --------------------------------------------------------------------
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

regDF$empl <- ifelse(regDF$EMPLi==1,  regDF$WTFINL, 0)

agg <- regDF %>%
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
  mutate(empRate = nEmp/nF)


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


emp <- ggplot() +
  geom_line(data = stateMean,  aes(x = YEAR, y = emp, group = STATECENSUS, colour = treat), alpha = 0.2) +
  geom_line(data = groupMean, aes(x = YEAR, y = emp, group = TREAT, colour = TREAT), size = 1) +
  scale_colour_manual(values = c("red", "blue")) +
  geom_vline(xintercept = 1992) + 
  ylab("Probability of employment") +
  xlab("Year") + 
  ggtitle("B: Probability of employment") + 
  scale_x_continuous(breaks = seq(1982, 2010, 4), labels = seq(1982, 2010, 4)) +
  scale_y_continuous(breaks = seq(0.3, 0.85, 0.1), labels = seq(0.3, 0.85, 0.1)) +
  theme_bw() + 
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# Ahrsworkt ---------------------------------------------------------------
regDF <- df %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #shorten the window
  #filter(EVENTt >= -11 & EVENTt <= 6 & EDUCATIONi == 0 & MARSTi == 0) %>%
  filter(YEAR > 1988 & YEAR < 2011 & EDUCATIONi == 0 & MARSTi == 0) %>%
  mutate(TREAT = case_when(
    TREAT == 0 ~ "Untreated states",
    TRUE~ "Treated states")) %>%
  filter(is.na(AHRSWORKT) == F)

agg <- regDF %>%
  mutate(YEARCAPPED = as.character(YEARCAPPED)) %>%
  mutate(YEARCAPPED = case_when(
    is.na(YEARCAPPED) == T ~ "Never treated",
    TRUE ~ YEARCAPPED)) %>%
  mutate(weightedHours = AHRSWORKT * WTFINL) %>%
  group_by(STATECENSUS, YEAR) %>%
  summarise(nF = sum(WTFINL), 
            nAhrs = sum(weightedHours),
            TREAT = max(TREAT),
            YEARCAPPED = unique(YEARCAPPED)) %>%
  #express birth rate as births per 1000 women
  mutate(meanAhrs = nAhrs/nF)

stateMean <- agg %>%
  group_by(YEAR, STATECENSUS) %>%
  summarise(treat = max(TREAT), 
            meanAhrs = mean(meanAhrs)) %>%
  mutate(STATECENSUS = as.factor(STATECENSUS),
         treat = as.factor(treat))

groupMean <- agg %>%
  group_by(YEAR, TREAT) %>%
  summarise(meanAhrs = mean(meanAhrs)) %>%
  mutate(treat = as.factor(TREAT)) %>%
  ungroup()

cohortMean <- agg %>%
  group_by(YEAR, YEARCAPPED) %>%
  summarise(meanAhrs = mean(meanAhrs)) %>%
  mutate(YEARCAPPED = as.factor(YEARCAPPED))


ahrs <- ggplot() +
  geom_line(data = stateMean,  aes(x = YEAR, y = meanAhrs, group = STATECENSUS, colour = treat), alpha = 0.2) +
  geom_line(data = groupMean, aes(x = YEAR, y = meanAhrs, group = TREAT, colour = TREAT), size = 1) +
  scale_colour_manual(values = c("red", "blue")) +
  geom_vline(xintercept = 1992) + 
  ylab("Number of hours worked weekly") +
  xlab("Year") + 
  ggtitle("C: Average hours worked weekly") + 
  scale_x_continuous(breaks = seq(1989, 2010, 3), labels = seq(1989, 2010, 3)) +
  scale_y_continuous(breaks = seq(25, 40, 5), labels = seq(25, 40, 5)) +
  theme_bw() + 
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

fig <- birth/emp/ahrs

ggsave("Output/Figures/weightedOutcomeMeans.png", fig, height = 9, width = 6)
