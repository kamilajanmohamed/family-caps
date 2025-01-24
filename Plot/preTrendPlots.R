## parallel trends visualisation, unweighted
library(tidyverse)
library(patchwork)

df <- read_csv("Data/Clean/studyDat.csv")


# For whole female sample -------------------------------------------------

subBirth <- df %>%
  select(YEAR, STATECENSUS, YEARCAPPED, YEARREPEALED, BIRTHi) %>%
  mutate(YEARCAPPED = case_when(
    is.na(YEARCAPPED)  == T ~ 0,
    TRUE ~ YEARCAPPED
  ))

#1992, 1994, 1995, 1996, 1997, 1998, 2003

df1992 <- subBirth %>%
  filter(YEARCAPPED == 0 | YEARCAPPED == 1992) %>%
  filter(YEAR <= 1997 & YEAR >= 1987) %>%
  group_by(YEAR, STATECENSUS) %>%
  summarise(pBirth = mean(BIRTHi),
            Group = max(YEARCAPPED)) %>%
  group_by(YEAR, Group) %>%
  summarise(means = mean(pBirth))

p1992 <- ggplot(df1992, aes(x = YEAR, y = means, group = as.factor(Group), colour = as.factor(Group))) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 1992)+
  scale_x_continuous(breaks = seq(1987, 1997, 1), labels = seq(1987, 1997, 1)) +
  labs(y = "Sample birth rate", x = "Year") +
  theme_minimal()+
  scale_colour_discrete(name = "Dose", labels = c("Never treated (N = 27)", "1992 Group (N = 1)")) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_blank())

df1994 <- subBirth %>%
  filter(YEARCAPPED == 0 | YEARCAPPED == 1994) %>%
  filter(YEAR <= 1999 & YEAR >= 1989) %>%
  group_by(YEAR, STATECENSUS) %>%
  summarise(pBirth = mean(BIRTHi),
            Group = max(YEARCAPPED)) %>%
  group_by(YEAR, Group) %>%
  summarise(means = mean(pBirth))

p1994 <- ggplot(df1994, aes(x = YEAR, y = means, group = as.factor(Group), colour = as.factor(Group))) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 1994)+
  scale_x_continuous(breaks = seq(1989, 1999, 1), labels = seq(1989, 1999, 1)) +
  labs(y = "Sample birth rate", x = "Year") +
  theme_minimal()+
  scale_colour_discrete(name = "Dose", labels = c("Never treated (N = 27)", "1994 Group (N = 2)")) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_blank())

df1995 <- subBirth %>%
  filter(YEARCAPPED == 0 | YEARCAPPED == 1995) %>%
  filter(YEAR <= 2000 & YEAR >= 1990) %>%
  group_by(YEAR, STATECENSUS) %>%
  summarise(pBirth = mean(BIRTHi),
            Group = max(YEARCAPPED)) %>%
  group_by(YEAR, Group) %>%
  summarise(means = mean(pBirth))

p1995 <- ggplot(df1995, aes(x = YEAR, y = means, group = as.factor(Group), colour = as.factor(Group))) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 1995)+
  scale_x_continuous(breaks = seq(1990, 2000, 1), labels = seq(1990, 2000, 1)) +
  labs(y = "Sample birth rate", x = "Year") +
  theme_minimal()+
  scale_colour_discrete(name = "Dose", labels = c("Never treated (N = 27)", "1995 Group (N = 7)")) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_blank())


df1996 <- subBirth %>%
  filter(YEARCAPPED == 0 | YEARCAPPED == 1996) %>%
  filter(YEAR <= 2001 & YEAR >= 1991) %>%
  group_by(YEAR, STATECENSUS) %>%
  summarise(pBirth = mean(BIRTHi),
            Group = max(YEARCAPPED)) %>%
  group_by(YEAR, Group) %>%
  summarise(means = mean(pBirth))

p1996 <- ggplot(df1996, aes(x = YEAR, y = means, group = as.factor(Group), colour = as.factor(Group))) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 1996)+
  scale_x_continuous(breaks = seq(1991, 2001, 1), labels = seq(1991, 2001, 1)) +
  labs(y = "Sample birth rate", x = "Year") +
  theme_minimal()+
  scale_colour_discrete(name = "Dose", labels = c("Never treated (N = 27)", "1996 Group (N = 7)")) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_blank())

df1997 <- subBirth %>%
  filter(YEARCAPPED == 0 | YEARCAPPED == 1997) %>%
  filter(YEAR <= 2002 & YEAR >= 1992) %>%
  group_by(YEAR, STATECENSUS) %>%
  summarise(pBirth = mean(BIRTHi),
            Group = max(YEARCAPPED)) %>%
  group_by(YEAR, Group) %>%
  summarise(means = mean(pBirth))

p1997 <- ggplot(df1997, aes(x = YEAR, y = means, group = as.factor(Group), colour = as.factor(Group))) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 1997)+
  scale_x_continuous(breaks = seq(1992, 2002, 1), labels = seq(1992, 2002, 1)) +
  labs(y = "Sample birth rate", x = "Year") +
  theme_minimal()+
  scale_colour_discrete(name = "Dose", labels = c("Never treated (N = 27)", "1997 Group (N = 5)")) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_blank())

df1998 <- subBirth %>%
  filter(YEARCAPPED == 0 | YEARCAPPED == 1998) %>%
  filter(YEAR <= 2003 & YEAR >= 1993) %>%
  group_by(YEAR, STATECENSUS) %>%
  summarise(pBirth = mean(BIRTHi),
            Group = max(YEARCAPPED)) %>%
  group_by(YEAR, Group) %>%
  summarise(means = mean(pBirth))

p1998 <- ggplot(df1998, aes(x = YEAR, y = means, group = as.factor(Group), colour = as.factor(Group))) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 1998)+
  scale_x_continuous(breaks = seq(1993, 2003, 1), labels = seq(1993, 2003, 1)) +
  labs(y = "Sample birth rate", x = "Year") +
  theme_minimal()+
  scale_colour_discrete(name = "Dose", labels = c("Never treated (N = 27)", "1998 Group (N = 1)")) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_blank())

df2003 <- subBirth %>%
  filter(YEARCAPPED == 0 | YEARCAPPED == 2003) %>%
  filter(YEAR <= 2008 & YEAR >= 1998) %>%
  group_by(YEAR, STATECENSUS) %>%
  summarise(pBirth = mean(BIRTHi),
            Group = max(YEARCAPPED)) %>%
  group_by(YEAR, Group) %>%
  summarise(means = mean(pBirth))

p2003 <- ggplot(df2003, aes(x = YEAR, y = means, group = as.factor(Group), colour = as.factor(Group))) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 2003)+
  scale_x_continuous(breaks = seq(1998, 2008, 1), labels = seq(1998, 2008, 1)) +
  labs(y = "Sample birth rate", x = "Year") +
  theme_minimal()+
  scale_colour_discrete(name = "Dose", labels = c("Never treated (N = 27)", "2003 Group (N = 1)")) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_blank())

p1992 + p1994 + p1995 + p1996 + p1997 + p1998 + p2003 + 
  plot_layout(ncol = 2)


