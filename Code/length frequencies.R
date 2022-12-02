# Length Frequency analysis
library(tidyverse)
library(lubridate)

fprop <- read_csv("Data/length frequencies/Female Proportions.csv") %>%
  pivot_longer(2:135, names_to = "Month", values_to = "Proportion")

fsample <- read_csv("Data/length frequencies/Female Sample Data.csv") %>%
  rename(Month = Interval, Total_Fish = Fish) %>% select(1:3)

fdat <- fprop %>% left_join(fsample) %>% mutate(Actual_Fish = round(Proportion * Total_Fish)) %>%
  select(-2) %>%
  uncount(Actual_Fish) %>% select(-1,-4,-5,-6) %>%
  rename(Period = Month) %>% mutate(Year = as.numeric(str_sub(Period,1,4)),
                                    Month = as.numeric(str_sub(Period, 6,7)),
                                    Sex = "Female")

mprop <- read_csv("Data/length frequencies/Male Proportions.csv") %>%
  pivot_longer(2:135, names_to = "Month", values_to = "Proportion")

msample <- read_csv("Data/length frequencies/Male Sample Data.csv") %>%
  rename(Month = Interval, Total_Fish = Fish) %>% select(1:3)

mdat <- mprop %>% left_join(fsample) %>% mutate(Actual_Fish = round(Proportion * Total_Fish)) %>%
  select(-2) %>%
  uncount(Actual_Fish) %>% select(-1,-4,-5,-6) %>%
  rename(Period = Month) %>% mutate(Year = as.numeric(str_sub(Period,1,4)),
                                    Month = as.numeric(str_sub(Period, 6,7)),
                                    Sex= "Male")

all_dat <- bind_rows(fdat, mdat)

#mutate(Period = as_date(Period, format = "%Y-%m"))

ggplot(all_dat, aes(`Length Class`*10, col=Sex, group = Sex, linetype=Sex)) + geom_density() + facet_wrap(~Month) +
  xlab("Length (mm)") + theme_classic()+
  theme(axis.title = element_text(face="bold", size=14),
        axis.text = element_text(size=12, colour="black"),
        panel.border = element_rect(fill=NA, colour="black"),
        legend.position = "bottom",
        strip.text = element_text(face="bold", size=12),
        legend.title = element_text(face="bold", size=12),
        legend.text = element_text(size=10))
ggsave("Monthly size frequencies all years.pdf",
       dpi=600, width=21, height=23, units = "cm")

table(fdat$Year)
table(fdat$Month)

all_dat17 <- all_dat %>% filter(Year < 2017)
ggplot(all_dat17, aes(`Length Class`*10, col=Sex, group = Sex)) + geom_density() + facet_wrap(~Month) +
  xlab("Length (mm)") + theme_classic()+
  theme(axis.title = element_text(face="bold", size=14),
        axis.text = element_text(size=12, colour="black"),
        panel.border = element_rect(fill=NA, colour="black"),
        legend.position = "bottom",
        strip.text = element_text(face="bold", size=12),
        legend.title = element_text(face="bold", size=12),
        legend.text = element_text(size=10))
ggsave("Monthly size frequencies pre2017.pdf",
       dpi=600, width=21, height=23, units = "cm")
                                                   

all_dat18 <- all_dat %>% filter(Year == 2018)          
ggplot(all_dat18, aes(`Length Class`, col=Sex, group = Sex)) + geom_density() + facet_wrap(~Month) +
  xlab("Length (cm)") + theme_classic()+
  geom_vline(xintercept = 6.5, linetype=3)+
  theme(axis.title = element_text(face="bold", size=14),
        axis.text = element_text(size=12, colour="black"),
        panel.border = element_rect(fill=NA, colour="black"),
        legend.position = "bottom")
#ggsave("Data/length frequencies/Monthly size frequencies 2018.png",
#       dpi=600, width=21, height=23, units = "cm")

all_dat19 <- all_dat %>% filter(Year == 2019)          
ggplot(all_dat19, aes(`Length Class`, col=Sex, group = Sex)) + geom_density() + facet_wrap(~Month) +
  xlab("Length (cm)") + theme_classic()+
  theme(axis.title = element_text(face="bold", size=14),
        axis.text = element_text(size=12, colour="black"),
        panel.border = element_rect(fill=NA, colour="black"),
        legend.position = "bottom")
#ggsave("Data/length frequencies/Monthly size frequencies 2019.png",
#       dpi=600, width=21, height=23, units = "cm")
