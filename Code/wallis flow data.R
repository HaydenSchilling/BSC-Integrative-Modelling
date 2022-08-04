# Wallis FLow Data

library(tidyverse)
library(lubridate)

fdat <- read_csv("Data/Wallis Flow Data Clean.csv") %>%
  mutate(Date = as.Date(dmy_hm(`Date and time`))) %>%
  filter(Date > "1995-12-31") %>% drop_na()

ggplot(fdat, aes(Date, `Mean Discharge (ML/D)`)) + geom_line()+
  theme_classic() +
  theme(axis.text = element_text(size=12, colour="black"),
        axis.title = element_text(size=14, face="bold"))
#ggsave("Conceptual model/latest data from DJ/Wallis Flow Mean long.png",
#       dpi=600, width = 21, height=14.8, units="cm")

ggplot(fdat, aes(Date, `Max Discharge (ML/D)`)) + geom_line()+
  theme_classic() +
  theme(axis.text = element_text(size=12, colour="black"),
        axis.title = element_text(size=14, face="bold"))
#ggsave("Conceptual model/latest data from DJ/Wallis Flow Max long.png",
#       dpi=600, width = 21, height=14.8, units="cm")

quantile(fdat$`Mean Discharge (ML/D)`, c(0.1,0.5,0.9)) # top 90% is 279.43

fdat <- fdat %>% mutate(High_Flow_Event = case_when(`Mean Discharge (ML/D)`> 279 ~ 1,
                                                    T ~ 0))

write_csv(fdat, "Data/Wallis Flow Data Clean with events.csv")
ggplot(fdat, aes(Date, High_Flow_Event)) + geom_line()+
  theme_classic() +
  theme(axis.text = element_text(size=12, colour="black"),
        axis.title = element_text(size=14, face="bold"))
#ggsave("Conceptual model/latest data from DJ/Wallis Flow High Flow Events.png",  
#       dpi=600, width = 21, height=14.8, units="cm")

# ### Spring Flow data
# spr_dat <- fdat %>% mutate(Year = year(Date),
#                            Month = month(Date)) %>%
#   filter(Month>=8) %>% filter(Month<12) %>%
#   group_by(Year) %>% summarise(Mean_Flow = mean(`Mean Discharge (ML/D)`))
# 
# ggplot(spr_dat, aes(Year, Mean_Flow)) + geom_line()+
#   theme_classic() +
#   theme(axis.text = element_text(size=12, colour="black"),
#         axis.title = element_text(size=14, face="bold"))
# ggsave("Conceptual model/latest data from DJ/Wallis Flow spring Flow.png",  
#        dpi=600, width = 21, height=14.8, units="cm")
# 
# 
# fevents <- read_csv("Conceptual model/latest data from DJ/Wallis Flow Data Clean with events.csv")
# fevents <- fevents %>% mutate(Year = year(Date),
#                              Month = month(Date)) %>%
#   filter(Month>=8) %>% filter(Month<12) %>%
#   group_by(Year) %>% summarise(High_Flow_Events = sum(`High_Flow_Event`, na.rm=T))
# 
# ggplot(fevents, aes(Year, High_Flow_Events)) + geom_line()+
#   theme_classic() + ylab("Wet Spring")+
#   theme(axis.text = element_text(size=12, colour="black"),
#         axis.title = element_text(size=14, face="bold"))
# ggsave("Conceptual model/latest data from DJ/Wallis Flow spring Events.png",  
#        dpi=600, width = 21, height=14.8, units="cm")
