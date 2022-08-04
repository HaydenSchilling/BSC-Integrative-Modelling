# Wallis lake wind data
library(tidyverse)
library(lubridate)

ddat <- read_csv("Data/BARRA WALLIS Monthly Downwell Wind Data.csv") %>% filter(Location == 32.1806) %>%
  mutate(Date = dmy(paste0("15/",Month,"/",Year))) %>% filter(Year >1995)

ggplot(ddat, aes(Date, DownWind)) + geom_line() + theme_classic()+
  theme(axis.title = element_text(face="bold", size=14),
        axis.text = element_text(size=12, colour="black"),
        panel.border = element_rect(fill=NA, colour="black"))

ggplot(ddat, aes(Date, DownWind)) + geom_line() + theme_classic()+ facet_wrap(~Month)+
  theme(axis.title = element_text(face="bold", size=14),
        axis.text = element_text(size=12, colour="black"),
        panel.border = element_rect(fill=NA, colour="black"))


updat <- read_csv("Data/BARRA WALLIS Monthly Upwell Wind Data.csv") %>% filter(Location == 32.1806) %>%
  mutate(Date = dmy(paste0("15/",Month,"/",Year))) %>% filter(Year >1995)

ggplot(updat, aes(Date, UpWind)) + geom_line() + theme_classic()+
  theme(axis.title = element_text(face="bold", size=14),
        axis.text = element_text(size=12, colour="black"),
        panel.border = element_rect(fill=NA, colour="black"))

ggplot(updat, aes(Date, UpWind)) + geom_line() + theme_classic()+ facet_wrap(~Month)+
  theme(axis.title = element_text(face="bold", size=14),
        axis.text = element_text(size=12, colour="black"),
        panel.border = element_rect(fill=NA, colour="black"))

bothdat <- ddat %>% left_join(updat) %>% mutate(Ratio = DownWind/UpWind)

ggplot(bothdat, aes(Date, Ratio)) + geom_line() + theme_classic()+ facet_wrap(~Month)+
  theme(axis.title = element_text(face="bold", size=14),
        axis.text = element_text(size=12, colour="black"),
        panel.border = element_rect(fill=NA, colour="black"))

cor.test(bothdat$DownWind, bothdat$UpWind)

### So to explore wind effects

write_csv(bothdat, "Data/BARRA WALLIS Wind Data Clean.csv")


### annual

dd1 <- bothdat %>% mutate(July_Sept = case_when((Month>6 & Month <10) ~ "Yes",
                                                T ~ "No"),
                          Oct_Dec = case_when((Month>9) ~ "Yes",
                                              T ~ "No"),
                          Jan_Mar = case_when((Month<4) ~ "Yes",
                                              T ~ "No"))

Jul_Sept_wind <- dd1 %>% group_by(July_Sept, Year) %>%
  summarise(UpWind = sum(UpWind, na.rm=T),
            DownWind = sum(DownWind, na.rm=T)) %>% filter(July_Sept == "Yes")

Oct_Dec_wind <- dd1 %>% group_by(Oct_Dec, Year) %>%
  summarise(UpWind = sum(UpWind, na.rm=T),
            DownWind = sum(DownWind, na.rm=T)) %>% filter(Oct_Dec == "Yes")

Jan_Mar_wind <- dd1 %>% group_by(Jan_Mar, Year) %>%
  summarise(UpWind = sum(UpWind, na.rm=T),
            DownWind = sum(DownWind, na.rm=T)) %>% filter(Jan_Mar == "Yes")

d1 <- Jul_Sept_wind %>% left_join(Oct_Dec_wind, by="Year", suffix = c(".Jul_Sept", ".Oct_Dec")) %>%
  left_join(Jan_Mar_wind, by = "Year") %>% rename(UpWind.Jan_Mar = UpWind,
                                                  DownWind.Jan_Mar = DownWind) %>% ungroup() %>%
  select(-1,-5,-8)

write_csv(d1, "Data/BARRA WALLIS Wind Data Clean Annual.csv")
