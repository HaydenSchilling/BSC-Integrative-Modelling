# Wallis SST

library(tidyverse)

SSt <- read_csv("Data/IMOS SST Full.csv") %>%
  mutate(`old SST` = `old SST` - 273.15,
         `new SST` = `new SST` - 273.15) %>%
  rowwise() %>% mutate(Full_SST = mean(c(`old SST`,`new SST`), na.rm=T)) %>%
  mutate(Date = as.Date(lubridate::ymd_hms(`time (UTC)`)),
         Year = lubridate::year(Date),
         Month = lubridate::month(Date)) %>%
  mutate(July_Sept = case_when((Month>6 & Month <10) ~ "Yes",
                               T ~ "No"),
         Nov_Dec = case_when((Month>10) ~ "Yes",
                             T ~ "No"),
         Jan_Mar = case_when((Month<4) ~ "Yes",
                             T ~ "No"))


cor.test(SSt$`old SST`, SSt$`new SST`)

plot(SSt$`old SST`, SSt$`new SST`)

Jul_Sept_SST <- SSt %>% group_by(July_Sept, Year) %>%
  summarise(mean_SST = mean(Full_SST, na.rm=T)) %>% filter(July_Sept == "Yes")

Nov_Dec_SST <- SSt %>% group_by(Nov_Dec, Year) %>%
  summarise(mean_SST = mean(Full_SST, na.rm=T)) %>% filter(Nov_Dec == "Yes")

Jan_Mar_SST <- SSt %>% group_by(Jan_Mar, Year) %>%
  summarise(mean_SST = mean(Full_SST, na.rm=T)) %>% filter(Jan_Mar == "Yes")

d1 <- Jul_Sept_SST %>% left_join(Nov_Dec_SST, by="Year", suffix = c(".Jul_Sept", ".Nov_Dec")) %>%
  left_join(Jan_Mar_SST, by = "Year") %>% rename(mean_SST.Jan_Mar = mean_SST) %>% ungroup() %>%
  select(-1,-4,-6)

#write_csv(d1, "Conceptual model/latest data from DJ/Wallis offshore SST summary.csv")
