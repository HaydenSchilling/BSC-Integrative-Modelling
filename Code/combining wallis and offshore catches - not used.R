# combining Wallis and Offshore Data to look at ratio?
library(tidyverse)
library(lubridate)

odat <- read_csv("Data/Number of large offshore catches.csv")
all_dat <- read_csv("Data/Commercial CPUE Wallis Lake monthly by sex.csv") %>%
  group_by(`Event date Year`, `Event date Month`) %>% rename(Month = `Event date Month`,
                                                                   Year  = `Event date Year`) %>%
  mutate(Month = as.numeric(Month))%>%
  select(-Date) %>%
  summarise(Total_Catch= sum(Total_Catch), Effort = max(Total_Effort)) %>%
  mutate(CPUE = Total_Catch/Effort) %>% ungroup()

bdat <- all_dat %>% left_join(odat) 
bdat$Total_max_catches <- bdat$Total_max_catches %>% replace_na(0)
bdat <- bdat %>% mutate(offshore_inshore_ratio = Total_max_catches/Total_Catch)

ggplot(bdat, aes(Date, Total_max_catches)) + geom_line() +
  geom_line(aes(y=CPUE*2), col="red")

ggplot(bdat, aes(Date, Total_Catch)) + geom_line() +
  geom_line(aes(y=Total_max_catches*2000), col="red")

ggplot(bdat, aes(Date, offshore_inshore_ratio)) + geom_line() +theme_classic()
#ggsave("Data/offshore wallis lake ratio.png", dpi = 600, units = "cm",
#       width=21, height =14.8)

cor.test(bdat$CPUE, bdat$Total_max_catches)
ccf(bdat$CPUE, bdat$Total_max_catches)

ccfvalues <- ccf(bdat$CPUE, bdat$Total_max_catches)
ccfvalues # some relationships at -5, -5 and +8, +9

# add in rain data

rdat <- read_csv("Data/Wallis Rain clean monthly.csv") %>% select(-Date)
bdat <- bdat %>% left_join(rdat) 
bdat$Rain_mm <- bdat$Rain_mm %>% replace_na(0)

ggplot(bdat, aes(Date, Total_max_catches*100)) + geom_line() +
  geom_line(aes(y=Rain_mm), col="red")

ccf(bdat$Rain_mm, bdat$Total_max_catches)
ccfvalues2 <- ccf(bdat$Rain_mm, bdat$Total_max_catches)
ccfvalues2 # some relationships at -4, -5, +2
ccf(bdat$Rain_mm, bdat$offshore_inshore_ratio)


## add in flow data
fdat <- read_csv("Data/Wallis Flow Data Clean.csv") %>%
  mutate(Month = month(dmy_hm(`Date and time`)),
         Year = year(dmy_hm(`Date and time`))) %>%
  select(-`Date and time`)
bdat <- bdat %>% left_join(fdat)   

ggplot(bdat, aes(Date, Total_max_catches*100)) + geom_line() +
  geom_line(aes(y=`Mean Discharge (ML/D)`), col="red")

ccf(bdat$`Mean Discharge (ML/D)`, bdat$Total_max_catches)
ccfvalues3 <- ccf(bdat$`Mean Discharge (ML/D)`, bdat$Total_max_catches)
ccfvalues3 # some relationships at -2, -3, -4

ccf(bdat$`Max Discharge (ML/D)`, bdat$Total_max_catches)
ccfvalues4 <- ccf(bdat$`Max Discharge (ML/D)`, bdat$Rain_mm)
ccfvalues4 # strong relationship at +1

ccf(bdat$`Mean Discharge (ML/D)`, bdat$offshore_inshore_ratio)
ccfvalues3 <- ccf(bdat$`Mean Discharge (ML/D)`, bdat$offshore_inshore_ratio)
ccfvalues3 # some relationships at -2, -3, -4

#install.packages("astsa")
library(astsa)
lag2.plot (bdat$`Mean Discharge (ML/D)`,bdat$Total_max_catches,  5) 

ccf(bdat$`Max Discharge (ML/D)`, bdat$Effort) 
ccf(bdat$`Mean Discharge (ML/D)`,bdat$Total_Catch) 
lag2.plot (bdat$`Mean Discharge (ML/D)`, bdat$Total_Catch,  11)


lag2.plot (bdat$`Mean Discharge (ML/D)`,bdat$offshore_inshore_ratio,  5)
