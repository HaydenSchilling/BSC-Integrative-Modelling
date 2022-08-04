library(tidyverse)
library(lubridate)

odat <- read_csv("Data/BSC Wallis Offshore.csv")

table(odat$`Fishing Business Owner Name`)
hist(odat$`Whole Catch Weight`)
plot(odat$`Catch Weight`, odat$`Whole Catch Weight`)

odat2 <- odat %>% group_by(`Event Date`, `Fishing Business Owner Name`) %>%
  summarise(Total_Catch = sum(`Catch Weight`, na.rm=T)) %>%
  mutate(Date = dmy(`Event Date`))

hist(odat2$Total_Catch)

ggplot(odat2, aes(Date, y = Total_Catch)) + geom_point(alpha=0.5)
#ggsave("Conceptual model/latest data from DJ/Total offshore over time.png",
#       dpi=600, units="cm", height=14.8, width=21)

odat3 <- odat2 %>% mutate(Max_Catch = case_when(Total_Catch>=24 ~1,
                                                TRUE ~ 0),
                          Month = month(Date),
                          Year = year(Date)) %>%
  group_by(Month, Year) %>%
  summarise(Total_max_catches = sum(Max_Catch, na.rm=T)) %>%
  mutate(Date = ymd(paste0(Year,"-",Month,"-15")))

ggplot(odat3, aes(Date, y = Total_max_catches)) + 
  geom_line() + theme_classic() +
  ylab("Number of Catches > 24kg")+
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14))
#ggsave("Conceptual model/latest data from DJ/Catches of 24kg.png",
#       dpi=600, width = 21, height=14.8, units="cm")

write_csv(odat3, "Data/Number of large offshore catches.csv")
