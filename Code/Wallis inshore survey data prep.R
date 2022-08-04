# wallis inshore survey data

library(tidyverse)
library(lubridate)

# mydata <- read_csv("Data/Field data newer from matt.csv") %>% select(1:Field_notes)
# 
# mydata <- mydata %>% filter(Est_code == "WLLOFF") %>% mutate(Date = lubridate::ymd(Date))
# mydata <- mydata %>% mutate(Month = month(Date),
#                             Year = year(Date)) %>% drop_na(Depl_dur_decday)
# 
# dat <- mydata %>% group_by(Year, Month) %>%
#   summarise(Total_PORAM = sum(PORARM, na.rm=T),
#             Total_Traps = n(), CPUE = Total_PORAM/Total_Traps) %>%
#   mutate(Date = dmy(paste0("15/",Month,"/",Year)))
# 
# ggplot(dat, aes(x= Date, y=CPUE)) + geom_col() + theme_classic()+
#   theme(axis.title = element_text(face = "bold", size=14),
#         axis.text = element_text(colour="black", size=12))
# 
# write_csv(dat, "Data/Inshore survey summary.csv")

dat1 <- read_csv("Data/Inshore survey summary.csv") %>% select(-Total_Traps)
dat2 <- read_csv("Data/Wallis survey summary.csv") %>% select(-Total_Traps)

mydat <- dat2 %>% left_join(dat1, by = c("Year", "Month"))
write_csv(mydat, "Data/inshore and wallis combined.csv")

cor.test(mydat$CPUE, mydat$Wallis_Lake_CPUE)
plot(mydat$CPUE, mydat$Wallis_Lake_CPUE)

mydat2 <- mydat %>% pivot_longer(c(4,6) ,names_to = "Data", values_to = "CPUE_value") %>%
  mutate(Date = lubridate::dmy(paste0("15/",Month,"/",Year)))

 ggplot(mydat2, aes(x= Date, y=CPUE_value, col=Data)) + geom_point() + theme_classic()+
   #geom_point(aes(y=Wallis_Lake_CPUE), col="green") +
   theme(axis.title = element_text(face = "bold", size=14),
         axis.text = element_text(colour="black", size=12))+
   theme(legend.position = "bottom")

 #ggsave("Data/inshore and wallis combined plot.png", dpi = 600,
#        width=21, height = 14.8, units="cm") 
 
 ggplot(mydat, aes(x= Date, y=CPUE)) + geom_point() + theme_classic()+
    geom_point(aes(y=Wallis_Lake_CPUE), col="green") +
    theme(axis.title = element_text(face = "bold", size=14),
          axis.text = element_text(colour="black", size=12))+
    theme(legend.position = "bottom")
 
 #ggsave("Data/inshore and wallis combined plot2.png", dpi = 600,
#        width=21, height = 14.8, units="cm") 
 
 
 #### now split by size class
 
 mydata <- read_csv("Data/Field bio data newer from matt.csv")# %>% select(1:Field_notes)

 mydata <- mydata %>% filter(Est_code == "WLLOFF") %>% mutate(Date = lubridate::ymd(Date))
 mydata <- mydata %>% mutate(Month = month(Date),
                             Year = year(Date)) %>% 
   mutate (Size_class= case_when(CL_mm >65 ~ 'recruits', 
                                 CL_mm <50 ~ 'small',
                                 CL_mm >=50 & CL_mm <=65 ~ 'prerecruits'))
 
 bio_summary <- mydata %>% group_by (Date, Size_class, Site , Rep, Sex)%>%
   summarise(Crab_abundance = n() ) %>% ungroup() %>% drop_na(Size_class)
 
 
 bio_summary_full <- bio_summary %>% complete(Date,Sex, Size_class, Site, Rep,fill= list(Crab_abundance=0)) %>%
   arrange(Date,Site, Rep, Size_class, Sex) %>% mutate(Month = month(Date), Year = year(Date))
 # %>% drop_na(Depl_dur_decday)

 dat <- bio_summary_full %>% group_by(Year, Month, Size_class) %>% #added , Sex
   summarise(Total_PORAM = sum(Crab_abundance, na.rm=T),
             Total_Traps = n(), CPUE = Total_PORAM/Total_Traps) %>%
   mutate(Date = dmy(paste0("15/",Month,"/",Year)))
 
 # dat_sex <- bio_summary_full %>% group_by(Year, Month,Sex, Size_class) %>%
 #   summarise(Total_PORAM = sum(Crab_abundance, na.rm=T),
 #             Total_Traps = n(), CPUE = Total_PORAM/Total_Traps) %>%
 #   mutate(Date = dmy(paste0("15/",Month,"/",Year)))

 ggplot(dat, aes(x= Date, y=CPUE)) + geom_col() + theme_classic()+ facet_wrap(~Size_class, ncol=1)+
   theme(axis.title = element_text(face = "bold", size=14),
         axis.text = element_text(colour="black", size=12))

 write_csv(dat, "Data/Inshore survey summary with size.csv")
 
 dat2 <- read_csv("Data/Wallis survey summary by size.csv") %>% select(-Total_Traps)
 dat1 <- read_csv("Data/Inshore survey summary with size.csv") %>% select(-Total_Traps)
 
 mydat <- dat2 %>% left_join(dat1, by = c("Year", "Month", "Size_class"))
 write_csv(mydat, "Data/inshore and wallis combined by size.csv")
 
 #dat1<-dat
 
 mydat2 <- mydat %>% pivot_longer(c(5,7) ,names_to = "Data", values_to = "CPUE_value") %>%
   mutate(Date = lubridate::dmy(paste0("15/",Month,"/",Year)))
 
 
 mydat <- mydat %>% mutate(Size_class = case_when(Size_class == "prerecruits" ~ "a) 50 - 65 mm CL",
                                                    Size_class == "recruits" ~ "b) >65 mm CL",
                                                  Size_class == "small" ~ "small",
                                                    T ~ "other")) %>% filter(Size_class != "small")
 
 ggplot(mydat2, aes(x= Date, y=CPUE_value, col=Data)) + geom_point() + theme_classic()+ facet_wrap(~Size_class, ncol=1)+
   #geom_point(aes(y=Wallis_Lake_CPUE), col="green") +
   theme(axis.title = element_text(face = "bold", size=14),
         axis.text = element_text(colour="black", size=12))+
   theme(legend.position = "bottom")
 
 #ggsave("Data/inshore and wallis combined plot by size.png", dpi = 600,
#        width=21, height = 14.8, units="cm") 
 
 ggplot(mydat, aes(x= Date, y=CPUE)) + geom_point() + theme_classic()+ facet_wrap(~Size_class, ncol=1)+
   geom_point(aes(y=Wallis_Lake_CPUE), col="green") +
   theme(axis.title = element_text(face = "bold", size=14),
         axis.text = element_text(colour="black", size=12))+
   theme(legend.position = "bottom")
 
 #ggsave("Data/inshore and wallis combined plot2 by size.png", dpi = 600,
#        width=21, height = 14.8, units="cm") 
 
 ggplot(mydat, aes(CPUE, Wallis_Lake_CPUE)) + geom_point() + facet_wrap(~Size_class, scales = "free", ncol=3) +
   theme_classic()+
   theme(axis.title = element_text(face = "bold", size=14),
         axis.text = element_text(colour="black", size=12),
         panel.background = element_rect(fill=NA, colour="black"),
         strip.text = element_text(face="bold", size=12)) + geom_smooth(method = "lm") +
    labs(y = "Wallis Lake CPUE (Crabs/Trap)", x = "Inshore Survey CPUE (Crabs/Trap)")
 
 #ggsave("Conceptual model/Figures/inshore and wallis correlation plot3 by size.png", dpi = 600,
#        width=21, height = 14.8, units="cm") 
 #ggsave("Conceptual model/Figures/inshore and wallis correlation plot3 by size.pdf", dpi = 600,
  #      width=21, height = 14.8, units="cm")
 
 recruits <- mydat %>% filter(Size_class == "b) >65 mm CL")
cor.test(recruits$Wallis_Lake_CPUE, recruits$CPUE) 
not_recruits <- mydat %>% filter(Size_class == "a) 50 - 65 mm CL")
cor.test(not_recruits$Wallis_Lake_CPUE, not_recruits$CPUE) 

# ### add flow data
# fdat <-  read_csv("Data/Wallis Flow Data Clean with events.csv") %>% 
#    mutate(Date = as.Date(lubridate::dmy_hm(`Date and time`)),
#           Year = lubridate::year(Date),
#           Month = lubridate::month(Date)) %>% select(-Date)
# recruits2 <- fdat %>% left_join(recruits) %>% mutate(max_flow_lag = lag(`Max Discharge (ML/D)`))
# cor.test(recruits2$CPUE, recruits2$`Max Discharge (ML/D)`)
# cor.test(recruits2$CPUE, recruits2$max_flow_lag)
# cor.test(recruits2$CPUE, recruits2$`Mean Discharge (ML/D)`)
# 
# cor.test(recruits2$Wallis_Lake_CPUE, recruits2$`Max Discharge (ML/D)`)
# cor.test(recruits2$Wallis_Lake_CPUE, recruits2$max_flow_lag)
# cor.test(recruits2$Wallis_Lake_CPUE, recruits2$`Mean Discharge (ML/D)`)
