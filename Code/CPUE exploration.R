# Initial explore

library(tidyverse)

# mydata <- read_csv("Conceptual model/BSC_Logbook/Rosh/EFT4_BSC_Logbook.csv") %>%
#   mutate(Date = lubridate::dmy(`Event Date`))
# 
# dat_sum <- mydata %>% filter(Estuaryname == "Wallis Lake") %>%
#   group_by(`Event date Year`, `Event date Month`, `Species Common Name`) %>%
#   summarise(Total_Catch = sum(`Catch Weight`, na.rm=T),
#             Total_Effort = sum(`Catch Effort Quantity`, na.rm=T),
#             CPUE = Total_Catch/Total_Effort) %>%
#   mutate(Date = lubridate::dmy(paste0("01/",`Event date Month`,"/",`Event date Year`))) %>%
#   ungroup()
# 
# dat_sum  
# 
# write_csv(dat_sum, "Conceptual model/Commercial CPUE Wallis Lake summary 18-21.csv")
# 
# ggplot(dat_sum, aes(x=Date, y = CPUE, col=`Species Common Name`))+ geom_line() +
#   theme_classic()
# 
# ggsave("Conceptual model/monthly CPUE by sex.png",dpi=600, width=21, height=14.8, units="cm")  
# 
# 
# ### combine sexes
# dat_sum2 <- mydata %>% filter(Estuaryname == "Wallis Lake") %>%
#   group_by(`Event date Year`, `Event date Month`) %>%
#   summarise(Total_Catch = sum(`Catch Weight`, na.rm=T),
#             Total_Effort = sum(`Catch Effort Quantity`, na.rm=T),
#             CPUE = Total_Catch/Total_Effort) %>%
#   mutate(Date = lubridate::dmy(paste0("01/",`Event date Month`,"/",`Event date Year`))) %>%
#   ungroup()
# 
# dat_sum2  
# 
# ggplot(dat_sum2, aes(x=Date, y = CPUE))+ geom_line() +
#   theme_classic()
# ggsave("Conceptual model/monthly CPUE.png",dpi=600, width=21, height=14.8, units="cm")  
# 
# 
# rdat <- read_csv("Final Data/Bio_Environ_full_WLL_1.csv")
# rdat <- rdat %>% mutate(Month = lubridate::month(Date),
#                         Year = lubridate::year(Date))
# 
# rsum <- rdat %>% group_by(Month, Year) %>% 
#   summarise(n=n(),
#             CPUE_all = sum(All_crabs)/n,
#             CPUE_recruits = sum(All_recruits)/n,
#             CPUE_F_recruits = sum(F_recruits)/n,
#             CPUE_M_recruits = sum(M_recruits)/n)
# rsum
# 
# psych::pairs.panels(rsum)
# datdat <- dat_sum %>% mutate(Month = as.numeric(`Event date Month`)) %>%
#   rename(Year = `Event date Year`) %>%
#   select(-Date, -`Event date Month`)
# 
# fdat <- full_join(datdat, rsum)
# 
# psych::pairs.panels(fdat)
# 
# ### 2015 - 2018
# library(tidyverse)
# 
# mydata <- read_csv("Conceptual model/BSC_Logbook/BSC_2015-2018.csv") %>%
#   mutate(Date = lubridate::dmy(`Event Date`))
# 
# dat_sum <- mydata %>% filter(Estuaryname == "Wallis Lake") %>%
#   group_by(`Event date Year`, `Event date Month`, `Species Common Name`) %>%
#   summarise(Total_Catch = sum(`Catch Weight`, na.rm=T),
#             Total_Effort = sum(`Catch Effort Quantity`, na.rm=T),
#             CPUE = Total_Catch/Total_Effort) %>%
#   mutate(Date = lubridate::dmy(paste0("01/",`Event date Month`,"/",`Event date Year`))) %>%
#   ungroup()
# 
# dat_sum  
# 
# write_csv(dat_sum, "Conceptual model/Commercial CPUE Wallis Lake summary 15-18.csv")
# 
# ggplot(dat_sum, aes(x=Date, y = CPUE, col=`Species Common Name`))+ geom_line() +
#   theme_classic()
# 
# #ggsave("Conceptual model/monthly CPUE by sex.png",dpi=600, width=21, height=14.8, units="cm")  
# 
# 
# ### 2012 - 2015
# library(tidyverse)
# 
# mydata <- read_csv("Conceptual model/BSC_Logbook/BSC_2012_2015_OUT.csv") %>%
#   mutate(Date = lubridate::dmy(`Event Date`))
# 
# dat_sum <- mydata %>% filter(Estuaryname == "Wallis Lake") %>%
#   group_by(`Event date Year`, `Event date Month`, `Species Common Name`) %>%
#   summarise(Total_Catch = sum(`Catch Weight`, na.rm=T),
#             Total_Effort = sum(`Catch Effort Quantity`, na.rm=T),
#             CPUE = Total_Catch/Total_Effort) %>%
#   mutate(Date = lubridate::dmy(paste0("01/",`Event date Month`,"/",`Event date Year`))) %>%
#   ungroup()
# 
# dat_sum  
# 
# write_csv(dat_sum, "Conceptual model/Commercial CPUE Wallis Lake summary 12-15.csv")
# 
# ggplot(dat_sum, aes(x=Date, y = CPUE, col=`Species Common Name`))+ geom_line() +
#   theme_classic()
# 
# 
# ### 2009 - 2012
# library(tidyverse)
# 
# mydata <- read_csv("Conceptual model/BSC_Logbook/BSC_2009-2012.csv") %>%
#   mutate(Date = lubridate::dmy(`Event Date`))
# 
# dat_sum <- mydata %>% filter(Estuaryname == "Wallis Lake") %>%
#   group_by(`Event date Year`, `Event date Month`, `Species Common Name`) %>%
#   summarise(Total_Catch = sum(`Catch Weight`, na.rm=T),
#             Total_Effort = sum(`Catch Effort Quantity`, na.rm=T),
#             CPUE = Total_Catch/Total_Effort) %>%
#   mutate(Date = lubridate::dmy(paste0("01/",`Event date Month`,"/",`Event date Year`))) %>%
#   ungroup()
# 
# dat_sum  
# 
# write_csv(dat_sum, "Conceptual model/Commercial CPUE Wallis Lake summary 09-12.csv")
# 
# ggplot(dat_sum, aes(x=Date, y = CPUE, col=`Species Common Name`))+ geom_line() +
#   theme_classic()
# 
# 
# ### combine all
# 
# dat1 <- read_csv("Conceptual model/Commercial CPUE Wallis Lake summary 09-12.csv")
# dat2 <- read_csv("Conceptual model/Commercial CPUE Wallis Lake summary 12-15.csv")
# dat3 <- read_csv("Conceptual model/Commercial CPUE Wallis Lake summary 15-18.csv")
# dat4 <- read_csv("Conceptual model/Commercial CPUE Wallis Lake summary 18-21.csv")
# 
# all_dat <- bind_rows(dat1, dat2, dat3, dat4)
# 
# write_csv(all_dat, "Conceptual model/Commercial CPUE Wallis Lake monthly by sex.csv")
all_dat <- read_csv("Data/Commercial CPUE Wallis Lake monthly by sex.csv")


ggplot(all_dat, aes(x=Date, y = CPUE, col=`Species Common Name`))+ geom_line() +
  theme_classic() + scale_x_date(breaks = "3 year")
#ggsave("Conceptual model/latest data from DJ/CPUE Wallis by sex.png", dpi =600, width =21, height=14.8, units="cm")

ggplot(all_dat, aes(x=Date, y = CPUE, col=`Species Common Name`))+ geom_line() +
  theme_classic() + facet_wrap(~`Event date Month`) + theme(legend.position = "bottom")+
  geom_vline(xintercept = as.Date("2017-11-01"), linetype=3)
#ggsave("Conceptual model/latest data from DJ/CPUE Wallis monthyl by sex.png", dpi =600, width =21, height=21, units="cm")

ggplot(all_dat, aes(x=Date, y = Total_Catch/1000, col=`Species Common Name`, linetype=`Species Common Name`))+ geom_line() +
  theme_classic() + facet_wrap(~`Event date Month`) + ylab("Catch (t)")+ 
  geom_vline(xintercept = as.Date("2017-11-01"), linetype=3)+
  scale_x_date(date_breaks = "3 years", date_labels = "%Y")+
  scale_linetype_discrete(name="Sex", labels = c("Female", "Male"))+
  scale_color_discrete(name="Sex", labels = c("Female", "Male"))+
  theme(axis.title = element_text(face="bold", size=14),
        axis.text = element_text(size=12, colour="black"),
        axis.text.x = element_text(size=12, colour="black", angle=45, vjust=0.5),
        panel.border = element_rect(fill=NA, colour="black"),
        legend.position = "bottom",
        strip.text = element_text(face="bold", size=12),
        legend.title = element_text(face="bold", size=14),
        legend.text = element_text(size=12))
#ggsave("Conceptual model/Figures/Catch Total Wallis monthly by sex.pdf", dpi =600, width =21, height=21, units="cm")


mdat <- all_dat %>% filter(`Species Common Name` == "Male Blue Swimmer Crab")
f1 <- lm(Total_Catch ~ Total_Effort,data=mdat)
summary(f1)


fdat <- all_dat %>% filter(`Species Common Name` == "Female Blue Swimmer Crab")
f2 <- lm(Total_Catch ~ Total_Effort,data=fdat)
summary(f2)

sumdat <- all_dat %>% group_by(`Event date Month`, `Species Common Name`) %>% summarise(Total_Catch = sum(Total_Catch, na.rm=T)) %>%
  mutate(`Event date Month`= as.numeric(`Event date Month`),
         Season = case_when((`Event date Month` >= 6 & `Event date Month` <=11) ~ "Winter",
                            `Event date Month` <= 4 ~ "Summer",
                     T ~ "Other")) %>% ungroup() 
sumdatA <- sumdat %>% pivot_wider(names_from = `Species Common Name`, values_from = Total_Catch) %>%
  mutate(Female_Proportion = `Female Blue Swimmer Crab`/(`Female Blue Swimmer Crab`+`Male Blue Swimmer Crab`))

ggplot(sumdatA, aes(`Event date Month`, Female_Proportion)) + geom_line() +theme_classic()+
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14),
        axis.ticks = element_line(colour="black")) +
  scale_x_continuous(breaks = seq(1,12,1)) +xlab("Month")+ ylab("Proportion Female (by weight)")
#ggsave("Conceptual model/latest data from DJ/Sex ratio by month.png", dpi=600, width=15, height=15, units="cm")

sumdatB <- all_dat %>% select(-5,-6) %>%
  pivot_wider(names_from = `Species Common Name`, values_from = Total_Catch) %>%
  mutate(Female_Proportion = `Female Blue Swimmer Crab`/(`Female Blue Swimmer Crab`+`Male Blue Swimmer Crab`)) %>%
  group_by(`Event date Month`) %>% summarise(Female_Proportion_mean = mean(Female_Proportion),
                                             sd_Fem_Prop = sd(Female_Proportion),
                                             n=n(),
                                             SE_Fem_Prop = sd_Fem_Prop/sqrt(n)) %>%
  mutate(month = as.numeric(`Event date Month`))

ggplot(sumdatB, aes(month, Female_Proportion_mean)) + geom_line() +theme_classic()+
  geom_errorbar(aes(ymin=Female_Proportion_mean - sd_Fem_Prop, ymax = Female_Proportion_mean + sd_Fem_Prop))+
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14),
        axis.ticks = element_line(colour="black")) +
  scale_y_continuous(limits = c(0,1), breaks=seq(0,1, 0.25))+
  scale_x_continuous(breaks = seq(1,12,1), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                      "Aug", "Sep", "Oct", "Nov", "Dec")) +
  xlab("Month")+ ylab("Mean Proportion Female (±SD)")
#ggsave("Conceptual model/Figures/Female catch proportions.pdf", units="cm", height = 14.8, width =21, dpi=600)
#ggsave("Conceptual model/Figures/Female catch proportions.png", units="cm", height = 14.8, width =21, dpi=600)


# sumdat2 <- sumdat %>%
# group_by(Season, `Species Common Name`) %>%
#   summarise(Total_Catch = sum(Total_Catch, na.rm=T)) %>%
#   pivot_wider(names_from = `Species Common Name`, values_from = Total_Catch) %>%
#   mutate(Female_Proportion = `Female Blue Swimmer Crab`/(`Female Blue Swimmer Crab`+`Male Blue Swimmer Crab`))
# 
# ### Old stuff

# library(glmmTMB)
# library(DHARMa)
# f3 <- glmmTMB(Total_Catch ~ `Species Common Name`,data=all_dat, family="tweedie", offset=log(Total_Effort))
# resids <- simulateResiduals(f3)
# plot(resids)
# summary(f3)
# car::Anova(f3)
# 
# 
# all_dat$Prediction <- predict(f3, newdata = all_dat, type="response")
# 
# ggplot(all_dat, aes(x=Date, y = Total_Catch))+ geom_line() +
#   theme_classic() + geom_line(aes(y=Prediction), lty=2) + facet_wrap(~`Species Common Name`, ncol=1)
# #ggsave("Conceptual model/latest data from DJ/Catch predictions by effort.png", dpi =600, width =21, height=15, units="cm")
# 
# 
# head(all_dat)
# 
# no_sex <- all_dat %>% group_by(`Event date Year`, `Event date Month`, Date) %>%
#   summarise(Total_Catch= sum(Total_Catch), Effort = max(Total_Effort)) %>%
#   mutate(CPUE = Total_Catch/Effort)
# 
# 
# head(no_sex)
# 
# ggplot(no_sex, aes(x=Date, y = Total_Catch))+ geom_line() +
#   theme_classic() #+ geom_line(aes(y=Prediction), lty=2) #+ facet_wrap(~`Species Common Name`, ncol=1)
# ggsave("Conceptual model/latest data from DJ/Catch combined sexes.png", dpi =600, width =21, height=15, units="cm")
# 
# ggplot(no_sex, aes(x=Date, y = CPUE))+ geom_line() +
#   theme_classic()
# ggsave("Conceptual model/latest data from DJ/CPUE combined sexes.png", dpi =600, width =21, height=15, units="cm")
# 
# winter_harvest <- no_sex %>% mutate(Month = as.numeric(`Event date Month`)) %>%
#   filter(Month<11 & Month >7) %>% group_by(`Event date Year`) %>%
#   summarise(winter_Total_Catch = sum(Total_Catch, na.rm=T),
#             winter_Total_Effort = sum(Effort, na.rm=T),
#             winter_CPUE = winter_Total_Catch/winter_Total_Effort)
#   
# peak_harvest <- no_sex %>% mutate(Month = as.numeric(`Event date Month`)) %>%
#   filter(Month<6) %>% group_by(`Event date Year`) %>%
#   summarise(Peak_Total_Catch = sum(Total_Catch, na.rm=T),
#             Peak_Total_Effort = sum(Effort, na.rm=T),
#             Peak_CPUE = Peak_Total_Catch/Peak_Total_Effort)    
# 
# ddat <- winter_harvest %>% left_join(peak_harvest) %>% mutate(ratio_peak_winter = Peak_Total_Catch/winter_Total_Catch)
# 
# psych::pairs.panels(ddat)
