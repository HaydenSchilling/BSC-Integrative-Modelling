# Offshore catch and flow relationship

odat <- read_csv("Data/Number of large offshore catches.csv") %>% select(-Date)
fdat <-  read_csv("Data/Wallis Flow Data Clean with events.csv") %>% 
  mutate(Date = as.Date(lubridate::dmy_hm(`Date and time`)),
         Year = lubridate::year(Date),
         Month = lubridate::month(Date))

full_dat <- fdat %>% left_join(odat) %>% filter(Year >= 2010) %>%
  filter(Date < "2021-07-01") 
full_dat$Total_max_catches <- replace_na(full_dat$Total_max_catches, 0)
full_dat <- full_dat %>%  mutate(lag_High_Flow_Event = lag(High_Flow_Event),
                                                lag_Max_Flow = lag(`Max Discharge (ML/D)`)) %>%
  rename(Max_Flow = `Max Discharge (ML/D)`) %>%
  mutate(offshore_catch = case_when(Total_max_catches>0 ~ 1,
                                    T ~ 0))

# library(DHARMa)
# library(glmmTMB)
# 
# f1 <- glmmTMB(Total_max_catches ~ High_Flow_Event, data=full_dat, family="nbinom1")
# resids <- simulateResiduals(f1)
# plot(resids)
# car::Anova(f1)
# 
# f2 <- glmmTMB(Total_max_catches ~ lag_High_Flow_Event, data=full_dat, family="nbinom1")
# resids <- simulateResiduals(f1)
# plot(resids)
# car::Anova(f2)

cor.test(full_dat$Total_max_catches, full_dat$High_Flow_Event)
cor.test(full_dat$Total_max_catches, full_dat$lag_High_Flow_Event)


# f3 <- glmmTMB(Total_max_catches ~ Max_Flow, data=full_dat, family="nbinom1")
# resids <- simulateResiduals(f3)
# plot(resids)
# car::Anova(f3)
# plot(effects::allEffects(f3))
# 
# f4 <- glmmTMB(Total_max_catches ~ lag_Max_Flow, data=full_dat, family="nbinom1")
# resids <- simulateResiduals(f4)
# plot(resids)
# car::Anova(f4)
# 
# # presence absence offshore
# f1 <- glmmTMB(offshore_catch ~ High_Flow_Event, data=full_dat, family="binomial")
# resids <- simulateResiduals(f1)
# plot(resids)
# car::Anova(f1)
# plot(effects::allEffects(f1))
# 
# f2 <- glmmTMB(offshore_catch ~ lag_High_Flow_Event, data=full_dat, family="binomial")
# resids <- simulateResiduals(f1)
# plot(resids)
# car::Anova(f2)
# 
# 
# f3 <- glmmTMB(offshore_catch ~ Max_Flow, data=full_dat, family="binomial")
# resids <- simulateResiduals(f3)
# plot(resids)
# car::Anova(f3)
# plot(effects::allEffects(f3))
# 
# f4 <- glmmTMB(offshore_catch ~ lag_Max_Flow, data=full_dat, family="binomial")
# resids <- simulateResiduals(f4)
# plot(resids)
# car::Anova(f4)

# ### Add rain data
# 
# rdat <- read_csv("Conceptual model/latest data from DJ/Wallis Rain clean monthly.csv") %>% select(-Date)
# full_dat <- full_dat %>% left_join(rdat) %>% mutate(rain_lag1 = lag(Rain_mm,1),
#                                                     rain_lag2 = lag(Rain_mm,2))
# 
# 
# f1 <- glmmTMB(Total_max_catches ~ Rain_mm, data=full_dat, family="nbinom1")
# resids <- simulateResiduals(f1)
# plot(resids)
# car::Anova(f1)
# 
# f2 <- glmmTMB(Total_max_catches ~ rain_lag1, data=full_dat, family="nbinom1")
# resids <- simulateResiduals(f2)
# plot(resids)
# car::Anova(f2)
# 
# f3 <- glmmTMB(Total_max_catches ~ rain_lag2, data=full_dat, family="nbinom1")
# resids <- simulateResiduals(f3)
# plot(resids)
# car::Anova(f3)
# 
# # presence-absence
# f1 <- glmmTMB(offshore_catch ~ Rain_mm, data=full_dat, family="binomial")
# resids <- simulateResiduals(f1)
# plot(resids)
# car::Anova(f1)
# plot(effects::allEffects(f1))
# 
# f2 <- glmmTMB(offshore_catch ~ rain_lag1, data=full_dat, family="binomial")
# resids <- simulateResiduals(f1)
# plot(resids)
# car::Anova(f2)
# 
# f3 <- glmmTMB(offshore_catch ~ rain_lag2, data=full_dat, family="binomial")
# resids <- simulateResiduals(f3)
# plot(resids)
# car::Anova(f3)
