# Wallis long term data

library(tidyverse)

mydat <- read_csv("Data/BSC_estuary_1997-2009.csv")

#mydat <- mydat %>% filter(ReportedArea == "Wallis Lake")

dat <- mydat %>% mutate(ReportedArea = case_when(ReportedArea == "Port Stephens/ Myall Lakes, Myall River, Tea Gardens" ~ "Port Stephens, Karuah River, Larpent River",
                                                                                                  T ~ ReportedArea)) %>%
  
  group_by(ReportedArea, CalendarYear, MonthMM, Species) %>%
  summarise(Total_Catch = sum(SumWholeWeightKg, na.rm=T),
            Days_Scaled_Fished = sum(SumDaysFishedScaled, na.rm=T),
            CPUE = Total_Catch/Days_Scaled_Fished) %>%
  ungroup() %>%
  mutate(Date = lubridate::dmy(paste0("15/",MonthMM,"/",CalendarYear))) 
  

lg_catches <- dat %>% filter(Total_Catch>1000)
dat2 <- dat %>% filter(ReportedArea %in% lg_catches$ReportedArea) %>%
  ungroup() %>%
  complete(ReportedArea, Date) %>%
  filter(ReportedArea != "Clarence R Iluka Maclean Yamba Wooloweyah") %>%
  filter(ReportedArea != "Hawkesbury River, Broken Bay, Brisbane Water, Pittwater, Patonga") %>%
  filter(ReportedArea != "Tuggerah Lakes, Munmorah, Budgewoi")


unique(dat2$ReportedArea)

#dat2 <- dat2 %>% mutate(ReportedArea = case_when(ReportedArea == "Port Stephens/ Myall Lakes, Myall River, Tea Gardens" ~ "Port Stephens, Karuah River, Larpent River",
#                                                 T ~ ReportedArea))


ggplot(dat2, aes(Date, y = Total_Catch)) + geom_line() + theme_classic()+
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(size=14, face="bold")) + facet_wrap(~ReportedArea, scales = "free_y")
#ggsave("Data/long term catch.png", width = 21, height=14.8, units="cm", dpi=600)

dat3 <- dat2 %>% filter(ReportedArea == "Wallis Lake")
pWCPUE <- ggplot(dat3, aes(Date, y = CPUE)) + geom_line() + theme_classic()+
  ylab("CPUE (kg/Scaled Fisher Day)")+
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(size=14, face="bold"))
pWCPUE
#ggsave("Data/Wallis Lake long term CPUE.png", width = 21, height=14.8, units="cm", dpi=600)

dat3 <- dat3 %>% mutate(FinYear = case_when(MonthMM > 6 ~ CalendarYear+1,
                                            T ~ CalendarYear))

FinYearTots <- dat3 %>% group_by(FinYear) %>%
  summarise(Fin_year_Total_Catch = sum(Total_Catch, na.rm=T),
            Fin_year_Total_Effort_days = sum(Days_Scaled_Fished, na.rm=T)) %>%
  filter(FinYear != 2022) %>%
  mutate(Fin_year_CPUE = Fin_year_Total_Catch/Fin_year_Total_Effort_days)

write_csv(FinYearTots, "Data/Wallis Financial Year Summary.csv")

pTotal <- ggplot(dat3, aes(Date, y = Total_Catch/1000)) + geom_line() + theme_classic()+
  ylab("Monthly Catch (t)")+
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(size=14, face="bold"))
pTotal
#ggsave("Data/Wallis Lake long term catch.png", width = 21, height=14.8, units="cm", dpi=600)

coeff <- max(dat3$CPUE)/max(dat3$Total_Catch/1000)


p_monthly <- pTotal + scale_y_continuous(sec.axis = sec_axis(~.*coeff, name="CPUE (kg / Scaled Fisher Day)")) +
  geom_line(data=dat3, aes(Date, y = (CPUE)/coeff), col="red", linetype="dashed")+
  theme(axis.text.y.right = element_text(size=12, colour="red"),
        axis.title.y.right = element_text(face="bold", size=14, colour="red"))
p_monthly
#ggsave("Conceptual model/Figures/Wallis Lake long term catch.png", width = 21, height=14.8, units="cm", dpi=600)
#ggsave("Conceptual model/Figures/Wallis Lake long term catch.pdf", width = 21, height=14.8, units="cm", dpi=600)


write_csv(dat3, "Data/Wallis monthly catch.csv")


## Test correlations
cor.test(dat3$CPUE, dat3$Total_Catch)

dat3 <- dat3 %>% mutate(fin_year = case_when(MonthMM > 6 ~ CalendarYear + 1,
                                             T ~ CalendarYear))
dat_annual <- dat3 %>% group_by(fin_year) %>% summarise(total_catch = sum(Total_Catch, na.rm=T),
                                                        total_effort = sum(Days_Scaled_Fished),
                                                        CPUE = total_catch/total_effort) %>%
  filter(fin_year != 2022)

cor.test(dat_annual$total_catch, dat_annual$CPUE)

dat_month <- dat3 %>% group_by(MonthMM) %>% summarise(total_catch = sum(Total_Catch, na.rm=T),
                                                        total_effort = sum(Days_Scaled_Fished)) %>%
  ungroup() %>% mutate(prop_catch = total_catch/sum(total_catch)*100)
sum(dat_month$prop_catch)

### winter to summer ratio
dat3 <- dat3 %>% mutate(Season = case_when((MonthMM >= 6 & MonthMM <=11) ~ "Winter",
                                           MonthMM <= 4 ~ "Summer",
                                           T ~ "Other"))
dat4 <- dat3 %>% group_by(CalendarYear, Season) %>% summarise(Total_Catch = sum(Total_Catch, na.rm=T),
                                             Total_Effort = sum(Days_Scaled_Fished, na.rm=T),
                                             CPUE = Total_Catch/Total_Effort) %>%
  pivot_wider(names_from = Season, values_from = c(Total_Catch, Total_Effort, CPUE)) %>%
  ungroup() %>%
  mutate(lag_winter_Catch = lag(Total_Catch_Winter),
         lag_winter_CPUE = lag(CPUE_Winter),
         Prev_summer_CPUE = lag(CPUE_Summer),
         Prev_summer_Catch = lag(Total_Catch_Summer)) %>%
  mutate(#Ratio_Harvest = lag_winter_Catch/Total_Catch_Summer, # this one is confounded with summer to summer comparisons
         Ratio_Harvest2 = Total_Catch_Winter/Total_Catch_Summer,
         Ratio_Harvest3 = lag(Ratio_Harvest2),
         Summer_CPUE_Change = xts::diff.xts(CPUE_Summer, 1)) %>%
  filter(CalendarYear< 2022)

write_csv(dat4, "Data/Annual Data for modelling.csv")

head(dat4)

ggplot(dat4, aes(CalendarYear, Ratio_Harvest3)) + geom_line() +
  ylab("July-Nov Catch / Jan - April Catch") + theme_classic()+
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(size=14, face="bold"))
#ggsave("Data/Wallis Lake ratio seasonal catch.png", width = 21, height=14.8, units="cm", dpi=600)

#cor.test(dat4$Ratio_Harvest, dat4$CPUE_Summer)
cor.test(dat4$Ratio_Harvest2, dat4$CPUE_Summer)
cor.test(dat4$Ratio_Harvest3, dat4$CPUE_Summer)
#cor.test(dat4$Ratio_Harvest, dat4$Summer_CPUE_Change)
cor.test(dat4$Ratio_Harvest2, dat4$Summer_CPUE_Change)
cor.test(dat4$Ratio_Harvest3, dat4$Summer_CPUE_Change)
cor.test(dat4$lag_winter_Catch, dat4$Summer_CPUE_Change)
cor.test(dat4$lag_winter_CPUE, dat4$Summer_CPUE_Change)
cor.test(dat4$Total_Catch_Winter, dat4$CPUE_Winter)

#ggplot(dat4, aes(Ratio_Harvest, CPUE_Summer)) + geom_point() + geom_smooth()
ggplot(dat4, aes(Ratio_Harvest3, CPUE_Summer)) + geom_point() + geom_smooth()
p1 <- ggplot(dat4, aes(lag_winter_Catch/1000, Summer_CPUE_Change)) + geom_point() + geom_smooth(method="lm") + 
  theme_classic() + theme(axis.text = element_text(colour = "black", size=12),
                          axis.title = element_text(size=14, face="bold")) +
  ylab("Difference from Previous Year CPUE") +xlab("Preceeding Winter Catch (t)")
p2 <- ggplot(dat4, aes(lag_winter_CPUE, Summer_CPUE_Change)) + geom_point() + geom_smooth(method="lm")+ 
  theme_classic() + theme(axis.text = element_text(colour = "black", size=12),
                          axis.title = element_text(size=14, face="bold"))+
  ylab("Difference from Previous Year CPUE") + xlab("Preceeding Winter CPUE (kg/d)")

library(patchwork)
p1+p2

#ggsave("Data/Winter catch to CPUE change.png", width = 21, height=21, units="cm", dpi=600)
p1 # FIGURE 3
#ggsave("Conceptual model/Figures/Winter catch to CPUE change.png", width = 21, height=21, units="cm", dpi=600)
#ggsave("Conceptual model/Figures/Winter catch to CPUE change.pdf", width = 21, height=21, units="cm", dpi=600)


### Annual plots
pTotal_ann <- ggplot(dat4, aes(CalendarYear, y = Total_Catch_Summer/1000)) + geom_line() + theme_classic()+
  ylab("Summer (January - April) Catch (t)")+
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(size=14, face="bold"))
pTotal_ann

coeff2 <- max(dat4$CPUE_Summer, na.rm=T)/max(dat4$Total_Catch_Summer/1000, na.rm=T)


p_annual <- pTotal_ann + scale_y_continuous(sec.axis = sec_axis(~.*coeff2, name="CPUE (kg / Scaled Fisher Day)")) +
  geom_line(data=dat4, aes(CalendarYear, y = (CPUE_Summer)/coeff2), col="red", linetype="dashed")+
  theme(axis.text.y.right = element_text(size=12, colour="red"),
        axis.title.y.right = element_text(face="bold", size=14, colour="red")) + xlab("Date")
p_annual
#ggsave("Conceptual model/Figures/Wallis Lake long term catch annual.png", width = 21, height=14.8, units="cm", dpi=600)
#ggsave("Conceptual model/Figures/Wallis Lake long term catch annual.pdf", width = 21, height=14.8, units="cm", dpi=600)

p_monthly + p_annual + plot_layout(nrow=2) # Figure 1
#ggsave("Conceptual model/Figures/Wallis Lake long term catch annual and monthly.png", width = 21, height=14.8, units="cm", dpi=600)
#ggsave("Conceptual model/Figures/Wallis Lake long term catch annual and monthly.pdf", width = 21, height=14.8, units="cm", dpi=600)



dat4 <- dat4 %>% mutate(lag_winter_Catch = lag(Total_Catch_Winter))
plot(dat4$Total_Catch_Summer ~ dat4$lag_winter_Catch)
dat4 <- dat4 %>% ungroup() %>% mutate(Winter_fish = case_when(CalendarYear >=2013 ~ "Yes",
                                                T ~ "No"),
                        mean_summer_catch = mean(Total_Catch_Summer, na.rm=T),
                        diff_from_summer_mean = Total_Catch_Summer-mean_summer_catch)
ggplot(dat4, aes(y= Summer_CPUE_Change,x= lag_winter_Catch, col=Winter_fish)) + geom_text(aes(label=CalendarYear))
ggplot(dat4, aes(y= diff_from_summer_mean,x= lag_winter_Catch, col=Winter_fish)) + geom_text(aes(label=CalendarYear))


dat4 <- dat4 %>% select(-15,-16)

write_csv(dat4, "Data/Annual Data for modelling.csv")


fit1 <- lm(Summer_CPUE_Change ~ lag_winter_CPUE, data=dat4)
plot(fit1)
summary(fit1)
dat4$Predicted_Summer_CPUE_change <- predict(fit1, newdata = dat4)

ggplot(dat4, aes(x=CalendarYear, y = Summer_CPUE_Change)) + geom_line() +
  geom_line(aes(y=Predicted_Summer_CPUE_change), col="red")

plot(dat4$Predicted_Summer_CPUE_change, dat4$Summer_CPUE_Change)

fit2 <- lm(CPUE_Summer ~ lag_winter_Catch+ Prev_summer_CPUE, data=dat4)
plot(fit2)
summary(fit2)

dat4$Predicted_Summer_CPUE <- predict(fit2, newdata = dat4)

plot(dat4$CPUE_Summer , type="l")
lines(dat4$Predicted_Summer_CPUE, col="red")

ggplot(dat4, aes(x=CalendarYear, y = CPUE_Summer)) + geom_line() +
  geom_line(aes(y=Predicted_Summer_CPUE), col="red")

# monthly ratios of annual
dat5 <- dat3 %>% group_by(CalendarYear) %>% summarise(Annual_catch = sum(Total_Catch, na.rm=T),
                                                      Annual_Effort = sum(Days_Scaled_Fished, na.rm=T),
                                                      Annual_CPUE = Annual_catch/Annual_Effort)

dat6 <- dat3 %>% left_join(dat5) %>% mutate(Catch_ratio = Total_Catch/Annual_catch) %>%
  filter(CalendarYear != 2022) %>% filter(CalendarYear != 1997)

ggplot(dat6, aes(CalendarYear, Catch_ratio)) + geom_line() + facet_wrap(~MonthMM) + 
  theme_classic() + theme(axis.text = element_text(colour="black", size=12),
                          axis.title = element_text(size=14, face="bold"))
#ggsave("Data/Wallis Lake monthly facet as ratio.png", width = 21, height=21, units="cm", dpi=600)

  
### Feb Catch
dat3f <- dat3 %>% filter(MonthMM == 3)
ggplot(dat3f, aes(CalendarYear, Total_Catch)) + geom_line() + theme_classic()
ggplot(dat3f, aes(CalendarYear, CPUE)) + geom_line() + ylab("Feb CPUE")+
  theme_classic() + theme(axis.text = element_text(colour="black", size=12),
                            axis.title = element_text(size=14, face="bold"))
#ggsave("Data/Wallis Lake Feb Catch.png", width = 21, height=21, units="cm", dpi=600)


# #### Try wind relationships
# head(dat3)
# 
# library(xts)
# wdat <- read_csv("Data/BARRA WALLIS Wind Data Clean.csv") %>% left_join(dat3) %>%
#   mutate(Difference_from_prev_year = xts::diff.xts(CPUE, 12)) %>% drop_na()
# 
# 
# ggplot(wdat, aes(Date, CPUE)) + geom_line() +
#   geom_line(aes(y=DownWind/1000), col="red")
# 
# ccf(wdat$DownWind, wdat$CPUE)
# ccfvalues3 <- ccf(wdat$DownWind, wdat$CPUE)
# ccfvalues3 # some relationships at -2, -3, -4
# 
# 
# #install.packages("astsa")
# library(astsa)
# lag2.plot (wdat$DownWind,wdat$CPUE,  11) 
# 
# ccf(wdat$`Max Discharge (ML/D)`, wdat$Effort) 
# ccf(wdat$`Mean Discharge (ML/D)`,wdat$Total_Catch) 
# 
# 
# 
# ### 1st differencing
# 
# wdat <- wdat 
# 
# ggplot(wdat, aes(Date, Difference_from_prev_year)) + geom_line() +
#   ylab("CPUE difference from previous year") +
#   theme_classic() + theme(axis.text = element_text(colour="black", size=12),
#                          axis.title = element_text(size=14, face="bold"))
# #ggsave("Data/CPUE difference from previous year.png",
# #       dpi=600, width=21, height=14.8, units="cm")
# 
# ccf(wdat$Difference_from_prev_year, wdat$DownWind)
# lag2.plot (wdat$Difference_from_prev_year, wdat$DownWind,  6)
