# Comparing time series - not this was a bit buggy at the end
library(tidyverse)

annual_catch <- read_csv("Data/Annual Data for modelling.csv") 
SST_dat <- read_csv("Data/Wallis offshore SST summary.csv") %>% rename(CalendarYear = Year)

dat1 <- annual_catch %>% left_join(SST_dat) %>%
  mutate(mean_SST_July_Sept_lag = lag(mean_SST.Jul_Sept),
         mean_SST_Nov_Dec_lag = lag(mean_SST.Nov_Dec))

cor.test(dat1$CPUE_Summer, dat1$mean_SST.Jan_Mar)
cor.test(dat1$Summer_CPUE_Change, dat1$mean_SST.Jan_Mar) # weak pattern

cor.test(dat1$CPUE_Summer, dat1$mean_SST_July_Sept_lag)
cor.test(dat1$Summer_CPUE_Change, dat1$mean_SST_July_Sept_lag)

cor.test(log(dat1$Total_Catch_Summer), dat1$mean_SST_Nov_Dec_lag)
cor.test(dat1$CPUE_Summer, dat1$mean_SST_Nov_Dec_lag) # very weak pattern
cor.test(dat1$Summer_CPUE_Change, dat1$mean_SST_Nov_Dec_lag)

p1 <- ggplot(dat1, aes(mean_SST_Nov_Dec_lag, Summer_CPUE_Change)) + geom_point() + geom_smooth(method="lm")
p2 <- ggplot(dat1, aes(mean_SST_July_Sept_lag, Summer_CPUE_Change)) + geom_point() + geom_smooth(method="lm")
p3 <- ggplot(dat1, aes(mean_SST.Jan_Mar, Summer_CPUE_Change)) + geom_point() + geom_smooth(method="lm")
p4 <- ggplot(dat1, aes(mean_SST_Nov_Dec_lag, CPUE_Summer)) + geom_point() + geom_smooth(method="lm")
p5 <- ggplot(dat1, aes(mean_SST_July_Sept_lag, CPUE_Summer)) + geom_point() + geom_smooth(method="lm")
p6 <- ggplot(dat1, aes(mean_SST.Jan_Mar, CPUE_Summer)) + geom_point() + geom_smooth(method="lm")


library(patchwork)
p1+ p2 + p3 + p4 + p5 + p6
#ggsave("Data/SST CPUE relationships.png", dpi =600, width = 21, height=18, units="cm")

ggplot(dat1, aes(CalendarYear, Summer_CPUE_Change)) + geom_line() + theme_classic()+
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14))+
  labs(x="Year", y = "Difference from Previous Summer CPUE")
#ggsave("Data/Annual CPUE change.png", dpi =600, width = 21, height=14.8, units="cm")


## Wind data 
wdat <- read_csv("Data/BARRA WALLIS Wind Data Clean Annual.csv") %>% rename(CalendarYear = Year)

dat2 <- dat1 %>% left_join(wdat) %>% mutate(UpWind_July_Sept_lag = lag(UpWind.Jul_Sept),
                                         UpWind_Nov_Dec_lag = lag(UpWind.Oct_Dec),
                                         DownWind_July_Sept_lag = lag(DownWind.Jul_Sept),
                                         DownWind_Nov_Dec_lag = lag(DownWind.Oct_Dec))

cor.test(dat2$CPUE_Summer, dat2$UpWind.Jan_Mar)
cor.test(dat2$Summer_CPUE_Change, dat2$UpWind.Jan_Mar) # weak positive
cor.test(dat2$mean_SST.Jan_Mar, dat2$UpWind.Jan_Mar) # correlated to temp as expected with upwelling


cor.test(dat2$CPUE_Summer, dat2$DownWind.Jan_Mar)
cor.test(dat2$Summer_CPUE_Change, dat2$DownWind.Jan_Mar)

cor.test(dat2$CPUE_Summer, dat2$UpWind_July_Sept_lag)
cor.test(dat2$Summer_CPUE_Change, dat2$UpWind_July_Sept_lag)
cor.test(dat2$CPUE_Summer, dat2$DownWind_July_Sept_lag)
cor.test(dat2$Summer_CPUE_Change, dat2$DownWind_July_Sept_lag)

cor.test(log(dat2$Total_Catch_Summer), dat2$DownWind_Nov_Dec_lag)
cor.test(dat2$CPUE_Summer, dat2$DownWind_Nov_Dec_lag)
cor.test(dat2$Summer_CPUE_Change, dat2$DownWind_Nov_Dec_lag)
cor.test(dat2$CPUE_Summer, dat2$UpWind_Nov_Dec_lag)
cor.test(dat2$Summer_CPUE_Change, dat2$UpWind_Nov_Dec_lag)

p17 <- ggplot(dat2, aes(UpWind.Jan_Mar, Summer_CPUE_Change)) + geom_point() + geom_smooth(method="lm")
p17
#ggsave("Data/Summer upwelling wind effect.png",dpi=600, width= 15, height=15, units="cm")


### Flow data
# fdat <-  read_csv("Data/Wallis Flow Data Clean with events.csv") %>% 
#   mutate(Date = as.Date(lubridate::dmy_hm(`Date and time`)),
#          Year = lubridate::year(Date),
#          Month = lubridate::month(Date)) %>% select(-Date)# %>% rename(CalendarYear = Year)
# 
# fdat <- fdat %>% mutate(July_Sept = case_when((Month>6 & Month <10) ~ "Yes",
#                                                                  T ~ "No"),
#                                            Oct_Dec = case_when((Month>=10) ~ "Yes",
#                                                                T ~ "No"),
#                                            Jan_Mar = case_when((Month<4) ~ "Yes",
#                                                                T ~ "No"))
# 
# 
# Jul_Sept_flow <- fdat %>% group_by(July_Sept, Year) %>%
#   summarise(mean_flow = mean(`Mean Discharge (ML/D)`, na.rm=T)) %>% filter(July_Sept == "Yes")
# 
# Oct_Dec_flow <- fdat %>% group_by(Oct_Dec, Year) %>%
#   summarise(mean_flow = mean(`Mean Discharge (ML/D)`, na.rm=T)) %>% filter(Oct_Dec == "Yes")
# 
# Jan_Mar_flow <- fdat %>% group_by(Jan_Mar, Year) %>%
#   summarise(mean_flow = mean(`Mean Discharge (ML/D)`, na.rm=T)) %>% filter(Jan_Mar == "Yes")
# 
# d1 <- Jul_Sept_flow %>% left_join(Oct_Dec_flow, by="Year", suffix = c(".Jul_Sept", ".Oct_Dec")) %>%
#   left_join(Jan_Mar_flow, by = "Year") %>% rename(mean_flow.Jan_Mar = mean_flow) %>% ungroup() %>%
#   select(-1,-4,-6)
# 
# write_csv(d1, "Data/Wallis Flow Data Annual periods.csv")
fdat <- read_csv("Data/Wallis Flow Data Annual periods.csv") %>% rename(CalendarYear = Year)
dat3 <- dat2 %>% left_join(fdat) %>% mutate(Flow_July_Sept_lag = lag(mean_flow.Jul_Sept),
                                            Flow_Oct_Dec_lag = lag(mean_flow.Oct_Dec))

cor.test(dat3$CPUE_Summer, dat3$mean_flow.Jan_Mar)
cor.test(dat3$Summer_CPUE_Change, dat3$mean_flow.Jan_Mar)

cor.test(dat3$CPUE_Summer, dat3$Flow_July_Sept_lag)
cor.test(dat3$Summer_CPUE_Change, dat3$Flow_July_Sept_lag)

cor.test(log(dat3$Total_Catch_Summer), dat3$Flow_Oct_Dec_lag)
cor.test(dat3$CPUE_Summer, dat3$Flow_Oct_Dec_lag)
cor.test(dat3$Summer_CPUE_Change, dat3$Flow_Oct_Dec_lag)



### PDO data
pdo_dat <- read_csv("Data/PDO data.csv")
#pdo_dat <- read_csv("Data/IPO data.csv")

psych::pairs.panels(pdo_dat)

ppp <- pdo_dat %>% rename(CalendarYear = Year)
pdo_long <- pdo_dat %>% pivot_longer(2:13, values_to = "PDO", names_to = "Month") %>%
  mutate(Fin_Year = case_when(Month == "Jan" ~ Year,
                              Month == "Feb" ~ Year,
                              Month == "Mar" ~ Year,
                              Month == "Apr" ~ Year,
                              Month == "May" ~ Year,
                              Month == "Jun" ~ Year,
                              T ~ Year + 1))
pdo_annual <- pdo_long %>% group_by(Year) %>% summarise(PDO_calendar_mean = mean(PDO, na.rm=T)) %>%
  rename(CalendarYear = Year)

datX <- dat2 %>% left_join(pdo_annual) %>% mutate(PDO_calendar_mean_lag = lag(PDO_calendar_mean))
#psych::pairs.panels(datX)

cor.test(datX$PDO_calendar_mean, datX$CPUE_Summer)
ggplot(datX, aes(PDO_calendar_mean, CPUE_Summer, label=CalendarYear)) + geom_label() + theme_minimal() + geom_smooth(method="lm")
#ggsave("Data/IPO calendar.png", height = 14.8, width=21, units="cm", dpi =600)

f1 <- lm(CPUE_Summer ~ PDO_calendar_mean + lag_winter_CPUE, data = datX)
summary(f1)
plot(effects::allEffects(f1))


datX$Pred_CPUE <- predict(f1, newdata = datX)
plot(datX$CPUE_Summer~ datX$CalendarYear, type="l")
lines(datX$Pred_CPUE~ datX$CalendarYear, type="l", col="red")

# Financial year now
pdo_long <- pdo_dat %>% pivot_longer(2:13, values_to = "PDO", names_to = "Month") %>%
  mutate(Fin_Year = case_when(Month == "Jan" ~ Year,
                              Month == "Feb" ~ Year,
                              Month == "Mar" ~ Year,
                              Month == "Apr" ~ Year,
                              Month == "May" ~ Year,
                              Month == "Jun" ~ Year,
                              T ~ Year + 1))
pdo_annual <- pdo_long %>% group_by(Fin_Year) %>% summarise(PDO_Fin_mean = mean(PDO, na.rm=T)) %>%
  rename(CalendarYear = Fin_Year)

datX <- datX %>% left_join(pdo_annual) %>% mutate(PDO_Fin_mean_lag = lag(PDO_Fin_mean))
#psych::pairs.panels(datX)

cor.test(datX$PDO_Fin_mean, datX$CPUE_Summer)
cor.test(datX$PDO_Fin_mean_lag, datX$CPUE_Summer)

ggplot(datX, aes(PDO_Fin_mean_lag, CPUE_Summer, label=CalendarYear)) + geom_label() + theme_minimal() + geom_smooth(method="lm")
#ggsave("Data/PDO financial_lag.png", height = 14.8, width=21, units="cm", dpi =600)

ggplot(datX, aes(PDO_Fin_mean, CPUE_Summer, label=CalendarYear)) + geom_label() + theme_minimal() + geom_smooth(method="lm")
#ggsave("Data/PDO financial.png", height = 14.8, width=21, units="cm", dpi =600)

### test part years
pdo_long <- pdo_dat %>% pivot_longer(2:13, values_to = "PDO", names_to = "Month") %>%
  mutate(Semester = case_when(Month == "Jan" ~ "1st",
                              Month == "Feb" ~ "1st",
                              Month == "Mar" ~ "1st",
                              Month == "Apr" ~ "1st",
                              Month == "May" ~ "1st",
                              Month == "Jun" ~ "1st",
                              T ~ "2nd"))
pdo_annual <- pdo_long %>% group_by(Year, Semester) %>% summarise(PDO_mean = mean(PDO, na.rm=T)) %>%
  rename(CalendarYear = Year) %>% pivot_wider(values_from = PDO_mean, names_from = Semester) %>%
  rename(PDO_Jan_Jun = `1st`, PDO_Jul_Dec = `2nd`)

datX <- datX %>% left_join(pdo_annual) %>% mutate(PDO_Jan_Jun_lag = lag(PDO_Jan_Jun),
                                                  PDO_Jul_Dec_lag = lag(PDO_Jul_Dec))
datX <- datX %>% left_join(ppp) %>% mutate(lag_Dec = lag(Dec),
                                           lag_Nov = lag(Nov),
                                           lag_Oct = lag(Oct),
                                           lag_Sep = lag(Sep),
                                           lag_Aug = lag(Aug),
                                           lag_July = lag(Jul)) %>%
  rowwise() %>% mutate(s_mean = mean(c(lag_Nov, lag_Dec, Jan), na.rm = TRUE))

cor.test(datX$CPUE_Summer, datX$lag_July)
cor.test(datX$CPUE_Summer, datX$lag_Aug)
cor.test(datX$CPUE_Summer, datX$lag_Sep)
cor.test(datX$CPUE_Summer, datX$lag_Oct)
cor.test(datX$CPUE_Summer, datX$lag_Nov)
cor.test(datX$CPUE_Summer, datX$lag_Dec)
cor.test(datX$CPUE_Summer, datX$Jan)
cor.test(datX$CPUE_Summer, datX$s_mean)


### Figure 6
ggplot(datX, aes(s_mean, CPUE_Summer)) + geom_point() + geom_smooth(method="lm")+
  theme_classic() + xlab("November - January mean PDO") + ylab("Summer CPUE (kg / Scaled Fisher Day)")+
  theme(axis.text = element_text(colour="black", size = 12),
        axis.title = element_text(face="bold", size=14))

#ggsave("Conceptual model/Figures/PDO correlations.png", width= 14.8, height=14.8, units = "cm", dpi=600)
#ggsave("Conceptual model/Figures/PDO correlations.pdf", width= 14.8, height=14.8, units = "cm", dpi=600)


#psych::pairs.panels(datX)
cor.test(datX$Prev_summer_CPUE, datX$CPUE_Summer)
cor.test(datX$PDO_Jan_Jun, datX$CPUE_Summer)
cor.test(datX$PDO_Jan_Jun, datX$Summer_CPUE_Change)
cor.test(datX$PDO_Jan_Jun_lag, datX$CPUE_Summer)

cor.test(datX$PDO_Jul_Dec, datX$CPUE_Summer)
cor.test(datX$PDO_Jul_Dec_lag, datX$CPUE_Summer)
cor.test(datX$PDO_Jul_Dec_lag, datX$Summer_CPUE_Change)

f2 <- lm(CPUE_Summer ~ s_mean  + lag_winter_Catch + Prev_summer_CPUE, data= datX)
summary(f2)
plot(effects::allEffects(f2))
resids <- DHARMa::simulateResiduals(f2)
plot(resids)
performance::r2(f2)

datX$Pred_CPUE <- predict(f2, newdata = datX)
plot(datX$CPUE_Summer~ datX$CalendarYear, type="l")
lines(datX$Pred_CPUE~ datX$CalendarYear, type="l", col="red")

### standardised
f2b <- lm(CPUE_Summer ~ scale(s_mean) + scale(lag_winter_Catch) + scale(Prev_summer_CPUE), data= datX)
summary(f2b)
plot(effects::allEffects(f2b))
resids <- DHARMa::simulateResiduals(f2b)
plot(resids)
performance::r2(f2b)

datX <- datX %>% mutate(good_bad = case_when(Total_Catch_Summer < 40000 ~ "Bad",
                                             T ~ "OK"))

mdat <- datX %>% dplyr::select(CPUE_Summer, s_mean , lag_winter_Catch , Prev_summer_CPUE, good_bad, CalendarYear) %>% rename(Year = CalendarYear) #%>%
mdat2 <- mdat %>% drop_na(CPUE_Summer, s_mean, lag_winter_Catch , Prev_summer_CPUE,good_bad)

extra_year <- data.frame(Year = 2022)
mdat <- bind_rows(mdat, extra_year)

psych::pairs.panels(mdat) # correlations OK

library(caret)

#specify the cross-validation method
ctrl <- trainControl(method = "LOOCV")

#fit a regression model and use LOOCV to evaluate performance # THIS IS MODEL IN PAPER
model <- train(CPUE_Summer ~ s_mean + lag_winter_Catch + Prev_summer_CPUE, data = mdat2, method = "lm", trControl = ctrl)
summary (model)

#view summary of LOOCV               
datCC <- predict(f2, newdata = mdat, se.fit=F, interval = "prediction")
plot(datCC)

mdat$Pred_CPUE <- predict(f2, newdata = mdat)
plot(mdat$CPUE_Summer, type="l")
lines(datCC[,1], type="l", col="red")
lines(datCC[,2], type="l", col="red")
lines(datCC[,3], type="l", col="red")
ddatCC <- as.data.frame(datCC)
ddatCC$Year <- seq(1997,2022,1)
#mdat$Year <- seq(1997,2022,1)


## FIGURE 7
ggplot(ddatCC, aes(x=Year, y = fit)) + geom_line() +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) +
  geom_line(data=mdat, aes(x=Year, y = CPUE_Summer), col="red") +
  theme_classic() + ylab("Jan - April Mean CPUE\n(kg / Scaled Fisher Day)") +
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14))+
  geom_text(data=NULL, aes(x = 2000, y =60, label= "Predicted CPUE"), inherit.aes = F)+
  geom_text(data=NULL, aes(x = 2000, y =53, label= "Observed CPUE"), col="red", inherit.aes = F)

#ggsave("Conceptual model/Figures/CPUE Predictions.png", height = 14.8, width=21, units="cm", dpi =600)
#ggsave("Conceptual model/Figures/CPUE Predictions.pdf", height = 14.8, width=21, units="cm", dpi =600)

plot(mdat$CPUE_Summer, mdat$Pred_CPUE)


#### binary predictions
# 
# 
# f3 <- glm(good_bad ~  s_mean + lag_winter_Catch + Prev_summer_CPUE, data= datX, family="binomial")
# summary(f3)
# plot(effects::allEffects(f3))
# resids <- DHARMa::simulateResiduals(f3)
# plot(resids)
# performance::r2(f3)
# plot(datX$good_bad ~ datX$CalendarYear)

f4 <- MASS::lda(good_bad ~  lag_winter_Catch + Prev_summer_CPUE, data= datX)
summary(f4)
f4

library(caret)

#specify the cross-validation method
ctrl <- trainControl(method = "LOOCV")

#fit a regression model and use LOOCV to evaluate performance
model <- train(good_bad ~ lag_winter_Catch + Prev_summer_CPUE, data = mdat2, method = "qda", trControl = ctrl)
summary (model)
plot(model)
#text(model$finalModel, use.n.=TRUE, all=TRUE, cex=.8)
model$results
model$pred
#view summary of LOOCV               
datCC <- predict(model, newdata = mdat, se.fit=F, interval = "prediction")
datCC
plot(datCC)
confusionMatrix(data = datCC, mfdat2$good_bad)

table(as.character(datCC), mfdat$good_bad)

#install.packages("klaR")
#install.packages("ggord")
library(klaR)
library(psych)
library(MASS)
library(ggord)
#library(devtools)
#remotes::install_github("fawda123/ggord")

fin_dat <- read_csv("Data/Wallis Financial Year Summary.csv") %>% 
  rename(Year = FinYear)

mfdat <- mdat %>% left_join(fin_dat)

ggplot(mfdat, aes(Year, Fin_year_CPUE, col=good_bad)) + geom_point()
ggplot(mfdat, aes(Year, Fin_year_Total_Catch, col=good_bad)) + geom_point()

mean(mfdat$Fin_year_CPUE, na.rm=T)
mean(mfdat$Fin_year_Total_Catch, na.rm=T)

mfdat <- mfdat %>% mutate(good_bad = case_when(Fin_year_Total_Catch < 78769.96 ~ "Below average",
                                               T ~ "Above average")) %>% filter(Year != 2022) %>%
  filter(Year != 1997)



mdat3 <- mdat2 %>% mutate(good_bad = as.factor(good_bad))
#ggord(f4, datX$good_bad, ylim = c(-10, 10))
partimat(good_bad ~ lag_winter_Catch + Prev_summer_CPUE, data = mdat3, method = "qda",
               imageplot=F, col.mean="blue")

str(mdat2$good_bad)

fff <- qda(good_bad ~ lag_winter_Catch + Prev_summer_CPUE, data = mdat3)
summary(fff)
fff$means

xx <- qda(good_bad ~ lag_winter_Catch + Prev_summer_CPUE, data = mdat3)
xx
xx$scaling
xx$counts
mdat3$Pred_QDA <- predict(xx, newdata = mdat3)[["posterior"]][,1]
mdat3 <- mdat3 %>% mutate(PRED_QDA = case_when(Pred_QDA >0.5 ~ "Bad",
                                               T ~ "OK"))

table(mdat3$good_bad, mdat3$PRED_QDA)
1 - 5/23 # 78% accuracy

ggplot(datX, aes(CalendarYear, CPUE_Summer, col=good_bad)) + geom_point()
ggplot(datX, aes(CalendarYear, Total_Catch_Summer, col=good_bad)) + geom_point()






xx <- qda(good_bad ~ lag_winter_Catch + Prev_summer_CPUE, data = mfdat)
xx
xx$scaling
xx$counts
mfdat$Pred_QDA <- predict(xx, newdata = mfdat)[["posterior"]][,1]
mfdat <- mfdat %>% mutate(PRED_QDA2 = case_when(Pred_QDA >0.5 ~ "Below average",
                                               T ~ "Above average"))

table(mfdat$good_bad, mfdat$PRED_QDA)
1 - 4/24 # 83% accuracy

mfdat2 <- mfdat %>% drop_na(good_bad, lag_winter_Catch, Prev_summer_CPUE) %>% filter(Year != 2022) %>%
  mutate(good_bad = as.factor(good_bad),
         Match = case_when(good_bad == PRED_QDA2 ~ "Match",
                           T ~ "No Match"))
par(mfrow=c(1,2))
partimat(good_bad ~lag_winter_Catch +  Prev_summer_CPUE, data = mfdat2, method = "qda",
         imageplot=F, col.mean=NA)

## Figure 8
ggplot(mfdat2, aes(Year, Fin_year_Total_Catch/1000)) + geom_line() + geom_point(aes(col=Match, shape=Match),size=2) +
  theme_classic() + geom_hline(yintercept = mean(mfdat$Fin_year_Total_Catch, na.rm=T)/1000, linetype=2) +
  scale_colour_manual(values=c("blue", "red"))+
  scale_x_continuous(breaks = seq(2000,2020,4))+
  xlab("Financial year ending") + ylab("Financial year total catch (t)")+
  theme(axis.text = element_text(size=10, colour="black"),
        axis.title = element_text(size=12, face="bold"),
        axis.ticks = element_line(colour="black"),
        legend.title = element_blank(),
        legend.text = element_text(size=10, colour="black"),
        legend.position = c(0.25,0.84),
        panel.border = element_rect(colour="black", fill=NA))

#ggsave("Data/Binary prediction by year.png", dpi =600, width = 7.4, height=7.4, units="cm")
#ggsave("Data/Binary prediction by year.pdf", dpi =600, width = 7.4, height=7.4, units="cm")

       
plot(mfdat2$Pred_QDA, mfdat2$Fin_year_Total_Catch)

