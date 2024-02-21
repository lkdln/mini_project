library(dplyr)
library(ggplot2)
library(tidyr)
library(viridis) #colour palette
library(ggpubr) # qqplot 
library(lme4) #model lmm
library(lmtest) #likelihood test 
library(nlme)
library(ggeffects)
library(sjPlot)
library(MuMIn)
library(psych)
library(car)


################################################################################
################################################################################
################################### Data  ######################################
################################################################################
################################################################################


### Upload gas emissions dataset for each catchments fields 
eddy <- read.csv('ghg_ghgeddy_2018-2021/greenhouse_aggregated.csv')
# subset to focus on CO2 and CH4 concentration for catchment 4 and 9 
eddy <- subset(eddy, select=c(Datetime, CH4_1_1_1..Tower.4., CH4_1_1_1..Tower.9.))
eddy$Date <- as.Date(eddy$Datetime, format="%d/%m/%Y") #create a new column for date only 
eddy$Datetime <- as.POSIXct(eddy$Datetime, format = "%d/%m/%Y %H:%M") # convert datetime column type from chr to dttm
#remove occurence before july 20 - catchment 9 didn't have any records of ch4 + catchment 4 co2 radar was down 
eddy <- eddy %>% filter(eddy$Datetime >='2020-07-01 00:00:00')
# pivot to get the gas value as rows 
eddy <- pivot_longer(eddy, cols = CH4_1_1_1..Tower.4. : CH4_1_1_1..Tower.9., names_to = "Catchment", values_to = "CH4.values")
# add a column for corresponding farmlet depending on the UNIT ID (mapping referenced in NWFP user guide)
eddy$Catchment <- ifelse(grepl("Tower.4", eddy$Catchment, ignore.case = TRUE), "Catchment 4",
                         ifelse(grepl("Tower.9", eddy$Catchment, ignore.case = TRUE), "Catchment 9", NA))
eddy<- eddy[!(is.na(eddy$CH4.values)), ] # remove entry with no CH4 recordings 
boxplot(CH4.values ~ Catchment, data=eddy) # no outliers spotted 
glimpse(eddy)
ggplot() + geom_line(eddy, mapping=aes(x=Date, y=CH4.values, color=Catchment))
## Prepare the dataset with CH4 emissions 
eddy <- eddy %>% group_by(Catchment, Date)  %>% summarise(CH4.values= mean(CH4.values, na.rm=TRUE))
eddy <- eddy %>% ungroup()
eddy <- as.data.frame(eddy)

### upload livestock location dataset for each catchment fields 
sheep_loc <- read.csv('livestock_2018-2021/sheep_location_aggregated.csv')
lamb_loc <- read.csv('livestock_2018-2021/lamb_location_aggregated.csv')
cattle_loc <- read.csv('livestock_2018-2021/cattle_location_aggregated.csv')
# add livestock category for analysis down the line 
sheep_loc$Species <- "Sheep"
lamb_loc$Species <- "Lamb"
cattle_loc$Species <- "Cattle"
# create a dataset for all livestock location 
loc <- bind_rows(sheep_loc, lamb_loc, cattle_loc) # aggregate
loc <- subset(loc, select=c(Date, Species, Catchment.4.After..2013.08.13, Catchment.9))  # select relevant columns 
names(loc)[names(loc) == 'Catchment.4.After..2013.08.13'] <- 'Catchment.4' #rename the column for ease 
loc$Date <- as.Date(loc$Date, format= "%d/%m/%Y") # reformat date and get datetime 
## Prepare the dataset with livestock count per catchment per date 
loc <- loc %>% pivot_longer(cols = Catchment.4 : Catchment.9, names_to = "Catchment", values_to = "Count.animal") # pivot catchment 4 and 9 column to one catchment columns
#loc <- loc %>% pivot_wider(names_from=Species, values_from=Count.animal) # pivot the livestock column into three columns per livestock category 
loc$Catchment <- ifelse(grepl("Catchment.4", loc$Catchment, ignore.case = TRUE), "Catchment 4", 
                        ifelse(grepl("Catchment.9", loc$Catchment , ignore.case = TRUE), "Catchment 9",NA))
loc <- as.data.frame(loc)

##### SOIL DATA
soil <- read.csv('soil_aggregated.csv')
soil <- subset(soil, select = -c(Soil.Temperature...15cm.Depth..oC...Catchment.2., Soil.Moisture...10cm.Depth......Catchment.2.))
colnames(soil)[colnames(soil) == "Soil.Temperature...15cm.Depth..oC...Catchment.4.After..2013.08.13."] <- "soil.temp.catchment.4"
colnames(soil)[colnames(soil) == "Soil.Temperature...15cm.Depth..oC...Catchment.9."] <- "soil.temp.catchment.9"
colnames(soil)[colnames(soil) == "Soil.Moisture...10cm.Depth......Catchment.4.After..2013.08.13."] <- "soil.moist.catchment.4"
colnames(soil)[colnames(soil) == "Soil.Moisture...10cm.Depth......Catchment.9."] <- "soil.moist.catchment.9"
soil <- soil[!(is.na(soil$soil.temp.catchment.4) | is.na(soil$soil.temp.catchment.9) | is.na(soil$soil.moist.catchment.4) | is.na(soil$soil.moist.catchment.9)), ]
head(soil)
soil$date <- as.Date(soil$Datetime, format="%d/%m/%Y" )
soil <- subset(soil, select=-c(Datetime))
soil<- soil %>% pivot_longer(cols = soil.temp.catchment.4 : soil.moist.catchment.9, names_to = "measure", values_to = "values")
soil$catchment <-  ifelse(grepl("catchment.4", soil$measure, ignore.case = TRUE), "Catchment 4", 
                          ifelse(grepl("catchment.9", soil$measure, ignore.case = TRUE), "Catchment 9", 0))
soil$measure <-  ifelse(grepl("soil.temp", soil$measure, ignore.case = TRUE), "soil.temp", 
                        ifelse(grepl("soil.moist", soil$measure, ignore.case = TRUE), "soil.moist", 0))
soil <- soil %>% group_by(date, measure, catchment) %>% summarise(values=mean(values, na.rm=TRUE))
soil <- soil %>% pivot_wider(names_from=measure, values_from=values)
soil <- soil %>% ungroup()
soil <- as.data.frame(soil)


### MET WEATHER DATA 
met <- read.csv('met_aggregated.csv')
met <- met[, !grepl("Quality", names(met))] # drop all columns with the ekywoord Site..Quality
names(met) <- tolower(names(met)) #normalise the column name for ease 
met <- subset(met, select=-c(solar.radiation..w.m2...site., wind.direction..o...site.,wind.speed..km.h...site.))
colnames(met)[colnames(met) == "precipitation..mm...site."] <- "precipitation.mm"
colnames(met)[colnames(met) == "air.temperature..oc...site."] <- "air.temperature.oc"
colnames(met)[colnames(met) == "relative.humidity....rh...site."] <- "relative.humidity.rh"
#met$datetime <- as.POSIXct(met$datetime, format = "%d/%m/%Y %H:%M") # convert datetime column type from chr to dttm
met$date <- as.Date(met$datetime, format="%d/%m/%Y" )
met <- met %>% group_by(date) %>% summarise(precipitation.mm= sum(precipitation.mm, na.rm=T), air.temperature.oc = mean(air.temperature.oc, na.rm=T),
                                            relative.humidity.rh=mean(relative.humidity.rh, na.rm =T))
# add a catchment column to prepare for merged dataset 
met4 <- met 
met4$catchment <- "Catchment 4"
met9 <- met 
met9$catchment <- "Catchment 9"
met <- bind_rows(met4, met9) # aggregate
met <- met[!(is.na(met$date)), ]
# check collinearity
met <- as.data.frame(met)
pairs.panels(met) # check corrolation 
#max corr 0.32 - all good to keep  



# MERGE DATA 
merged_data <- merge(eddy, loc, by = c("Date", "Catchment") , all = TRUE) # ch4 emissions and livestock counts merge 
names(merged_data) <- tolower(names(merged_data)) #normalise the column name for ease 
merged_data$catchmentnb <- ifelse(grepl("Catchment 4", merged_data$catchment, ignore.case = TRUE), 1, 
                                  ifelse(grepl("Catchment 9", merged_data$catchment, ignore.case = TRUE),2, 0))  # categorise the catchment 
merged_data$ch4.values <- ifelse(merged_data$ch4.values == "NaN", NA, merged_data$ch4.values) # replace NA string as empty NaN 
#add met data 
merged_data <- merge(merged_data, met, by=c("date", "catchment"), all=T) # add weather data 
merged_data<- merged_data[!(is.na(merged_data$ch4.values)), ] # remove empty NA CH4 rows
merged_data <- merge(merged_data, soil, by=c("date", "catchment"), all=T) # add soil data 
summary(merged_data)



################################################################################
################################################################################
################################### Model ######################################
################################################################################
################################################################################


pairs.panels(merged_data) # check corrolation 
# corrolation found on air temperaure, soil moisture and soil temperature removed  ( 0.72,0.92, 0.65 )

m1 <- lmer(ch4.values ~ species * catchment * scale(count.animal) + (1 | date) , data = merged_data)

m2 <- lmer(ch4.values ~ species * scale(count.animal) * catchment * scale(precipitation.mm + relative.humidity.rh) + (1 | date), data = merged_data, REML=T)

m3 <- lmer(ch4.values ~ species * scale(count.animal) * catchment * scale(precipitation.mm * relative.humidity.rh) + (1 | date), data = merged_data, REML=T)

m4 <- lmer(ch4.values ~ species * scale(count.animal) * catchment * scale(precipitation.mm * relative.humidity.rh) + (scale(precipitation.mm + relative.humidity.rh)| date) + (1 | date) , data = merged_data, REML=T)

lrtest(m1,m2, m3, m4)
AIC(m1)
AIC(m2)
AIC(m3)
AIC(m4)
# vif(m3[,-c(1,2,4)]) # not including date and catchment 
# vif(m3[,-c(1,2,4, 8,10,11)]) # air temperaure, soil moisture and soil temperature removed due to their VIF > 3 .
summary(m4)
r.squaredGLMM(m4) # conditional and marginal rsquared calculation
# we only have one random effect so we're not looking at removing any 
# now looking at the fixed effects
plot2 <- ggpredict(m4, terms=c("catchment", "species"))
plot(plot2, add.data = T, jitter =T) + 
  labs(y="CH4 concentration (mmol CH4 mol-1 unit)", x="") + 
  theme(axis.text=element_text(size=17),
        axis.title=element_text(size=17,face="bold"),
        legend.text = element_text(size=17),
        strip.text= element_text(size=17))

par(font.lab = 100, cex.lab = 1.5, cex.main = 2) # Adjust font size as needed
tab_model(m4,  dv.labels=c("lmer( CH4 ~ species * scale(count.animal) * catchment * scale(precipitation.mm * relative.humidity.rh) +
                           (scale(air.temperature.oc + relative.humidity.rh)| date) + (1 | date)) "),
          show.se=T,  show.icc = F,show.ngroups = F,  show.re.var =T,
          wrap.labels = 35) 

plot_model(m4,vline.color = "red",sort.est = TRUE, title = NULL,
           axis.title = NULL,
           axis.labels = NULL,
           legend.title = NULL,)




########### 
########### 
########### PRESENTATION PLOTTY PLOT
########### 
########### 

eddy4 <- eddy %>% filter(eddy$Catchment=="Catchment 4") # subset for catchment 4
eddy9 <- eddy %>% filter(eddy$Catchment=="Catchment 9") # same for catchment 9
events4 <- events %>% filter(events$Catchment=="Catchment 4") # subset for catchment 4
events9 <- events %>% filter(events$Catchment=="Catchment 9") # same for catchment 9
random_colors4 <-viridis_pal()(length(unique(events4$Field_Operation))) #get how many Field Operation happen to sample right amount of colours
random_colors9 <-viridis_pal()(length(unique(events9$Field_Operation)))
eddy4co2 <- eddy4 %>% filter(Type=="CO2")
eddy4ch4 <- eddy4 %>% filter(Type=="CH4")
eddy9co2 <- eddy9 %>% filter(Type=="CO2")
eddy9ch4 <- eddy9 %>% filter(Type=="CH4")

# Plot catchment 4 , CO2 concentration 
gg4_co2 <- ggplot() +
  geom_point(data = eddy4co2, aes(x = Datetime, y = Values, color = Type)) +
  scale_color_manual(values = c("CO2" = "grey", "CH4" = "brown"), name = "Gas Concentration")
gg4_co2 <- gg4_co2 + new_scale_colour() + 
  geom_segment(data = events4, aes(x = as.POSIXct(Date_IN), xend = as.POSIXct(Date_OUT), y = 0, yend = Inf, color = Field_Operation), linetype = "solid") +
  scale_color_manual(values = random_colors4, name = "Field Operations")+
  scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") + 
  labs(x = element_blank(), y = "Carbon (CO2) Concentration co2 mmol CO2 mol-1]", title = "Catchment 4 ") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
# Plot catchment 9 , CO2 concentration 
gg9_co2 <- ggplot() +
  geom_point(data = eddy9co2, aes(x = Datetime, y = Values, color = Type)) +
  scale_color_manual(values = c("CO2" = "grey","CH4" = "brown"), name = "Gas Concentration")
gg9_co2 <- gg9_co2 + new_scale_colour() + 
  geom_segment(data = events9, aes(x = as.POSIXct(Date_IN), xend = as.POSIXct(Date_OUT), y = 0, yend = Inf, color = Field_Operation), linetype = "solid") +
  scale_color_manual(values = random_colors9, name = "Field Operations")+
  scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") + 
  labs(x = element_blank(), y = element_blank(), title = "Catchment 9 ") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
# Plot catchment 4 , CH4 concentration 
gg4_ch4 <- ggplot() +
  geom_point(data = eddy4ch4, aes(x = Datetime, y = Values, color = Type)) +
  scale_color_manual(values = c("CO2" = "grey","CH4" = "brown"), name = "Gas Concentration")
gg4_ch4 <- gg4_ch4 + new_scale_colour() + 
  geom_segment(data = events4, aes(x = as.POSIXct(Date_IN), xend = as.POSIXct(Date_OUT), y = 0, yend = Inf, color = Field_Operation), linetype = "solid") +
  scale_color_manual(values = random_colors4, name = "Field Operations")+
  scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") + 
  scale_y_continuous(name = "Methane (CH4) Concentration [nmol CH4 mol-1]", limits = c(0, 4000)) +
  labs(x = "Datetime") +
  theme(axis.text.x = element_text(color="black", size=8, angle=45))
# Plot catchment 9 , CH4 concentration 
gg9_ch4 <- ggplot() +
  geom_point(data = eddy9ch4, aes(x = Datetime, y = Values, color = Type)) +
  scale_color_manual(values = c("CO2" = "grey","CH4" = "brown"), name = "Gas Concentration")
gg9_ch4 <- gg9_ch4 + new_scale_colour() + 
  geom_segment(data = events9, aes(x = as.POSIXct(Date_IN), xend = as.POSIXct(Date_OUT), y = 0, yend = Inf, color = Field_Operation), linetype = "solid") +
  scale_color_manual(values = random_colors9, name = "Field Operations")+
  scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") + 
  scale_y_continuous(limits=c(0,4000)) + 
  labs(x = "Datetime", y = element_blank()) +
  theme(axis.text.x = element_text(color="black", size=8, angle=45))
### aggregate the 4 ggplot into one 
ggarrange(gg4_co2, gg9_co2, gg4_ch4,gg9_ch4, 
          nrow = 2 , ncol = 2, common.legend = T, heights = c(5,5,5), widths=c(10,10,10)) 


