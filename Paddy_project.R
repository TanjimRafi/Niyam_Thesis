# Please set the working directory
setwd("E:/Niyam_STAT")

# Load libraries
library(TSA)
library(astsa)
library(readxl)
library(lmtest)
library(tseries)
library(forecast)
library(lubridate)
library(tidyverse)

################################################################################
#################################### Dhaka #####################################
################################################################################

# Dhaka station(2008-2017)
dhaka1 <- read_excel("E:/Niyam_STAT/2008_17/Rainfall_2008-2017/Dhaka.xls")
dhaka1$DATE <- dmy(dhaka1$DATE)
dhaka1 <- dhaka1 %>% filter(Station == "Dhaka_Banani") %>% select(District,DATE,`Rainfall(mm)`)
colnames(dhaka1) <- c("District","Date","Rainfall(mm)")
dhaka1$Year <- year(dhaka1$Date)

# Dhaka station(2018)
dhaka2 <- read_excel("E:/Niyam_STAT/BMD Data/Daily Total Rainfall.xlsx")
colnames(dhaka2) <- dhaka2[6,]
dhaka2 <- dhaka2 %>%
  filter(str_detect(Stati, "Dhaka")) %>%
  pivot_longer(!c(Stati,Year,Month), names_to = "Day", values_to = "Rainfall(mm)")
dhaka2$DATE <- ymd(paste(dhaka2$Year, dhaka2$Month, dhaka2$Day, sep = "-")) 
dhaka2 <- dhaka2 %>% drop_na(DATE) %>% select(Stati,DATE,`Rainfall(mm)`,Year)
colnames(dhaka2) <- c("District","Date","Rainfall(mm)","Year")

# Dhaka station(2019)
dhaka3 <- read_excel("E:/Niyam_STAT/19.xls")
colnames(dhaka3) <- dhaka3[6,]
dhaka3 <- dhaka3 %>%
  filter(str_detect(Stati, "Dhaka")) %>%
  pivot_longer(!c(Stati,Year,Month), names_to = "Day", values_to = "Rainfall(mm)")
dhaka3$DATE <- ymd(paste(dhaka3$Year, dhaka3$Month, dhaka3$Day, sep = "-")) 
dhaka3 <- dhaka3 %>% drop_na(DATE) %>% select(Stati,DATE,`Rainfall(mm)`,Year)
colnames(dhaka3) <- c("District","Date","Rainfall(mm)","Year")

# Dhaka station(2020)
dhaka4 <- read_excel("E:/Niyam_STAT/20.xls")
dhaka4 <- dhaka4 %>%
  filter(str_detect(Stations, "Dhaka")) %>%
  pivot_longer(!c(Stations,Station_ID,Lat,Lon,Year,Month), names_to = "Day", values_to = "Rainfall(mm)") 
dhaka4$Day <- gsub("D_","",as.character(dhaka4$Day))
dhaka4$DATE <- ymd(paste(dhaka4$Year, dhaka4$Month, dhaka4$Day, sep = "-")) 
dhaka4 <- dhaka4 %>% drop_na(DATE) %>% select(Stations,DATE,`Rainfall(mm)`,Year)
dhaka4$`Rainfall(mm)`<- as.numeric(dhaka4$`Rainfall(mm)`)
dhaka4[is.na(dhaka4)] <- 0
colnames(dhaka4) <- c("District","Date","Rainfall(mm)","Year")

# Dhaka Station(2008-2020) -> Clean data
dhaka <- rbind(dhaka1,dhaka2,dhaka3,dhaka4)

### Total Rainfall amount for each year ###
dhaka %>%
  ggplot(mapping = aes(x = Date, y = `Rainfall(mm)`)) + 
  geom_line(color = "steelblue") +
  facet_wrap(Year ~., ncol = 3, scales = "free") +
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 day", date_labels = "%d-%b") +
  scale_y_continuous(n.breaks = 10) +
  theme(axis.text.x = element_text(angle = 0, size = 7)) +
  theme(plot.margin = unit(c(1,1,1,1), "mm")) +
  ggtitle(label = "Rainfall(mm) in Dhaka(2008-2020)") +
  ylab("Total Rainfall(mm)/Day")

dhaka$Year <- as.factor(dhaka$Year)
dhaka$DayOfYear <- as.numeric(format(dhaka$Date, "%j"))

### Total Rainfall amount for all years ###
dhaka %>%
  ggplot(mapping = aes(x = DayOfYear, y = `Rainfall(mm)`, group = Year, color = Year)) + 
  geom_line() +
  scale_x_continuous(breaks = c(1,32,61,92,122,153,183,214,245,275,306,336),
                     labels = c("01-Jan","01-Feb","01-Mar","01-Apr","01-May","01-Jun",
                                "01-Jul","01-Aug","01-Sep","01-Oct","01-Nov","01-Dec")) +
  scale_y_continuous(n.breaks = 10) +
  theme(axis.text.x = element_text(angle = 0, size = 7)) +
  theme(plot.margin = unit(c(1,1,1,1), "mm")) +
  theme_classic() +
  ggtitle(label = "Rainfall(mm) in Dhaka(2008-2020)") +
  ylab("Total Rainfall(mm)/Day") +
  xlab("Date")

################################################################################
################################## Rajshahi ####################################
################################################################################

# Rajshahi station(2008-2017)
raj1 <- read_excel("E:/Niyam_STAT/2008_17/Rainfall_2008-2017/Rajshahi.xls")
raj1$DATE <- dmy(raj1$DATE)
raj1 <- raj1 %>%  filter(Station == "Rajshahi") %>% select(District,DATE,`Rainfall(mm)`)
colnames(raj1) <- c("District","Date","Rainfall(mm)")
raj1$Year <- year(raj1$Date)

# Rajshahi station(2018)
raj2 <- read_excel("E:/Niyam_STAT/BMD Data/Daily Total Rainfall.xlsx")
colnames(raj2) <- raj2[6,]
raj2 <- raj2 %>%
  filter(str_detect(Stati, "Rajshahi")) %>%
  pivot_longer(!c(Stati,Year,Month), names_to = "Day", values_to = "Rainfall(mm)")
raj2$DATE <- ymd(paste(raj2$Year, raj2$Month, raj2$Day, sep = "-")) 
raj2 <- raj2 %>% drop_na(DATE) %>% select(Stati,DATE,`Rainfall(mm)`,Year)
colnames(raj2) <- c("District","Date","Rainfall(mm)","Year")

# Rajshahi station(2019)
raj3 <- read_excel("E:/Niyam_STAT/19.xls")
colnames(raj3) <- raj3[6,]
raj3 <- raj3 %>%
  filter(str_detect(Stati, "Rajshahi")) %>%
  pivot_longer(!c(Stati,Year,Month), names_to = "Day", values_to = "Rainfall(mm)")
raj3$DATE <- ymd(paste(raj3$Year, raj3$Month, raj3$Day, sep = "-")) 
raj3 <- raj3 %>% drop_na(DATE) %>% select(Stati,DATE,`Rainfall(mm)`,Year)
colnames(raj3) <- c("District","Date","Rainfall(mm)","Year")

# Rajshahi station(2020)
raj4 <- read_excel("E:/Niyam_STAT/20.xls")
raj4 <- raj4 %>%
  filter(str_detect(Stations, "Rajshahi")) %>%
  pivot_longer(!c(Stations,Station_ID,Lat,Lon,Year,Month), names_to = "Day", values_to = "Rainfall(mm)") 
raj4$Day <- gsub("D_","",as.character(raj4$Day))
raj4$DATE <- ymd(paste(raj4$Year, raj4$Month, raj4$Day, sep = "-")) 
raj4 <- raj4 %>% drop_na(DATE) %>% select(Stations,DATE,`Rainfall(mm)`,Year)
raj4$`Rainfall(mm)`<- as.numeric(raj4$`Rainfall(mm)`)
raj4[is.na(raj4)] <- 0
colnames(raj4) <- c("District","Date","Rainfall(mm)","Year")

# Rajshahi Station(2008-2020) -> Clean data
rajshahi <- rbind(raj1,raj2,raj3,raj4) 

### Total Rainfall amount for each year ###
rajshahi %>%
  ggplot(mapping = aes(x = Date, y = `Rainfall(mm)`)) + 
  geom_line(color = "steelblue") +
  facet_wrap(Year ~., ncol = 3, scales = "free") +
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 day", date_labels = "%d-%b") +
  scale_y_continuous(n.breaks = 10) +
  theme(axis.text.x = element_text(angle = 0, size = 7)) +
  theme(plot.margin = unit(c(1,1,1,1), "mm")) +
  ggtitle(label = "Rainfall(mm) in Rajshahi(2008-2020)") +
  ylab("Total Rainfall(mm)/Day")

rajshahi$Year <- as.factor(rajshahi$Year)
rajshahi$DayOfYear <- as.numeric(format(rajshahi$Date, "%j"))

### Total Rainfall amount for all years ###
rajshahi %>%
  ggplot(mapping = aes(x = DayOfYear, y = `Rainfall(mm)`, group = Year, color = Year)) + 
  geom_line() +
  scale_x_continuous(breaks = c(1,32,61,92,122,153,183,214,245,275,306,336),
                     labels = c("01-Jan","01-Feb","01-Mar","01-Apr","01-May","01-Jun",
                                "01-Jul","01-Aug","01-Sep","01-Oct","01-Nov","01-Dec")) +
  scale_y_continuous(n.breaks = 10) +
  theme(axis.text.x = element_text(angle = 0, size = 7)) +
  theme(plot.margin = unit(c(1,1,1,1), "mm")) +
  theme_classic() +
  ggtitle(label = "Rainfall(mm) in Rajshahi(2008-2020)") +
  ylab("Total Rainfall(mm)/Day") +
  xlab("Date")

################################################################################
#################################### Khulna ####################################
################################################################################

# Khulna station(2008-2017)
khulna1 <- read_excel("E:/Niyam_STAT/2008_17/Rainfall_2008-2017/Khulna.xls")
khulna1$DATE <- dmy(khulna1$DATE)
khulna1 <- khulna1 %>% filter(Station == "Khulna") %>% select(District,DATE,`Rainfall(mm)`)
colnames(khulna1) <- c("District","Date","Rainfall(mm)")
khulna1$Year <- year(khulna1$Date)

# Khulna station(2018)
khulna2 <- read_excel("E:/Niyam_STAT/BMD Data/Daily Total Rainfall.xlsx")
colnames(khulna2) <- khulna2[6,]
khulna2 <- khulna2 %>%
  filter(str_detect(Stati, "Khulna")) %>%
  pivot_longer(!c(Stati,Year,Month), names_to = "Day", values_to = "Rainfall(mm)")
khulna2$DATE <- ymd(paste(khulna2$Year, khulna2$Month, khulna2$Day, sep = "-")) 
khulna2 <- khulna2 %>% drop_na(DATE) %>% select(Stati,DATE,`Rainfall(mm)`,Year)
colnames(khulna2) <- c("District","Date","Rainfall(mm)","Year")

# Khulna station(2019)
khulna3 <- read_excel("E:/Niyam_STAT/19.xls")
colnames(khulna3) <- khulna3[6,]
khulna3 <- khulna3 %>%
  filter(str_detect(Stati, "Khulna")) %>%
  pivot_longer(!c(Stati,Year,Month), names_to = "Day", values_to = "Rainfall(mm)")
khulna3$DATE <- ymd(paste(khulna3$Year, khulna3$Month, khulna3$Day, sep = "-")) 
khulna3 <- khulna3 %>% drop_na(DATE) %>% select(Stati,DATE,`Rainfall(mm)`,Year)
colnames(khulna3) <- c("District","Date","Rainfall(mm)","Year")

# Khulna station(2020)
khulna4 <- read_excel("E:/Niyam_STAT/20.xls")
khulna4 <- khulna4 %>%
  filter(str_detect(Stations, "Khulna")) %>%
  pivot_longer(!c(Stations,Station_ID,Lat,Lon,Year,Month), names_to = "Day", values_to = "Rainfall(mm)") 
khulna4$Day <- gsub("D_","",as.character(khulna4$Day))
khulna4$DATE <- ymd(paste(khulna4$Year, khulna4$Month, khulna4$Day, sep = "-")) 
khulna4 <- khulna4 %>% drop_na(DATE) %>% select(Stations,DATE,`Rainfall(mm)`,Year)
khulna4$`Rainfall(mm)`<- as.numeric(khulna4$`Rainfall(mm)`)
khulna4[is.na(khulna4)] <- 0
colnames(khulna4) <- c("District","Date","Rainfall(mm)","Year")

# Khulna Station(2007-2020) -> Clean data
khulna <- rbind(khulna1,khulna2,khulna3,khulna4) 

### Total Rainfall amount for each year ###
khulna %>%
  ggplot(mapping = aes(x = Date, y = `Rainfall(mm)`)) + 
  geom_line(color = "steelblue") +
  facet_wrap(Year ~., ncol = 3, scales = "free") +
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 day", date_labels = "%d-%b") +
  scale_y_continuous(n.breaks = 10) +
  theme(axis.text.x = element_text(angle = 0, size = 7)) +
  theme(plot.margin = unit(c(1,1,1,1), "mm")) +
  ggtitle(label = "Rainfall(mm) in Khulna(2008-2020)") +
  ylab("Total Rainfall(mm)/Day")

khulna$Year <- as.factor(khulna$Year)
khulna$DayOfYear <- as.numeric(format(khulna$Date, "%j"))

### Total Rainfall amount for all years ###
khulna %>%
  ggplot(mapping = aes(x = DayOfYear, y = `Rainfall(mm)`, group = Year, color = Year)) + 
  geom_line() +
  scale_x_continuous(breaks = c(1,32,61,92,122,153,183,214,245,275,306,336),
                     labels = c("01-Jan","01-Feb","01-Mar","01-Apr","01-May","01-Jun",
                                "01-Jul","01-Aug","01-Sep","01-Oct","01-Nov","01-Dec")) +
  scale_y_continuous(n.breaks = 10) +
  theme(axis.text.x = element_text(angle = 0, size = 7)) +
  theme(plot.margin = unit(c(1,1,1,1), "mm")) +
  theme_classic() +
  ggtitle(label = "Rainfall(mm) in Khulna(2008-2020)") +
  ylab("Total Rainfall(mm)/Day") +
  xlab("Date")

################################################################################
#################################### Sylhet ####################################
################################################################################

# Sylhet station(2008-2017)
sylhet1 <- read_excel("E:/Niyam_STAT/2008_17/Rainfall_2008-2017/Sylhet.xls")
sylhet1$DATE <- dmy(sylhet1$DATE)
sylhet1 <- sylhet1 %>% filter(Station == "Sylhet") %>% select(District,DATE,`Rainfall(mm)`)
colnames(sylhet1) <- c("District","Date","Rainfall(mm)")
sylhet1$Year <- year(sylhet1$Date)

# Sylhet station(2018)
sylhet2 <- read_excel("E:/Niyam_STAT/BMD Data/Daily Total Rainfall.xlsx")
colnames(sylhet2) <- sylhet2[6,]
sylhet2 <- sylhet2 %>%
  filter(str_detect(Stati, "Sylhet")) %>%
  pivot_longer(!c(Stati,Year,Month), names_to = "Day", values_to = "Rainfall(mm)")
sylhet2$DATE <- ymd(paste(sylhet2$Year, sylhet2$Month, sylhet2$Day, sep = "-")) 
sylhet2 <- sylhet2 %>% drop_na(DATE) %>% select(Stati,DATE,`Rainfall(mm)`,Year)
colnames(sylhet2) <- c("District","Date","Rainfall(mm)","Year")

# Sylhet station(2019)
sylhet3 <- read_excel("E:/Niyam_STAT/19.xls")
colnames(sylhet3) <- sylhet3[6,]
sylhet3 <- sylhet3 %>%
  filter(str_detect(Stati, "Sylhet")) %>%
  pivot_longer(!c(Stati,Year,Month), names_to = "Day", values_to = "Rainfall(mm)")
sylhet3$DATE <- ymd(paste(sylhet3$Year, sylhet3$Month, sylhet3$Day, sep = "-")) 
sylhet3 <- sylhet3 %>% drop_na(DATE) %>% select(Stati,DATE,`Rainfall(mm)`,Year)
colnames(sylhet3) <- c("District","Date","Rainfall(mm)","Year")

# Sylhet station(2020)
sylhet4 <- read_excel("E:/Niyam_STAT/20.xls")
sylhet4 <- sylhet4 %>%
  filter(str_detect(Stations, "Sylhet")) %>%
  pivot_longer(!c(Stations,Station_ID,Lat,Lon,Year,Month), names_to = "Day", values_to = "Rainfall(mm)") 
sylhet4$Day <- gsub("D_","",as.character(sylhet4$Day))
sylhet4$DATE <- ymd(paste(sylhet4$Year, sylhet4$Month, sylhet4$Day, sep = "-")) 
sylhet4 <- sylhet4 %>% drop_na(DATE) %>% select(Stations,DATE,`Rainfall(mm)`,Year)
sylhet4$`Rainfall(mm)`<- as.numeric(sylhet4$`Rainfall(mm)`)
sylhet4[is.na(sylhet4)] <- 0
colnames(sylhet4) <- c("District","Date","Rainfall(mm)","Year")

# Sylhet Station(2007-2020) -> Clean data
sylhet <- rbind(sylhet1,sylhet2,sylhet3,sylhet4) 

### Total Rainfall amount for each year ###
sylhet %>%
  ggplot(mapping = aes(x = Date, y = `Rainfall(mm)`)) + 
  geom_line(color = "steelblue") +
  facet_wrap(Year ~., ncol = 3, scales = "free") +
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 day", date_labels = "%d-%b") +
  scale_y_continuous(n.breaks = 10) +
  theme(axis.text.x = element_text(angle = 0, size = 7)) +
  theme(plot.margin = unit(c(1,1,1,1), "mm")) +
  ggtitle(label = "Rainfall(mm) in Sylhet(2008-2020)") +
  ylab("Total Rainfall(mm)/Day")

sylhet$Year <- as.factor(sylhet$Year)
sylhet$DayOfYear <- as.numeric(format(sylhet$Date, "%j"))

### Total Rainfall amount for all years ###
sylhet %>%
  ggplot(mapping = aes(x = DayOfYear, y = `Rainfall(mm)`, group = Year, color = Year)) + 
  geom_line() +
  scale_x_continuous(breaks = c(1,32,61,92,122,153,183,214,245,275,306,336),
                     labels = c("01-Jan","01-Feb","01-Mar","01-Apr","01-May","01-Jun",
                                "01-Jul","01-Aug","01-Sep","01-Oct","01-Nov","01-Dec")) +
  scale_y_continuous(n.breaks = 10) +
  theme(axis.text.x = element_text(angle = 0, size = 7)) +
  theme(plot.margin = unit(c(1,1,1,1), "mm")) +
  theme_classic() +
  ggtitle(label = "Rainfall(mm) in Sylhet(2008-2020)") +
  ylab("Total Rainfall(mm)/Day") +
  xlab("Date")

################################################################################
################################## Chittagong ##################################
################################################################################

# Chittagong station(2008-2017)
chit1 <- read_excel("E:/Niyam_STAT/2008_17/Rainfall_2008-2017/Chittagong.xls")
chit1$DATE <- dmy(chit1$DATE)
chit1 <- chit1 %>% filter(Station == "Chittagong") %>% select(District,DATE,`Rainfall(mm)`)
colnames(chit1) <- c("District","Date","Rainfall(mm)")
chit1$Year <- year(chit1$Date)

# Chittagong station(2018)
chit2 <- read_excel("E:/Niyam_STAT/BMD Data/Daily Total Rainfall.xlsx")
colnames(chit2) <- chit2[6,]
chit2 <- chit2 %>%
  filter(str_detect(Stati, "Chittagong")) %>%
  pivot_longer(!c(Stati,Year,Month), names_to = "Day", values_to = "Rainfall(mm)")
chit2$DATE <- ymd(paste(chit2$Year, chit2$Month, chit2$Day, sep = "-")) 
chit2 <- chit2 %>% drop_na(DATE) %>% select(Stati,DATE,`Rainfall(mm)`,Year)
colnames(chit2) <- c("District","Date","Rainfall(mm)","Year")

# Chittagong station(2019)
chit3 <- read_excel("E:/Niyam_STAT/19.xls")
colnames(chit3) <- chit3[6,]
chit3 <- chit3 %>%
  filter(str_detect(Stati, "Chittagong")) %>%
  pivot_longer(!c(Stati,Year,Month), names_to = "Day", values_to = "Rainfall(mm)")
chit3$DATE <- ymd(paste(chit3$Year, chit3$Month, chit3$Day, sep = "-")) 
chit3 <- chit3 %>% drop_na(DATE) %>% select(Stati,DATE,`Rainfall(mm)`,Year)
colnames(chit3) <- c("District","Date","Rainfall(mm)","Year")

# Chittagong station(2020)
chit4 <- read_excel("E:/Niyam_STAT/20.xls")
chit4 <- chit4 %>%
  filter(str_detect(Stations, "Chittagong")) %>%
  pivot_longer(!c(Stations,Station_ID,Lat,Lon,Year,Month), names_to = "Day", values_to = "Rainfall(mm)") 
chit4$Day <- gsub("D_","",as.character(chit4$Day))
chit4$DATE <- ymd(paste(chit4$Year, chit4$Month, chit4$Day, sep = "-")) 
chit4 <- chit4 %>% drop_na(DATE) %>% select(Stations,DATE,`Rainfall(mm)`,Year)
chit4$`Rainfall(mm)`<- as.numeric(chit4$`Rainfall(mm)`)
chit4[is.na(chit4)] <- 0
colnames(chit4) <- c("District","Date","Rainfall(mm)","Year")

# Chittagong Station(2007-2020) -> Clean data
chittagong <- rbind(chit1,chit2,chit3,chit4) 

### Total Rainfall amount for each year ###
chittagong %>%
  ggplot(mapping = aes(x = Date, y = `Rainfall(mm)`)) + 
  geom_line(color = "steelblue") +
  facet_wrap(Year ~., ncol = 3, scales = "free") +
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 day", date_labels = "%d-%b") +
  scale_y_continuous(n.breaks = 10) +
  theme(axis.text.x = element_text(angle = 0, size = 7)) +
  theme(plot.margin = unit(c(1,1,1,1), "mm")) +
  ggtitle(label = "Rainfall(mm) in Chittagong(2008-2020)") +
  ylab("Total Rainfall(mm)/Day")

chittagong$Year <- as.factor(chittagong$Year)
chittagong$DayOfYear <- as.numeric(format(chittagong$Date, "%j"))

### Total Rainfall amount for all years ###
chittagong %>%
  ggplot(mapping = aes(x = DayOfYear, y = `Rainfall(mm)`, group = Year, color = Year)) + 
  geom_line() +
  scale_x_continuous(breaks = c(1,32,61,92,122,153,183,214,245,275,306,336),
                     labels = c("01-Jan","01-Feb","01-Mar","01-Apr","01-May","01-Jun",
                                "01-Jul","01-Aug","01-Sep","01-Oct","01-Nov","01-Dec")) +
  scale_y_continuous(n.breaks = 10) +
  theme(axis.text.x = element_text(angle = 0, size = 7)) +
  theme(plot.margin = unit(c(1,1,1,1), "mm")) +
  theme_classic() +
  ggtitle(label = "Rainfall(mm) in Chittagong(2008-2020)") +
  ylab("Total Rainfall(mm)/Day") +
  xlab("Date")

################################################################################
################################### Modeling ###################################
################################################################################
data <- rbind(dhaka,rajshahi,khulna,sylhet,chittagong)
data$Month <- month(data$Date)
data$MonthYear <- paste(data$Month, data$Year, sep = "-")
data <- data %>%
  group_by(MonthYear, District) %>%
  summarise(Rainfall = sum(`Rainfall(mm)`, na.rm = TRUE)) %>%
  ungroup() 
data$Date <- dmy(paste0("1-",data$MonthYear)) 

################################################################################
######################### TS for Rainfall in Dhaka #############################
################################################################################
data <- data %>% 
  add_row(MonthYear = "1-2009", District = "Dhaka", Rainfall = 0.0, Date = ymd("2009-01-01")) %>%
  arrange(Date, District)

# Convert rainfall data into Time series object
dhaka_ts <- ts(as.vector(data %>% filter(District == "Dhaka") %>% select(Rainfall)),
               frequency = 12,
               start = c(2008,1,1))

# Time series plot
plot(dhaka_ts, 
     col = 639,
     type = "o",
     ylab = "Total Rainfall(mm)", 
     main = "Time Series of Rainfall(mm) in Dhaka")

#### Using the ADF Test
# A time series is said to be "stationary" if it has no trend, exhibits constant
# variance over time, and has a constant autocorrelation structure over time.
# This test will check for a unit root. If there is a unit root, then the data 
# is not stationary. The ADF test is a hypothesis test with the null hypothesis 
# being there is a unit root (non-stationary) and the alternative being there is 
# not a unit root (stationary).
# One way to test whether a time series is stationary is to perform an augmented
# Dickey-Fuller test, which uses the following null and alternative hypotheses:

# H0: The time series is non-stationary. In other words, it has some time-dependent
# structure and does not have constant variance over time.
# HA: The time series is stationary.
adf.test(dhaka_ts)

#### Using the Ljung-Box test
# The Ljun-Box test is a hypothesis test that checks if a time series contains 
# an autocorrelation.

# H0: The residuals are independently distributed.
# Ha: The residuals are not independently distributed and exhibit a serial correlation.
Box.test(dhaka_ts, lag=12, type="Ljung-Box")

par(mfrow=c(1,2))
acf(dhaka_ts,lag.max = 36)
# Comment:
# ACF plots shows correlation between elements of the time series.Lags are the 
# correlation for time series observations with their previous time stamps.
# Because we have strong correlation at lags 6,12,18,24,30 and so on we consider the 
# existence of seasonal auto correlation relationship.

pacf(dhaka_ts, lag.max = 36)
# PACF is the amount of correlation between a variable and a lag of itself that
# is not explained by correlation at all lower-order-lags.

# Model - 1(Remove Seasonal Pattern)
m1_dhaka <- arima(dhaka_ts,order=c(0,0,0),seasonal=list(order=c(0,1,0), period=12), method = "ML")
res_m1 <- residuals(m1_dhaka)
par(mfrow=c(1,1))
plot(res_m1,xlab='Time',ylab='Residuals')

par(mfrow=c(1,2))
acf(res_m1, lag.max = 36)
pacf(res_m1, lag.max = 36)

# Comment:
# The significant spike at lag 1,2 in the ACF suggests a non-seasonal MA(2) component
# The significant spike at lag 11,12 in the ACF suggests a seasonal MA(2) component
# Consequently, we begin with an ARIMA(0,1,2)(0,1,2) 

# Model - 2(Non-seasonal Differencing)
m2_dhaka <- arima(dhaka_ts,order=c(0,1,2),seasonal=list(order=c(0,1,1), period=12), method = "ML")
res_m2 <- residuals(m2_dhaka)
par(mfrow=c(1,1))
plot(res_m2,xlab='Time',ylab='Residuals')
coeftest(m2_dhaka)

par(mfrow=c(1,2))
acf(res_m2, lag.max = 36)
pacf(res_m2, lag.max = 36)

# Comment: (No remaining auto-correlations)
# Both the ACF and PACF show almost significant spikes at lag 2, and almost significant
# spikes at lag 11, indicating that some additional non-seasonal terms need to be
# included in the model. 

# Stationarity test
adf.test(res_m2)
# Possible models from EACF table
eacf(res_m2)

# Model - 3
m3_dhaka <- arima(dhaka_ts,order=c(1,1,2),seasonal=list(order=c(0,1,1), period=12), method = "ML")
res_m3 <- residuals(m3_dhaka)
par(mfrow=c(1,1))
plot(res_m3,xlab='Time',ylab='Residuals')
coeftest(m3_dhaka)

par(mfrow=c(1,2))
acf(res_m3, lag.max = 36)
pacf(res_m3, lag.max = 36)

# Suggested by EACF
# Model - 4
m4_dhaka <- arima(dhaka_ts,order=c(1,1,1),seasonal=list(order=c(0,1,1), period=12), method = "ML")
res_m4 <- residuals(m4_dhaka)
par(mfrow=c(1,1))
plot(res_m4,xlab='Time',ylab='Residuals')
coeftest(m4_dhaka)

par(mfrow=c(1,2))
acf(res_m4, lag.max = 36)
pacf(res_m4, lag.max = 36)

# Model - 5(According to significant coefficients from Model 2)
m5_dhaka <- arima(dhaka_ts,order=c(1,1,1),seasonal=list(order=c(1,1,1), period=12), method = "ML")
res_m5 <- residuals(m5_dhaka)
plot(res_m5,xlab='Time',ylab='Residuals')
coeftest(m5_dhaka)

par(mfrow=c(1,2))
acf(res_m5, lag.max = 36)
pacf(res_m5, lag.max = 36)

# AIC & BIC
sort.score <- function(x, score = c("bic", "aic")){
  if (score == "aic"){
    x[with(x, order(AIC)),]
  } else if (score == "bic") {
    x[with(x, order(BIC)),]
  } else {
    warning('score = "x" only accepts valid arguments ("aic","bic")')
  }
}
sc.AIC <- AIC(m1_dhaka,m2_dhaka,m3_dhaka,m4_dhaka,m5_dhaka)
sc.BIC <- BIC(m1_dhaka,m2_dhaka,m3_dhaka,m4_dhaka,m5_dhaka)

sort.score(sc.AIC, score = "aic")
# AIC gives us model4 i.e. SARIMA(1,1,1)x(0,1,1) as best model.
sort.score(sc.BIC, score = "bic")
# BIC gives us model4 i.e. SARIMA(1,1,1)x(0,1,1) as the best model.

# Model Diagnostics(Residual Analysis)
residual.analysis <- function(model, std = TRUE){
  library(TSA)
  if (std == TRUE){
    res.model = rstandard(model)
  }else{
    res.model = residuals(model)
  }
  par(mfrow=c(2,2))
  plot(res.model,type='o',ylab='Standardized residuals', main="Time series plot of standardized residuals")
  abline(h=0)
  hist(res.model,main="Histogram of standardized residuals")
  qqnorm(res.model,main="QQ plot of standardized residuals")
  qqline(res.model, col = 2)
  acf(res.model,main="ACF of standardized residuals")
  print(shapiro.test(res.model))
}

# Comment: We would like to choose model 2 as our final model as this model will
# give us better forecast result
residual.analysis(model = m4_dhaka)

# Prediction of rainfall for the next 5 years with the SARIMA(0,1,2)x(0,1,2) model
par(mfrow=c(1,1))
sarima.for(dhaka_ts,60,1,1,1,0,1,1,12, main = "Rainfall(mm) in Dhaka")
text(2020, 600, "PAST")
text(2022, 600, "FUTURE")
abline(v=2021, lty=2, col=4)

# There is the prediction bounds (+- 1 standard error represented by the darker 
# gray bands and +-2 standard errors boundaries represented by the lighter gray bands).
################################################################################

ggtsdisplay(dhaka_ts)
BoxCox.lambda(dhaka_ts)

dhaka_ts_seasonal_diff <- diff(dhaka_ts, lag = 12, differences = 1)
ggtsdisplay(dhaka_ts_seasonal_diff)

dhaka_ts_regular_diff <- diff(dhaka_ts, lag = 1, differences = 1)
ggtsdisplay(dhaka_ts_regular_diff)

# Remove seasonal and regular derivative
dhaka_ts_regular_seasonal_diff <- diff(diff(dhaka_ts, lag = 12))
ggtsdisplay(dhaka_ts_regular_seasonal_diff)

fit1 <- Arima(dhaka_ts, order = c(0,1,0), seasonal = c(1,1,0),
              lambda = NULL, include.constant = TRUE)

autoplot(fit1)
coeftest(fit1)
checkresiduals(fit1)
ggtsdisplay(fit1$residuals)
summary(fit1)

fit2 <- Arima(dhaka_ts, order = c(0,1,0), seasonal = c(0,1,1),
              lambda = NULL, include.constant = TRUE)

autoplot(fit2)
coeftest(fit2)
checkresiduals(fit2)
ggtsdisplay(fit2$residuals)
summary(fit2)

fit3 <- Arima(dhaka_ts, order = c(0,1,1), seasonal = c(0,1,1),
              lambda = NULL, include.constant = TRUE)

autoplot(fit3)
coeftest(fit3)
checkresiduals(fit3)
ggtsdisplay(fit3$residuals)
summary(fit3)

fit4 <- Arima(dhaka_ts, order = c(1,1,1), seasonal = c(0,1,1),
              lambda = NULL, include.constant = TRUE)

autoplot(fit4)
coeftest(fit4)
checkresiduals(fit4)
ggtsdisplay(fit4$residuals)
summary(fit4)

# Trust your eyes
autoplot(dhaka_ts) + 
  autolayer(fit4$fitted, series = "SARIMA(1,1,1)(0,1,1)") +
  theme_bw() +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  labs(title = "SARIMA (1,1,1) (0,1,1) rainfall prediction for Dhaka", y = "Rainfall(mm)") 

# 
auto.arima(dhaka_ts, trace = TRUE)

# Final Model
fit5 <- Arima(dhaka_ts, order = c(0,0,2), seasonal = c(1,1,0),
              lambda = NULL, include.constant = TRUE)

autoplot(fit5)
coeftest(fit5)
checkresiduals(fit5)
ggtsdisplay(fit5$residuals)
summary(fit5)

autoplot(dhaka_ts) + 
  autolayer(fit5$fitted, series = "Fit")


################################################################################
######################## TS for Rainfall in Rajshahi ###########################
################################################################################
# Convert rainfall data into Time series object
rajshahi_ts <- ts(as.vector(data %>% filter(District == "Rajshahi") %>% select(Rainfall)),
                  frequency = 12,
                  start = c(2008,1,1))
# Time series plot
par(mfrow=c(1,1))
plot(rajshahi_ts,
     col = 639,
     type = "o",
     ylab = "Total Rainfall(mm)",
     main = "Time series of Rainfall(mm) in Rajshahi")
# Augmented Dickey-Fuller test
adf.test(rajshahi_ts)
# Ljung-Box test
Box.test(rajshahi_ts, lag=12, type="Ljung-Box")

# ACF and PACF plot
par(mfrow=c(1,2))
acf(rajshahi_ts,lag.max = 36)
pacf(rajshahi_ts, lag.max = 36)

# Model - 1(Remove Seasonal Pattern)
m1_rajshahi <- arima(rajshahi_ts,order=c(0,0,0),seasonal=list(order=c(0,1,0), period=12), method = "ML")
res_m1 <- residuals(m1_rajshahi)
par(mfrow=c(1,1))
plot(res_m1,xlab='Time',ylab='Residuals')

par(mfrow=c(1,2))
acf(res_m1, lag.max = 36)
pacf(res_m1, lag.max = 36)

# Comment:
# The significant spike at lag 9,12 in the ACF suggests a seasonal MA(2) or MA(1) component
# Consequently, we begin with an ARIMA(0,1,0)(0,1,2) 

# Model - 2(Non-seasonal Differencing)
m2_rajshahi <- arima(rajshahi_ts,order=c(0,1,0),seasonal=list(order=c(0,1,1), period=12), method = "ML")
res_m2 <- residuals(m2_rajshahi)
par(mfrow=c(1,1))
plot(res_m2,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
coeftest(m2_rajshahi)

par(mfrow=c(1,2))
acf(res_m2, lag.max = 36)
pacf(res_m2, lag.max = 36)

# Model - 3
m3_rajshahi <- arima(rajshahi_ts,order=c(0,1,1),seasonal=list(order=c(0,1,1), period=12), method = "ML")
res_m3 <- residuals(m3_rajshahi)
par(mfrow=c(1,1))
plot(res_m3,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
coeftest(m3_rajshahi)

par(mfrow=c(1,2))
acf(res_m3, lag.max = 36)
pacf(res_m3, lag.max = 36)

# Model 3 has all the significant orders
adf.test(res_m3)
eacf(res_m3)

# Model - 4
m4_rajshahi <- arima(rajshahi_ts,order=c(0,1,1),seasonal=list(order=c(0,1,2), period=12), method = "ML")
res_m4 <- residuals(m4_rajshahi)
par(mfrow=c(1,1))
plot(res_m4,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
coeftest(m4_rajshahi)

par(mfrow=c(1,2))
acf(res_m4, lag.max = 36)
pacf(res_m4, lag.max = 36)

# Model - 5
m5_rajshahi <- arima(rajshahi_ts,order=c(0,1,2),seasonal=list(order=c(0,1,2), period=12), method = "ML")
res_m5 <- residuals(m5_rajshahi)
par(mfrow=c(1,1))
plot(res_m5,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
coeftest(m5_rajshahi)

par(mfrow=c(1,2))
acf(res_m5, lag.max = 36)
pacf(res_m5, lag.max = 36)

# AIC & BIC
sc.AIC <- AIC(m1_rajshahi,m2_rajshahi,m3_rajshahi,m4_rajshahi,m5_rajshahi)
sc.BIC <- BIC(m1_rajshahi,m2_rajshahi,m3_rajshahi,m4_rajshahi,m5_rajshahi)

sort.score(sc.AIC, score = "aic")
# AIC gives us model4 i.e. SARIMA(0,1,1)x(0,1,2) as best model.
sort.score(sc.BIC, score = "bic")
# BIC gives us model3 i.e. SARIMA(0,1,1)x(0,1,1) as the best model.
# Comment: We fail to choose our final model from AIC and BIC

# Model Diagnostics(Residual Analysis)
residual.analysis(model = m4_rajshahi)
residual.analysis(model = m3_rajshahi) # Better ACF of residuals

# Prediction of rainfall for the next 5 years with the SARIMA(0,1,1)x(0,1,1) model
par(mfrow=c(1,1))
sarima.for(rajshahi_ts,60,0,1,1,0,1,1,12, main = "Rainfall(mm) in Rajshahi")
text(2020, 450, "PAST")
text(2022, 450, "FUTURE")
abline(v=2021, lty=2, col=4)

################################################################################
# Trust your eyes
fit4 <- Arima(rajshahi_ts, order = c(0,1,1), seasonal = c(0,1,1),
              lambda = NULL, include.constant = TRUE)
autoplot(rajshahi_ts) + 
  autolayer(fit4$fitted, series = "SARIMA(0,1,1)(0,1,1)") +
  theme_bw() +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  labs(title = "SARIMA (0,1,1) (0,1,1) rainfall prediction for Rajshahi", y = "Rainfall(mm)") 
  

# Simulation to get the best model using computer's brain
auto.arima(rajshahi_ts, trace = TRUE)

# Fit best model suggested by auto.arima
fit5 <- Arima(rajshahi_ts, order = c(0,0,0), seasonal = c(0,1,1),
              lambda = NULL, include.constant = TRUE)

autoplot(fit5)
coeftest(fit5)
checkresiduals(fit5)
ggtsdisplay(fit5$residuals)
summary(fit5)

autoplot(rajshahi_ts) + 
  autolayer(fit5$fitted, series = "Fit")

################################################################################
########################## TS for Rainfall in Sylhet ###########################
################################################################################
# Impute unknown values for the 7th and 8th months of 2017
data <- data %>% 
  add_row(MonthYear = c("7-2017","8-2017"), District = c("Sylhet","Sylhet"),
          Rainfall = c(637.875,628.4583), Date = c(ymd("2017-07-01"),ymd("2017-08-01"))) %>%
  arrange(Date, District) 

# Convert rainfall data into Time series object
sylhet_ts <- ts(as.vector(data %>% filter(District == "Sylhet") %>% select(Rainfall)),
                frequency = 12,
                start = c(2008,1,1))
# Time series plot
par(mfrow=c(1,1))
plot(sylhet_ts,
     col = 639, 
     type = "o",
     ylab = "Total Rainfall(mm)",
     main = "Time series of Rainfall(mm) in Sylhet")
# Augmented Dickey-Fuller test/ Unit root test
adf.test(sylhet_ts)
# Ljung-Box test
Box.test(sylhet_ts, lag=12, type="Ljung-Box")

par(mfrow=c(1,2))
acf(sylhet_ts,lag.max = 36)
pacf(sylhet_ts, lag.max = 36)

# Model - 1(Remove Seasonal Pattern)
m1_sylhet <- arima(sylhet_ts,order=c(0,0,0),seasonal=list(order=c(0,1,0), period=12), method = "ML")
res_m1 <- residuals(m1_sylhet)
par(mfrow=c(1,1))
plot(res_m1,xlab='Time',ylab='Residuals')

par(mfrow=c(1,2))
acf(res_m1, lag.max = 36)
pacf(res_m1, lag.max = 36)

# Model - 2(Non-seasonal Differencing)
m2_sylhet <- arima(sylhet_ts,order=c(0,1,1),seasonal=list(order=c(0,1,1), period=12), method = "ML")
res_m2 <- residuals(m2_sylhet)
par(mfrow=c(1,1))
plot(res_m2,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
coeftest(m2_sylhet)

par(mfrow=c(1,2))
acf(res_m2, lag.max = 36)
pacf(res_m2, lag.max = 36)

adf.test(res_m2)
# Possible models from EACF table
eacf(res_m2)

# Model - 3
m3_sylhet <- arima(sylhet_ts,order=c(0,1,2),seasonal=list(order=c(0,1,1), period=12), method = "ML")
res_m3 <- residuals(m3_sylhet)
par(mfrow=c(1,1))
plot(res_m3,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
coeftest(m3_sylhet)

par(mfrow=c(1,2))
acf(res_m3, lag.max = 36)
pacf(res_m3, lag.max = 36)

# Model - 4
m4_sylhet <- arima(sylhet_ts,order=c(0,1,1),seasonal=list(order=c(0,1,2), period=12), method = "ML")
res_m4 <- residuals(m4_sylhet)
par(mfrow=c(1,1))
plot(res_m4,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
coeftest(m4_sylhet)

par(mfrow=c(1,2))
acf(res_m4, lag.max = 36)
pacf(res_m4, lag.max = 36)

# Model - 5
m5_sylhet <- arima(sylhet_ts,order=c(1,1,1),seasonal=list(order=c(0,1,1), period=12), method = "ML")
res_m5 <- residuals(m5_sylhet)
par(mfrow=c(1,1))
plot(res_m5,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
coeftest(m5_sylhet)

par(mfrow=c(1,2))
acf(res_m5, lag.max = 36)
pacf(res_m5, lag.max = 36)

# AIC & BIC
sc.AIC <- AIC(m1_sylhet,m2_sylhet,m3_sylhet,m4_sylhet,m5_sylhet)
sc.BIC <- BIC(m1_sylhet,m2_sylhet,m3_sylhet,m4_sylhet,m5_sylhet)

sort.score(sc.AIC, score = "aic")
# AIC gives us model2 i.e. SARIMA(0,1,1)x(0,1,1) as best model.
sort.score(sc.BIC, score = "bic")
# BIC gives us model2 i.e. SARIMA(0,1,1)x(0,1,1) as the best model.
# Comment: We would like to choose model 2 as our final model as this model will
# give us better forecast result

# Model Diagnostics(Residual Analysis)
residual.analysis(model = m2_sylhet) # According to AIC and BIC

# Prediction of rainfall for the next 5 years with the SARIMA(0,1,1)x(0,1,1) model
par(mfrow=c(1,1))
sarima.for(sylhet_ts,60,0,1,1,0,1,1,12, main = "Rainfall(mm) in Sylhet")
text(2020, 1000, "PAST")
text(2022, 1000, "FUTURE")
abline(v=2021, lty=2, col=4)

################################################################################
# Trust your eyes
fit4 <- Arima(sylhet_ts, order = c(0,1,1), seasonal = c(0,1,1),
              lambda = NULL, include.constant = TRUE)
autoplot(sylhet_ts) + 
  autolayer(fit4$fitted, series = "SARIMA(0,1,1)(0,1,1)") +
  theme_bw() +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  labs(title = "SARIMA (0,1,1) (0,1,1) rainfall prediction for Sylhet", y = "Rainfall(mm)") 


# Simulation to get the best model using computer's brain
auto.arima(sylhet_ts, trace = TRUE)

# Fit best model suggested by auto.arima
fit5 <- Arima(sylhet_ts, order = c(0,0,0), seasonal = c(1,1,0),
              lambda = NULL, include.constant = TRUE)

autoplot(fit5)
coeftest(fit5)
checkresiduals(fit5)
ggtsdisplay(fit5$residuals)
summary(fit5)

autoplot(sylhet_ts) + 
  autolayer(fit5$fitted, series = "Fit")

################################################################################
####################### TS for Rainfall in Chittagong ##########################
################################################################################
# Impute unknown values for the 7th and 8th no. months in 2017
data <- data %>% 
  add_row(MonthYear = c("10-2015","7-2017","6-2020"), District = c("Chittagong","Chittagong","Chittagong"),
          Rainfall = c(229.5,747.7083,689.625), Date = c(ymd("2015-10-01"),ymd("2017-07-01"),ymd("2020-06-01"))) %>%
  arrange(Date, District)

# Convert rainfall data into Time series object
chit_ts <- ts(as.vector(data %>% filter(District == "Chittagong") %>% select(Rainfall)),
              frequency = 12,
              start = c(2008,1,1))
# Time series plot
par(mfrow=c(1,1))
plot(chit_ts, 
     col = 639, 
     type = "o",
     ylab = "Total Rainfall(mm)",
     main = "Time series of Rainfall(mm) in Chittagong")
# Augmented Dickey-Fuller test/ Unit root test
adf.test(chit_ts)
# Ljung-Box test
Box.test(chit_ts, lag=12, type="Ljung-Box")

# ACF and PACF plot
par(mfrow=c(1,2))
acf(chit_ts,lag.max = 36)
pacf(chit_ts, lag.max = 36)

# Model - 1(Remove Seasonal Pattern)
m1_chit <- arima(chit_ts,order=c(0,0,0),seasonal=list(order=c(0,1,0), period=12), method = "ML")
res_m1 <- residuals(m1_chit)
par(mfrow=c(1,1))
plot(res_m1,xlab='Time',ylab='Residuals')

par(mfrow=c(1,2))
acf(res_m1, lag.max = 36)
pacf(res_m1, lag.max = 36)

# Model - 2(Non-seasonal Differencing)
m2_chit <- arima(chit_ts,order=c(0,1,1),seasonal=list(order=c(0,1,1), period=12), method = "ML")
res_m2 <- residuals(m2_chit)
par(mfrow=c(1,1))
plot(res_m2,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
coeftest(m2_chit)

par(mfrow=c(1,2))
acf(res_m2, lag.max = 36)
pacf(res_m2, lag.max = 36)

adf.test(res_m2)
eacf(res_m2)

# Model - 3
m3_chit <- arima(chit_ts,order=c(0,1,1),seasonal=list(order=c(0,1,2), period=12), method = "ML")
res_m3 <- residuals(m3_chit)
par(mfrow=c(1,1))
plot(res_m3,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
coeftest(m3_chit)

par(mfrow=c(1,2))
acf(res_m3, lag.max = 36)
pacf(res_m3, lag.max = 36)

# Model - 4(According to EACF table)
m4_chit <- arima(chit_ts,order=c(0,1,2),seasonal=list(order=c(0,1,2), period=12), method = "ML")
res_m4 <- residuals(m4_chit)
par(mfrow=c(1,1))
plot(res_m4,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
coeftest(m4_chit)

par(mfrow=c(1,2))
acf(res_m4, lag.max = 36)
pacf(res_m4, lag.max = 36)

# Model - 5
m5_chit <- arima(chit_ts,order=c(0,1,2),seasonal=list(order=c(0,1,1), period=12), method = "ML")
res_m5 <- residuals(m5_chit)
par(mfrow=c(1,1))
plot(res_m5,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
coeftest(m5_chit)

par(mfrow=c(1,2))
acf(res_m5, lag.max = 36)
pacf(res_m5, lag.max = 36)

# AIC & BIC
sc.AIC <- AIC(m1_chit,m2_chit,m3_chit,m4_chit,m5_chit)
sc.BIC <- BIC(m1_chit,m2_chit,m3_chit,m4_chit,m5_chit)

sort.score(sc.AIC, score = "aic")
# AIC gives us model3 i.e. SARIMA(0,1,1)x(0,1,2) as best model.
sort.score(sc.BIC, score = "bic")
# BIC gives us model2 i.e. SARIMA(0,1,1)x(0,1,1) as the best model.

# Model Diagnostics(Residual Analysis)
# Comment: We would like to choose model 3 as our final model as this model will
# give us better forecast result. Even though it's parameters are not significant
# at 5% level of significance but we can choose this model at 10% level of significance
residual.analysis(model = m3_chit) # According to AIC
residual.analysis(model = m2_chit) # According to BIC

# Prediction of rainfall for the next 5 years with the SARIMA(0,1,1)x(0,1,1) model
par(mfrow=c(1,1))
sarima.for(chit_ts,60,0,1,1,0,1,2,12, main = "Rainfall(mm) in Chittagong")
text(2020, 1500, "PAST")
text(2022, 1500, "FUTURE")
abline(v=2021, lty=2, col=4)

################################################################################
# Trust your eyes
fit4 <- Arima(chit_ts, order = c(0,1,1), seasonal = c(0,1,2),
              lambda = NULL, include.constant = TRUE)
autoplot(chit_ts) + 
  autolayer(fit4$fitted, series = "SARIMA(0,1,1)(0,1,2)") +
  theme_bw() +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  labs(title = "SARIMA (0,1,1) (0,1,2) rainfall prediction for Chittagong", y = "Rainfall(mm)") 


# Simulation to get the best model using computer's brain
auto.arima(chit_ts, trace = TRUE)

# Fit best model suggested by auto.arima
fit5 <- Arima(chit_ts, order = c(1,0,0), seasonal = c(1,1,0),
              lambda = NULL, include.constant = TRUE)

autoplot(fit5)
coeftest(fit5)
checkresiduals(fit5)
ggtsdisplay(fit5$residuals)
summary(fit5)

autoplot(chit_ts) + 
  autolayer(fit5$fitted, series = "Fit")


################################################################################
######################### TS for Rainfall in Khulna ############################
################################################################################
data <- data %>% 
  add_row(MonthYear = "8-2011", District = "Khulna", Rainfall = 412.4167, Date = ymd("2011-08-01")) %>%
  arrange(Date, District)

# Convert rainfall data into Time series object
khulna_ts <- ts(as.vector(data %>% filter(District == "Khulna") %>% select(Rainfall)),
                frequency = 12,
                start = c(2008,1,1))
# Time series plot
par(mfrow=c(1,1))
plot(khulna_ts, 
     col = 639 ,
     type = "o",
     ylab = "Total Rainfall(mm)",
     main = "Time series of Rainfall(mm) in Khulna")
# # Augmented Dickey-Fuller test/ Unit root test
adf.test(khulna_ts)
# Ljung-Box test
Box.test(khulna_ts, lag=12, type="Ljung-Box")

# ACF and PACF plot
par(mfrow=c(1,2))
acf(khulna_ts,lag.max = 36)
pacf(khulna_ts, lag.max = 36)

# Model - 1(Remove Seasonal Pattern)
m1_khulna <- arima(khulna_ts,order=c(0,0,0),seasonal=list(order=c(0,1,0), period=12), method = "ML")
res_m1 <- residuals(m1_khulna)
par(mfrow=c(1,1))
plot(res_m1,xlab='Time',ylab='Residuals')

par(mfrow=c(1,2))
acf(res_m1, lag.max = 36)
pacf(res_m1, lag.max = 36)

# Model - 2(Non-seasonal Differencing)
m2_khulna <- arima(khulna_ts,order=c(0,0,1),seasonal=list(order=c(0,1,1), period=12), method = "ML")
res_m2 <- residuals(m2_khulna)
par(mfrow=c(1,1))
plot(res_m2,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
coeftest(m2_khulna)

par(mfrow=c(1,2))
acf(res_m2, lag.max = 36)
pacf(res_m2, lag.max = 36)

adf.test(res_m2)
eacf(res_m2)

# Model - 3
m3_khulna <- arima(khulna_ts,order=c(0,1,1),seasonal=list(order=c(0,1,1), period=12), method = "ML")
res_m3 <- residuals(m3_khulna)
par(mfrow=c(1,1))
plot(res_m3,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
coeftest(m3_khulna)

par(mfrow=c(1,2))
acf(res_m3, lag.max = 36)
pacf(res_m3, lag.max = 36)

# Model - 4
m4_khulna <- arima(khulna_ts,order=c(0,1,2),seasonal=list(order=c(0,1,1), period=12), method = "ML")
res_m4 <- residuals(m4_khulna)
par(mfrow=c(1,1))
plot(res_m4,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
coeftest(m4_khulna)

par(mfrow=c(1,2))
acf(res_m4, lag.max = 36)
pacf(res_m4, lag.max = 36)

# Model - 5
m5_khulna <- arima(khulna_ts,order=c(0,0,13),seasonal=list(order=c(0,1,1), period=12), method = "ML")
res_m5 <- residuals(m5_khulna)
par(mfrow=c(1,1))
plot(res_m5,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
coeftest(m5_khulna)

par(mfrow=c(1,2))
acf(res_m5, lag.max = 36)
pacf(res_m5, lag.max = 36)

# AIC & BIC
sc.AIC <- AIC(m1_khulna,m2_khulna,m3_khulna,m4_khulna,m5_khulna)
sc.BIC <- BIC(m1_khulna,m2_khulna,m3_khulna,m4_khulna,m5_khulna)

sort.score(sc.AIC, score = "aic")
# AIC gives us model3 i.e. SARIMA(0,1,2)x(0,1,1) as best model.
sort.score(sc.BIC, score = "bic")
# BIC gives us model3 i.e. SARIMA(0,1,2)x(0,1,1) as the best model.

# Model Diagnostics(Residual Analysis)
# Comment: We would like to choose model 4 as our final model as this model will
# give us better forecast result
residual.analysis(model = m4_khulna)

# Prediction of rainfall for the next 5 years with the SARIMA(2,1,0)x(0,1,1) model
par(mfrow=c(1,1))
sarima.for(khulna_ts,60,0,1,2,0,1,1,12, main = "Rainfall(mm) in Khulna")
text(2020, 1000, "PAST")
text(2022, 1000, "FUTURE")
abline(v=2021, lty=2, col=4)

################################################################################
# Trust your eyes
fit4 <- Arima(khulna_ts, order = c(0,1,2), seasonal = c(0,1,1),
              lambda = NULL, include.constant = TRUE)
autoplot(khulna_ts) + 
  autolayer(fit4$fitted, series = "SARIMA(0,1,2)(0,1,1)") +
  theme_bw() +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  labs(title = "SARIMA (0,1,2) (0,1,1) rainfall prediction for Khulna", y = "Rainfall(mm)") 


# Simulation to get the best model using computer's brain
auto.arima(khulna_ts, trace = TRUE)

# Fit best model suggested by auto.arima
fit5 <- Arima(chit_ts, order = c(1,0,0), seasonal = c(1,0,2),
              lambda = NULL, include.constant = TRUE)

autoplot(fit5)
coeftest(fit5) 
checkresiduals(fit5)
ggtsdisplay(fit5$residuals)
summary(fit5)

autoplot(chit_ts) + 
  autolayer(fit5$fitted, series = "Fit")


ggseasonplot(dhaka_ts)
