rm(list = ls())
setwd("C:/Users/lxue5/Dropbox/2019Sp/CIS8695/2 Trees")
delays.df <- read.csv("CSV_FlightDelays.csv")

# Creating dummies
delays.df$Weekend <- delays.df$DAY_WEEK %in% c(6, 7)
delays.df$CARRIER_CO_MQ_DH_RU <- delays.df$CARRIER %in% c("CO", "MQ", "DH", "RU")
delays.df$MORNING <- delays.df$CRS_DEP_TIME.1 %in% c(6, 7, 8, 9)
delays.df$NOON <- delays.df$CRS_DEP_TIME.1 %in% c(10, 11, 12, 13)
delays.df$AFTER2P <- delays.df$CRS_DEP_TIME.1 %in% c(14, 15, 16, 17, 18)
delays.df$EVENING <- delays.df$CRS_DEP_TIME.1 %in% c(19, 20)


