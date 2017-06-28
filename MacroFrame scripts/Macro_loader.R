
#' @name macro_loader
#' @param  Blank will bring in csv's from
#' @return A data frame
#' @export


macro_loader<- function(){
  library(lubridate)
  library(timemerge)
  df_list<-csv_prep()



test_fun <- function(df) {
  df %>%
    mutate(Date = parse_date_time2(as.character(Date), orders = "%Y/%m/%d")) %>%
    group_by(Date) %>%
    gather(Cats, Vals, 2:length(.)) %>%
    binner(Date, Cats, Vals, "quarter", method = "avg") %>%
    select(-Count)
}

out_df_list <- list()
j <- 1
for (n in df_list) {
  out_df_list[[j]] <- test_fun(n)
  j <- j + 1
}
out1 <- out_df_list[[1]]
all_done <<- out_df_list %>%
  bind_rows()
final<<-spread(all_done, Cats, Total)
}




 # quarterly_macro  <- read.csv("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Time series csv's/quarterly macro data.csv", header = TRUE, sep =",")
 # Employment_ceasing <- read.csv("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Time series csv's/Reasons Ceasing Employment.csv", header = TRUE, sep =",")
 # Employment_Industy <- read.csv("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Time series csv's/EmploymentByIndustry.csv", header = TRUE, sep =",")
 # LabourForce <- read.csv("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Time series csv's/LabourForceAggregates.csv", header = TRUE, sep =",")
 # population1 <- read.csv("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Time series csv's/PopulationStats.csv", header = TRUE, sep =",")
 # weekly_earnings <- read.csv("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Time series csv's/Average Weekly Earnings.csv", header = TRUE, sep =",")
 # RandD <- read.csv("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Time series csv's/Research and Development.csv", header = TRUE, sep =",")
 # Underemployment <- read.csv("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Time series csv's/UnderemploymentByIndustry.csv", header = TRUE, sep =",")
 # WPI_Industry <- read.csv("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Time series csv's/WPI by industry.csv", header = TRUE, sep =",")
 # Foreign_Investment <- read.csv("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Time series csv's/ForeignInvestmentData.csv", header = TRUE, sep =",")
 # Bonds <- read.csv("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Time series csv's/Bonds.csv", header = TRUE, sep =",")
 # House_Index <- read.csv("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Time series csv's/House Index.csv", header = TRUE, sep =",")
 # LabourMob_Ind <- read.csv("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Time series csv's/Labour Mobility By Industry.csv", header = TRUE, sep =",")
 # LabourMob <- read.csv("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Time series csv's/LabourMobility.csv", header = TRUE, sep =",")










 #
 # quarterly_macro$Date <- as.Date(quarterly_macro$Date, format = "%d/%m/%Y" )
 # Employment_ceasing$Date <- as.Date(Employment_ceasing$Date, format = "%d/%m/%Y" )
 # Employment_Industy$Date <- as.Date(Employment_Industy$Date, format = "%d/%m/%Y" )
 # LabourForce$Date <- as.Date(LabourForce$Date, format = "%d/%m/%Y" )
 # population1$Date <- as.Date(population1$Date, format = "%d/%m/%Y" )
 # weekly_earnings$Date <- as.Date(weekly_earnings$Date, format = "%d/%m/%Y" )
 # RandD$Date <- as.Date(RandD$Date, format = "%d/%m/%Y" )
 # Underemployment$Date <- as.Date(Underemployment$Date, format = "%d/%m/%Y" )
 # WPI_Industry$Date <- as.Date(WPI_Industry$Date, format = "%d/%m/%Y" )
 # Foreign_Investment$Date <- as.Date(Foreign_Investment$Date, format = "%d/%m/%Y")
 # Bonds$Date <- as.Date(Bonds$Date, format = "%d/%m/%Y")
 # House_Index$Date <- as.Date(House_Index$Date, format = "%d/%m/%Y")
 # LabourMob_Ind$Date<- as.Date(LabourMob_Ind$Date, format = "%d/%m/%Y")
 # LabourMob$Date <- as.Date(LabourMob$Date, format = "%d/%m/%Y")
 #
 # quarterly_macro <<- quarterly_macro
 # Employment_ceasing <<- Employment_ceasing
 # Employment_Industy <<- Employment_Industy
 # LabourForce <<- LabourForce
 # population1 <<- population1
 # weekly_earnings <<- weekly_earnings
 # RandD <<- RandD
 # Underemployment <<- Underemployment
 # WPI_Industry <<- WPI_Industry
 # Foreign_Investment <<- Foreign_Investment
 # Bonds <<- Bonds
 # House_Index <<- House_Index
 # LabourMob_Ind <<- LabourMob_Ind
 # LabourMob <<- LabourMob
 #
 # df_list <- list(quarterly_macro,Employment_ceasing, Employment_Industy,LabourForce,population1,weekly_earnings,RandD,Underemployment,WPI_Industry,Foreign_Investment,Bonds,House_Index,LabourMob_Ind,LabourMob)








#
# final<-mutate( final,riskfree = Aus10yr-inflation.rate, na.rm= F)
#
# final$riskfree
#
# class(final$inflation.rate)
# class(final$Aus10yr)
#
#
#
# plot(final$inflation.rate,final$Aus10yr)
#
# ggplot(final, aes(x = inflation.rate&Aus10yr, y = Total, colour = inflation.rate,Aus10yr)) + geom_line()
