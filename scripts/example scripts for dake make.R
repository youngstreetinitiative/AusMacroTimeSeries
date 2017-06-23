
library(tidyverse)

LabourMob_Ind <- read.csv("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Time series csv's/Labour Mobility By Industry.csv", header = TRUE, sep = ",")
LabourMob <- read.csv("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Time series csv's/LabourMobility.csv", header = TRUE, sep = ",")

# a named list fo dfs
df_list <- list(LabourMob_Ind = LabourMob_Ind,LabourMob = LabourMob)

names(df_list)




# date maker example 1
for (n in seq_along(df_list)) {

  temp_df <- date_maker_loop(df_list[[n]])

  message(names(df_list[n]))
  assign(names(df_list[n]), temp_df, envir = .GlobalEnv)

}

# date maker example 2


df_list <- date_maker_list(df_list)

