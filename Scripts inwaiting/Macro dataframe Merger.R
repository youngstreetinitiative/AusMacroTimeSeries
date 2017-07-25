
df_list <- list(quarterly_macro,Employment_ceasing, Employment_Industy,LabourForce,population1,weekly_earnings,RandD,Underemployment,WPI_Industry,Foreign_Investment)

test_fun <- function(df) {
  df %>%
    mutate(Date = parse_date_time2(as.character(Date), orders = "%Y/%m/%d")) %>%
    group_by(Date) %>%
    gather(Cats, Vals, 2:length(.)) %>%
    binner( Date, Cats, Vals, "quarter")
}

out_df_list <- list()
j <- 1
for (n in df_list) {
  out_df_list[[j]] <- test_fun(n)
  j <- j + 1
}
out1 <- out_df_list[[1]]
all_done <- out_df_list %>%
  bind_rows()
final2<-spread(all_done, Cats, Total)

ggplot(all_done, aes(x = bin_id, y = Total, colour = Cats)) + geom_line() +guides(colour=FALSE)+ ylim(0,1)


ggplot(data = final ) +
  ylab('percent') +
  geom_line(aes(x=bin_id, y=inflation.rate, col = "inflation"),  size=1, alpha=.5) +
  geom_line(aes(x=bin_id, y=Aus10yr , col = "Bondyeild"),  size=1, alpha=.5) +
  geom_hline(yintercept = 0) +
  theme(legend.position=c(.1,.85))

final$inflation.rate <- final$inflation.rate*100
)

final$

# new_df <- Employment_ceasing  %>%
#   mutate(Date = parse_date_time2(as.character(Date), orders = "%Y/%m/%d")) %>%
#   group_by(Date) %>%
#   gather(Cats, Vals, EmpSomeTime:CeasedEmp)
# newer_df <- binner(new_df, Date, Cats, Vals, "quarter")
#
# new_df2 <- Employment_Industy  %>%
#   mutate(Date = parse_date_time2(as.character(Date), orders = "%Y/%m/%d")) %>%
#   group_by(Date) %>%
#   gather(Cats, Vals, Agriculture_EmpTotal:Total_EmpHrAvg)
# newer_df2 <- binner(new_df2, Date, Cats, Vals, "quarter")
#
# new_df3 <- Foreign_Investment  %>%
#   mutate(Date = parse_date_time2(as.character(Date), orders = "%Y/%m/%d")) %>%
#   group_by(Date) %>%
#   gather(Cats, Vals, Num_Approved_RealEstate:Value_Decided)
# newer_df3 <- binner(new_df3, Date, Cats, Vals, "quarter")
#
# new_df4 <- LabourForce  %>%
#   mutate(Date = parse_date_time2(as.character(Date), orders = "%Y/%m/%d")) %>%
#   group_by(Date) %>%
#   gather(Cats, Vals, Employed_Trend:UnderutilisationRate_Orig)
# newer_df4 <- binner(new_df4, Date, Cats, Vals, "quarter")
#
# new_df5 <- population  %>%
#   mutate(Date = parse_date_time2(as.character(Date), orders = "%Y/%m/%d")) %>%
#   group_by(Date) %>%
#   gather(Cats, Vals, InterstateMigrationNSW:PopulationAusPercChange)
# newer_df5 <- binner(new_df5, Date, Cats, Vals, "quarter")
#
# new_df6 <- quarterly_macro  %>%
#   mutate(Date = parse_date_time2(as.character(Date), orders = "%Y/%m/%d")) %>%
#   group_by(Date) %>%
#   gather(Cats, Vals, terms.of.trade:Median.Price.of.Established.House.Transfers..Unstratified.....Canberra..)
#
# newer_df6 <- binner(new_df6, Date, Cats, Vals, "quarter")
#
# new_df7 <- RandD  %>%
#   mutate(Date = parse_date_time2(as.character(Date), orders = "%Y/%m/%d")) %>%
#   group_by(Date) %>%
#   gather(Cats, Vals, Emp0_19:ExpRD_Total)
# newer_df7 <- binner(new_df7, Date, Cats, Vals, "quarter")
#
# new_df8 <- Underemployment  %>%
#   mutate(Date = parse_date_time2(as.character(Date), orders = "%Y/%m/%d")) %>%
#   group_by(Date) %>%
#   gather(Cats, Vals, Agriculture_UnderEmp:Overall_UnderEmp)
# newer_df8 <- binner(new_df8, Date, Cats, Vals, "quarter")
#
# new_df9 <- weekly_earnings  %>%
#   mutate(Date = parse_date_time2(as.character(Date), orders = "%Y/%m/%d")) %>%
#   group_by(Date) %>%
#   gather(Cats, Vals, Mining:Total)
# newer_df9 <- binner(new_df9, Date, Cats, Vals, "quarter")
#
# new_df10 <- WPI_Industry  %>%
#   mutate(Date = parse_date_time2(as.character(Date), orders = "%Y/%m/%d")) %>%
#   group_by(Date) %>%
#   gather(Cats, Vals, Mining_WPIHourly:Total_WPIHourly)
# newer_df10 <- binner(new_df10, Date, Cats, Vals, "quarter")
#
#
#
# merged_set <- bind_rows(newer_df,newer_df2,newer_df3,newer_df4,newer_df5,newer_df6,newer_df7,newer_df8,newer_df9,newer_df10)
# final<-spread(merged_set, Cats, Total)

