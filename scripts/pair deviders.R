pairs.ls <- list(vars(Administration_EmpPT, Administration_EmpTotal),
                vars(Agriculture_EmpPT, Agriculture_EmpTotal),
                vars(ArtsRec_EmpPT, ArtsRec_EmpTotal),
                vars(Construction_EmpPT, Construction_EmpTotal),
                vars(Education_EmpPT, Education_EmpTotal),

                vars(Financial_EmpPT, Financial_EmpTotal),
                vars(HealthSocial_EmpPT, HealthSocial_EmpTotal),
                vars(Hospitality_EmpPT, Hospitality_EmpTotal),

                vars(Manufacturing_EmpPT, Manufacturing_EmpTotal),
                vars(Media_EmpPT, Media_EmpTotal),
                vars(Mining_EmpPT, Mining_EmpTotal),
                vars(OtherServices_EmpPT, OtherServices_EmpTotal),
                vars(ProfScientificTech_EmpPT, ProfScientificTech_EmpTotal),
                vars(RentalRealEstate_EmpPT, RentalRealEstate_EmpTotal),

                vars(Retail_EmpPT, Retail_EmpTotal),
                vars(Transport_EmpPT, Transport_EmpTotal),
                vars(Utilities_EmpPT, Utilities_EmpTotal),
                vars(Wholesale_EmpPT, Wholesale_EmpTotal))



divide_pair <- function(df, list.input) {
  df <- df %>%ungroup()
  temp_df <- list.input %>%
    map( ~ do_opertation(df, .x)) %>%
    bind_cols()

  df %>%
    select(bin_id) %>%
    bind_cols(temp_df)%>%
    gather(Category, Value, 2:length(.))

}

do_opertation <- function(df, var.pair) {
  var1 <- var.pair[[1]]
  var2 <- var.pair[[2]]

  new.name.prefix <- quo_name(var1) %>% stringr::str_replace("_EmpPT", "")

  new.name <- glue::glue("{new.name.prefix}_Ratio")

  message(glue::glue("New variable name will be: {new.name}"))

  df %>%
    select(!!!var.pair) %>%
    mutate(temp.var = UQ(var1)/ UQ(var2)) %>%
    select(temp.var) %>%
    mutate(!!new.name := temp.var) %>%
    select(-temp.var)

}





x<-divide_pair(industry_S,pairs.ls)















