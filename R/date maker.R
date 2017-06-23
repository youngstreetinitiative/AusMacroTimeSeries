

#' @export


date_maker_loop <- function(df) {

  df %>%
    mutate(Date = as.Date(Date, format = "%d/%m/%Y"))

}



#' @export


date_maker_list <- function(df_list) {

  df_list <- df_list %>%
    map(~mutate(.,
                Date = as.Date(Date, format = "%d/%m/%Y")))

  lapply(names(df_list), function(x) {assign(x, df_list[[x]],.GlobalEnv)})


  df_list
}

