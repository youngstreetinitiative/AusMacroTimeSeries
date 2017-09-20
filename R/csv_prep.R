#' @export

csv_prep <- function() {


  paths_vec <- list.files("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Macroeconomic Analysis/Data/Time series csv's/",
                          full.names = T)
  names_vec <- list.files("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Macroeconomic Analysis/Data/Time series csv's/",
                          full.names = F) %>% str_replace(".csv", "")

  # step 1
  df_list <- list()
  for (n in seq_along(paths_vec)){
    print(n)
    df <- read.csv(paths_vec[n])

    df_list[[n]] <- df
  }

  # step 2

  df_list <- date_maker_list(df_list)

  # step 3

  for (n in seq_along(names_vec)){
    print(n)

    assign(names_vec[n], df_list[[n]],envir = .GlobalEnv )
  }
return(df_list)
}
