balance_panel <- function(df, freq = 12){
  stopifnot("date" %in% names(df))
  library(dplyr); library(zoo); library(forecast)
  
  df <- df %>% arrange(date)
  
  # início da série mensal
  start_y <- as.integer(format(min(df$date), "%Y"))
  start_m <- as.integer(format(min(df$date), "%m"))
  
  num_cols <- names(df)[sapply(df, is.numeric) & names(df) != "date"]
  
  df %>%
    mutate(across(all_of(num_cols), ~{
      col <- as.numeric(.x)
      if(all(is.na(col))) return(col)             # deixa como está
      
      col[is.infinite(col)] <- NA                 # limpa infinitos
      
      # ts mensal com mesmo comprimento do data.frame
      x <- ts(col, start = c(start_y, start_m), frequency = freq)
      
      # 1) tente interpolação com sazonalidade
      out <- tryCatch(as.numeric(na.interp(x)), error = function(e) NULL)
      if(!is.null(out)) return(out)
      
      # 2) fallback: interp linear + LOCF (frente e trás)
      out <- na.approx(col, na.rm = FALSE)
      out <- na.locf(out, na.rm = FALSE)
      out <- na.locf(out, fromLast = TRUE, na.rm = FALSE)
      out
    }))
}
balanced <- balance_panel(df, freq = 12)  # mensal
tail(as_tibble(balanced))
