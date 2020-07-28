### works with AUC to extract the most meaningful aspects


AU_discrete_engineering  <- function(df) {
  
  # engineers smile, blink, fake smile, and a measure of "amount of AUC events
  df <- df %>%
    mutate(smile = ifelse(AU06_c == 1 & AU12_c == 1, 1, 0)) %>%
    mutate(blink = AU45_c) %>%
    mutate(brow_up = AU02_c) %>%
    mutate(fake_smile = ifelse(AU06_c == 0 & AU12_c == 1, 1, 0))
}


#TODO AU r engineering