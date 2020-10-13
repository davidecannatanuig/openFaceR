#' Discrete features engineering.
#' @description A summary function calculating the most common discrete elements.
#' @param df the dataframe from which extracting the facial features.
#' @return a dataframe including measures of smiles, blinks, face smiles and overall count of AUCs.
#' @examples
#' au_discrete_engineering(john)
#'
#' @note DRAFT ONLY

au_discrete_engineering  <- function(df) {

  # engineers smile, blink, fake smile, and a measure of "amount of AUC events
  df <- df %>%
    dplyr::mutate(smile = ifelse(AU06_c == 1 & AU12_c == 1, 1, 0)) %>%
    dplyr::mutate(blink = AU45_c) %>%
    dplyr::mutate(brow_up = AU02_c) %>%
    dplyr::mutate(fake_smile = ifelse(AU06_c == 0 & AU12_c == 1, 1, 0))
}


#TODO AU r engineering
