#' Function to add columns to the imported data.
#' 
#' 

add_column_names <- function(data, type){
  remove_empty <- dplyr::select(data, !column13)
  names(remove_empty) <- col_names
  return(remove_empty)
}

col_names <- c(
  "CEMETERY",
  "SITECODE",
  "PERIOD",
  "LU_DATE",
  "E_DATE",
  "L_DATE",
  "CONTEXT",
  "SEX",
  "AGE",
  "TRAIT_TYPE",
  "GROUP",
  "EXPANSION",
  "VALUE"
)
