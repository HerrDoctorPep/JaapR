# Aditional feature engineering

engineer_features <- function(model_data){
  Mullerpier <- huizen_data$id[which(substring(trimws(huizen_data$postcode6), 1,5) == "3024E")]
  
  levels(model_data$Bijzonderheden) <- c(levels(model_data$Bijzonderheden),"Mullerpier")
  model_data$Bijzonderheden[which(model_data$id %in% Mullerpier)] <- "Mullerpier"
  
  return(model_data)
}

