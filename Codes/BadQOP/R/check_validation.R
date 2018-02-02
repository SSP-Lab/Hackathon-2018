

check_validation <- function(df,score.var = "score"){
  
  valid <- lapply(seq(0,1,by = 0.1), function(k)({
    tempdf <- df %>% filter_(paste0(score.var,'>',k))
    return(data.frame(score = k, perf = as.numeric(
      mean(as.character(tempdf$id) == as.character(tempdf$siret),na.rm = T)),
      n = as.numeric(nrow(tempdf)))
    )
  }))
  
  valid <- do.call(rbind,valid)
  
  return(valid)
}