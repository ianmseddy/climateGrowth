#type is forecast or hindcast
#to resample historical years into the future, use `type = "forecast"` and `yearType = "historical_years"`
# for normal variables, use `yearType = "future_period"` or `yearType = "historical_period"`
makeClimateVariablesForModule <- function(vars, 
                                          type = "projected", 
                                          years = 2020:2050, 
                                          yearType = "future_years") {
  if (grepl("period", yearType)) {
    vars <- paste0(vars, "_normal")
  }
  
  if (type == "projected") {
    out <- lapply(vars, FUN = function(var, year = years){
      
      a <- list(vars = paste0("future_", var), 
                fun = quote(calcAsIs),
                .dots = list(year))
      return(a)
    })
  } else if (type == "historical") {
    out <- lapply(vars, FUN = function(var, year = years) {
      a <- list(vars = paste0("historical_", var), 
                fun = quote(calcAsIs),
                .dots = list(year))
      return(a)
    })
  }  else {
    stop("incorrect type: ", type)
  }
  names(out) <- paste0(type, "_", vars)
  out <- lapply(out, function(x) {
    x$.dots <- setNames(x$.dots, yearType)
    return(x)
  })
  
  return(out)
}