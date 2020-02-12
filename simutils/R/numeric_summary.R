numeric_summary <-
function(x, na.rm) {
  
  # Include an error if x is not numeric
  if(!is.numeric(x)){
    stop("Data must be numeric")
  }
  
  # Create data frame
  data.frame( min = min(x, na.rm = na.rm),
       median = median(x, na.rm = na.rm),
       sd = sd(x, na.rm = na.rm),
       max = max(x, na.rm = na.rm))
}
