# Create numeric_summary() function
numeric_summary <- function(x, na.rm) {
  
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

# Test numeric_summary() function
numeric_summary(airquality$Ozone, TRUE)


# for dumping an R object into a directory
dir()
#dump("numeric_summary", file = "R/numeric_summary.R")
use_data(weather, overwrite = TRUE)
use_vignette("Generating_Summaries_with_Data_Summary")


install.packages("roxygen2")




