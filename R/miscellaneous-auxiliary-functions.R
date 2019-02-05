### Miscellaneous auxiliary functions

# 1. Generation of triangular distribution random numbers
rtriang <- function(min, mode, max) {
  uni <- runif(1)
  if(uni <= (mode - min)/(max - min))
    rt <- min + sqrt(uni*(max - min)*(mode - min))
  else
    rt <- max - sqrt((1 - uni)*(max - min)*(max - mode))

  return(rt)
}

# 2. Simple descriptive statistics for rankings
describe <- function(v){
  return(c(min(v), max(v), median(v), sd(v)))
}
