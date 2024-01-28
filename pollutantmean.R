# get_info <- function(directory, id) { }

pollutantmean <- function(directory, pollutant, id = 1:332)
{
  
  numbers <- c()
  
  for (j in id)
  {
    
    zeroes <- paste(rep("0", 3 - (floor(log(j, 10)) + 1)), collapse = "")
    pad <- paste(directory, "/", zeroes, j, ".csv", sep = "")
    
    data <- read.csv(pad, header = TRUE)
    relevant <- data[[pollutant]]
    relevant <- relevant[!is.na(relevant)]
    numbers <- c(numbers, relevant)
    
  }
  
  mean(numbers)
  
}

complete <- function(directory, id = 1:332)
{
  
  # info <- data.frame(id = numeric(0), nobs = numeric(0))
  
  numbers <- c()
  
  for (j in id)
  {
    
    zeroes <- paste(rep("0", 3 - (floor(log(j, 10)) + 1)), collapse = "")
    pad <- paste(directory, "/", zeroes, j, ".csv", sep = "")
    
    data <- read.csv(pad, header = TRUE)
    good <- data[complete.cases(data["sulfate"], data["nitrate"]), ]
    
    numbers <- c(numbers, nrow(good))
    
  }
  
  data.frame(id = id, nobs = numbers)
  
}

corr <- function(directory, threshold = 0)
{
  
  numbers <- c()
  
  for (j in 1:332)
  {
    
    zeroes <- paste(rep("0", 3 - (floor(log(j, 10)) + 1)), collapse = "")
    pad <- paste(directory, "/", zeroes, j, ".csv", sep = "")
    
    data <- read.csv(pad, header = TRUE)
    good <- data[complete.cases(data["sulfate"], data["nitrate"]), ]
    
    if (nrow(good) < threshold)
    {
      next
    }
    
    numbers <- c(numbers, cor(good$sulfate, good$nitrate))
    
  }
  
  numbers
  
}