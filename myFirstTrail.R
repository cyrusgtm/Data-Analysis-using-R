install.packages(cars)
cars
cars$speed
data <- c(1, 2, 3)
for (i in cars$speed){
  if (i %% 2 == 0) 
    newData <- append(data, i)
  }
print(newData)
