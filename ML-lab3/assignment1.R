set.seed(1234567890)
library(geosphere)

stations <- read.csv("stations.csv",fileEncoding = "Latin1")
temps <- read.csv("temps50k.csv",fileEncoding = "Latin1")

st <- merge(stations,temps,by="station_number")
h_distance <- 1000000# These three values are up to the students
  h_date <- 12
  h_time <- 7
  a <- 59.8586 # The point to predict (up to the students)
  b <- 17.6253
date <- "2015-07-12" # The date to predict (up to the students)
times <- c("04:00:00", "06:00:00", "08:00:00","10:00:00",
           "12:00:00" ,"14:00:00", "16:00:00","18:00:00",
           "20:00:00","22:00:00","24:00:00")
temp <- vector(length=length(times))
# Students’ code here

#distance kernel
gaussion_distance <- function(db_point, point_intreset)
{
  #distance to the other point
  dist <- distHaversine(db_point, point_intreset) 
  return (exp(-(dist / h_distance)^2)) #gaussian kernel
}

#gaussian date kernel
gaussian_date <- function(db_date, point_of_intreset_date)
{
  #date difference to other point
  diff_date <- as.numeric(difftime(point_of_intreset_date,db_date,unit = "days"))
  return (exp(-(diff_date / h_date)^2))
}

#gaussian time kernel
gaussian_hours <- function(db_time, point_of_intreset_date)
{
  #hours difference to other point
  diff_date <- as.numeric(difftime(point_of_intreset_date,db_time,unit = "hours"))
  return (exp(-(diff_date / h_time)^2))
}

point_intreset <- c(a,b) #point fo intreset
data_dist  = st[,c("longitude", "latitude")] 

#calculate gaussian distance 
gaussion_distacne_v<- gaussion_distance(data_dist, point_intreset)

#calculate gaussian date distance
gaussion_date_v <- gaussian_date(st$date,date)

time_conv <- data.frame(time=strptime( paste( Sys.Date(),st$time), "%Y-%m-%d %H:%M:%S"))
times <- strptime( paste( Sys.Date(),times), "%Y-%m-%d %H:%M:%S")

temp2 <- vector(length=length(times))
# temp_type <- vector(length=length(times))
# temp2_type <- vector(length=length(times))

for (i in 1:length(times))
{
  gausian_hours <- gaussian_hours(time_conv$time, times[i])
  #sum of all kernels
  sum_of_k <- (gaussion_distacne_v + gaussion_date_v + gausian_hours) 
  temp[i] <- sum((st$air_temperature * sum_of_k)) / sum(sum_of_k)
  # temp_type[i] <- "kernel_sum"
  
  #multiplication of kernels
  multiply_of_k <- (gaussion_distacne_v * gaussion_date_v * gausian_hours)
  temp2[i] <- sum((st$air_temperature * multiply_of_k)) / sum(multiply_of_k)
  # temp2_type[i] <- "kernel_mul"
}

#plot of additive kernels
plot(x=times,y=temp, type = "o" ,
     main = "Temperature using Additive kernels" , 
     xlab = "Time" , 
     ylab = "Temperature")

#plot of multiplicative kernels
plot(x=times,y=temp2, type = "o",
     main = "Temperature using Multiplicative kernels" , 
     xlab = "Time" , 
     ylab = "Temperature")

# result <- data.frame(kernel=c(temp,temp2)  ,type=c(temp_type,temp2_type), x=times)
# 
# library(ggplot2)
# ggplot(data=result, aes(x=x, y=kernel, colour=type)) + geom_line() + geom_point()
