library(forecast)
f <- read_csv( "/Users/xumaoran/Downloads/output/NationalNames.csv",col_type = "ccici")

f %>%
  filter(Name == "Alana") %>%
  # head() takes the first couple of rows in the data set
  tail()

f %>%
  filter(Name == "Howard") %>%
  # Basic plot command
  ggplot() +
  # Adding a line "layer" to the plot - note that we specify the x and y variances using mapping
  # Reference cheat sheet for different types of graphs
  geom_line(mapping = aes(x = Year, y = Count, color = Gender)) +
  # Adding axis titles
  labs(title = "How the name 'Howard' has changed over the years", x = "Year", 
       y = "Number of Babies Born")

id <- f$Id
name <- f$Name
year <- f$Year
count <- f$Count
Howard_raw <- which(name=='Howard')
year_raw <- year[Howard_raw]
c <- count[Howard_raw]
Howard <- which(c>100)
year_Howard <- year_raw[Howard]
count_Howard <- c[Howard]
par(mfrow=c(1,1))
plot(year_Howard, count_Howard, type='l', xlab = 'year')
acf(Howard)
dHoward = diff(count_Howard)
plot(dHoward, type="o", main="first difference", cex=0.5)
acf(dHoward)

Howard_raw <- which(name=='Howard')
year_raw <- year[Howard_raw]
c <- count[Howard_raw]
Howard <- which(c>100)
year_Howard <- year_raw[Howard]
count_Howard <- c[Howard]
par(mfrow=c(3,1))
plot(year_Howard, count_Howard, type='l', xlab = 'year', ylab = 'John')
acf(Howard)
dHoward = diff(count_Howard,lag = 1)
plot(year_Howard[-1], dHoward, type="o", main="first difference", cex=0.5,xlab='year')
acf(dHoward)
pacf(dHoward)

ma.6 <-  arima(dHoward, order = c(0,0,6), method = 'CSS')
forecast <- forecast.Arima(ma.6,h=20,level=c(99.5))
plot.forecast(forecast, main = 'Forecast of Howard in 20 years')
