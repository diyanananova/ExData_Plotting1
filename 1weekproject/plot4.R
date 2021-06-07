
#chron
#lubridate
#dplyr
#tidyverse

reads_c4_w1 <- read.table("/Users/diyanananova/Desktop/coursera/coursera_cours4_week1/household_power_consumption.txt", sep = ";", header = TRUE)
reads_new <- reads_c4_w1 %>%
  mutate(Date = dmy(Date), Global_active_power = as.numeric(Global_active_power)) %>%
  filter(between(Date, as.Date("2007-02-01"),as.Date("2007-02-02"))) %>%
  add_count(Global_active_power, wt = NULL, name = "Frequency") %>%
  drop_na() 

Date_Time <- paste(reads_new$Date, reads_new$Time)
plot_3 <- reads_new %>%
  select(-Time) %>%
  mutate(Data_Time = as.POSIXct(Date_Time))

char_columns <- sapply(plot_3, is.character)             # Identify character columns
data_chars_as_num <- plot_3                          # Replicate data
data_chars_as_num[ , char_columns] <- as.data.frame(   # Recode characters as numeric
  apply(data_chars_as_num[ , char_columns], 2, as.numeric))

#plot 4

png(filename="plot4.png")
attach(data_chars_as_num)
op <- par(mfcol = c(2, 2),
          mar = c(1,4,1,4) + 0.1)
plot4 <- plot(data_chars_as_num$Data_Time, data_chars_as_num$Global_active_power, type = "l", col = "black",  ylab = "Global_active_power", xlab = "datetime")

plot(data_chars_as_num$Data_Time, data_chars_as_num$Sub_metering_1, type = "l", col = "black",  ylab = "Energy sub metering", xlab = "",) # Plot with Base R
lines(data_chars_as_num$Data_Time, data_chars_as_num$Sub_metering_2, type = "l", col = "red")
lines(data_chars_as_num$Data_Time, data_chars_as_num$Sub_metering_3, type = "l", col = "blue")
mycols = c("black","red", "blue")
legend("topright", cex = 0.5, legend = c("Sub metering 1", "Submetering 2", "Submetering 3"), fill = mycols)

plot(data_chars_as_num$Data_Time, data_chars_as_num$Voltage, type = "l", col = "black",  ylab = "Voltage", xlab = "datetime")

plot(data_chars_as_num$Data_Time, data_chars_as_num$Global_reactive_power, type = "l", col = "black",  ylab = "Global_reactive_power", xlab = "datetime")
dev.off()
