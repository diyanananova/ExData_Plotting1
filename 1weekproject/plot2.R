
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
    
  
#plot 2
    
png(filename="plot2.png")
plot2 <- plot(data_chars_as_num$Data_Time , data_chars_as_num$Global_active_power, xlab="" , ylab="Global Active Power (kilowatts)", type = "l")
dev.off()
