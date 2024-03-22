#load the data
chip_dataset <- read.csv("chip_dataset.csv")

chip_dataset <- chip_dataset[,-1]
print(chip_dataset)


# Convert the release date
for(i in 1:nrow(chip_dataset)) {
  # Check if 'Na' is in the 'Release Date' column
  if(grepl("NaT", chip_dataset$`Release.Date`[i])) {
    # Replace with NA
    chip_dataset$`Release.Date`[i] <- NA
  } else {
    # Extract year, month, and day, then calculate new value
    date_split <- unlist(strsplit(chip_dataset$`Release.Date`[i], "/"))
    month <- as.numeric(date_split[1]) /12
    day <- as.numeric(date_split[2]) /365
    year <- as.numeric(date_split[3]) -2000
    chip_dataset$`Release.Date`[i] <- year + month + day
    }
  }
#change the release date to numerical
chip_dataset$`Release.Date` <- as.numeric(chip_dataset$`Release.Date`)
print(chip_dataset)

grouped <- split(chip_dataset, chip_dataset$Type)
#Group the data to CPU
CPU <- grouped$CPU
#remove all NA columns
CPU <- CPU[, !(names(CPU) %in% c('FP16.GFLOPS', 'FP32.GFLOPS', 'FP64.GFLOPS'))]


#find all the numerical columns
numerical_columns<- sapply(CPU, is.numeric)
# Verify that "Vendor" is a column name in CPU
print(numerical_columns)

#Extract the vendor columns
vendor_column <- CPU$Vendor
numerical_CPU <- CPU[, numerical_columns]

print(numerical_CPU)

# install the package ggally if not available
if (!require("GGally")) install.packages("GGally")
library(GGally)

# Create a pair plot
#plot between TDP and process_size
plot_data <- numerical_CPU[, c('TDP..W.', 'Process.Size..nm.')]
ggpairs(plot_data, mapping = aes(color = vendor_column))
#plot between TDP and release_date
plot_data <- numerical_CPU[, c('TDP..W.', 'Release.Date')]
ggpairs(plot_data, mapping = aes(color = vendor_column))

#plot between process_size and release_date
plot_data <- numerical_CPU[, c('Process.Size..nm.', 'Release.Date')]
ggpairs(plot_data, mapping = aes(color = vendor_column))

#----calculate confidence interval-------
alpha <- 0.05  # 95% confidence interval
#extract process_size from CPU
process_size=numerical_CPU$Process.Size..nm.

mean_process_size <- mean(process_size, na.rm = TRUE)
# Compute standard error
process_std_error <- sd(process_size, na.rm = TRUE) / sqrt(length(na.omit(process_size)))  
process_size_t_value <- qt(1 - alpha/2, df = length(na.omit(process_size)) - 1)
conf_interval_process_size <- c(mean_process_size - process_size_t_value * process_std_error, mean_process_size + process_size_t_value * process_std_error)

print(conf_interval_process_size)

#calculate the frequency confidence interval
#extract frequency from CPU
frequency_size=numerical_CPU$Freq..MHz.
mean_frequency_size <- mean(frequency_size, na.rm = TRUE)
std_error_frequency_size <- sd(frequency_size, na.rm = TRUE) / sqrt(length(na.omit(frequency_size)))
conf_interval_frequency_size <- mean_frequency_size + c(-1, 1) * qt(0.975, length(na.omit(frequency_size)) - 1) * std_error_frequency_size
print(conf_interval_frequency_size)
#fit the data
fit <- lm(Freq..MHz. ~ Process.Size..nm., data = chip_dataset)
summary(fit)
