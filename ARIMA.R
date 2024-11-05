#############################################################################################################################################
######################################## ARIMA example using data from HiMCM 2021 Problem A Lake Mead #######################################
#############################################################################################################################################
rm(list=ls(all=TRUE)) # clear the environment

# /Users/frank/Desktop/extracurricular activities/competitions/mathematics modelling/03 Prediction Model/03-02 Time Series Analysis/ARIMA/Code

#################################################### library all the packages to be used ####################################################
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork)
library(gridExtra)
library(tseries)
library(forecast)
library(writexl)

######################################### input the raw data and perform preliminary transformation #########################################
dt <- read_xlsx(path = "/Users/frank/Desktop/extracurricular activities/competitions/mathematics modelling/HiMCM/past paper/2021/2021 Problem A/2021_HiMCM_LakeMead_MonthlyElevationData.xlsx", range = "A2:M89")

dt <- dt %>% mutate_at(.vars = vars(JAN), .fun = as.numeric)

dt1 <- dt %>% pivot_longer(cols = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"), names_to = "month", values_to = "amount")

# transfer the date variable
dt1 <- dt1 %>% mutate(months = ifelse(month == "JAN", "01/",
                               ifelse(month == "FEB", "02/",
                               ifelse(month == "MAR", "03/",
                               ifelse(month == "APR", "04/",
                               ifelse(month == "MAY", "05/",
                               ifelse(month == "JUN", "06/",
                               ifelse(month == "JUL", "07/",
                               ifelse(month == "AUG", "08/",
                               ifelse(month == "SEP", "09/",
                               ifelse(month == "OCT", "10/",
                               ifelse(month == "NOV", "11/",
                               ifelse(month == "DEC", "12/", NA
                               )))))))))))))
dt1 <- dt1 %>% mutate_at(.vars = vars(Year, months), .fun = as.character)
dt1 <- dt1 %>% mutate(date = paste(dt1$months, dt1$Year, sep = ""))
dt2 <- subset(dt1, select = c(date, amount))
vec_date <- as.vector(dt2$date)
date_date <- dmy(paste0("01-", vec_date))
dt2 <- dt2 %>% mutate(date = date_date)

class(dt2$date) # should be "Date"
class(dt2$amount) # should be "numeric"
rm(list=ls()[!ls() %in% "dt2"]) # clear all environment except dt2
dt2 <- na.omit(dt2)
# write_xlsx(dt2, "/Users/frank/Desktop/extracurricular activities/competitions/mathematics modelling/03 Prediction Model/03-02 Time Series Analysis/ARIMA/Code/lake_mead_data.xlsx")

######################################## plot difference process and perform ADF test ########################################
diff1 <- diff(dt2$amount)
diff1 <- c(diff1, NA)
dt.diff1 <- as.data.frame(diff1)
dt.diff1 <- dt.diff1 %>% mutate(date = dt2$date)
dt.diff1 <- na.omit(dt.diff1)

diff2 <- diff(dt.diff1$diff1)
diff2 <- c(diff2, NA)
dt.diff2 <- as.data.frame(diff2)
dt.diff2 <- dt.diff2 %>% mutate(date = dt.diff1$date)
dt.diff2 <- na.omit(dt.diff2)

p1 <- ggplot(dt2, aes(x = date, y = amount)) +
  geom_line(color = "black", linewidth = 0.8) +
  labs(title = "",
       x = "Date",
       y = "Raw data") +
  theme_classic()

p2 <- ggplot(dt.diff1, aes(x = date, y = diff1)) +
  geom_line(color = "darkblue", linewidth = 0.8) +
  labs(title = "",
       x = "Date",
       y = "1st difference") +
  theme_classic()

p3 <- ggplot(dt.diff2, aes(x = date, y = diff2)) +
  geom_line(color = "darkred", linewidth = 0.8) +
  labs(title = "",
       x = "Date",
       y = "2nd difference") +
  theme_classic()

diff.plots <- grid.arrange(p1, p2, p3, ncol = 1)
ggsave("/Users/frank/Desktop/extracurricular activities/competitions/mathematics modelling/03 Prediction Model/03-02 Time Series Analysis/ARIMA/Code/difference plots.png", plot = diff.plots, width = 10, height = 9, dpi = 300)

# ADF tests, p<0.05 for stationary time series
adf.test(na.omit(dt2$amount))
adf.test(na.omit(dt.diff1$diff1))
adf.test(na.omit(dt.diff2$diff2))

rm(list=ls()[!ls() %in% c("dt.diff1", "dt.diff2", "dt2")])

######################################## ACF and PACF plots used for determining p and q parameters ########################################
# png("/Users/frank/Desktop/extracurricular activities/competitions/mathematics modelling/03 Prediction Model/03-02 Time Series Analysis/ARIMA/Code/ACF PACF plots.png", width = 4500, height = 2000, res = 300)
# par(mfrow = c(1, 2))
# acf(na.omit(dt.diff1$diff1), main = "") # to determine q
# pacf(na.omit(dt.diff1$diff1), main = "") # to determine p
# dev.off()

acf_values <- acf(na.omit(dt.diff1$diff1), plot = FALSE)
pacf_values <- pacf(na.omit(dt.diff1$diff1), plot = FALSE)

acf_data <- data.frame(
  Lag = acf_values$lag,
  ACF = acf_values$acf
)

pacf_data <- data.frame(
  Lag = pacf_values$lag,
  PACF = pacf_values$acf
)

# to determine q
acf_plot <- ggplot(acf_data, aes(x = Lag, y = ACF)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "", x = "Lag", y = "ACF") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),      # Title size
    axis.title.x = element_text(size = 14),    # X-axis title size
    axis.title.y = element_text(size = 14),    # Y-axis title size
    axis.text.x = element_text(size = 12),     # X-axis labels size
    axis.text.y = element_text(size = 12)       # Y-axis labels size
  )

# to determine p
pacf_plot <- ggplot(pacf_data, aes(x = Lag, y = PACF)) +
  geom_bar(stat = "identity", fill = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "", x = "Lag", y = "PACF") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),      # Title size
    axis.title.x = element_text(size = 14),    # X-axis title size
    axis.title.y = element_text(size = 14),    # Y-axis title size
    axis.text.x = element_text(size = 12),     # X-axis labels size
    axis.text.y = element_text(size = 12)       # Y-axis labels size
  )

ACF_PACF <- grid.arrange(acf_plot, pacf_plot, ncol = 2)
ggsave("/Users/frank/Desktop/extracurricular activities/competitions/mathematics modelling/03 Prediction Model/03-02 Time Series Analysis/ARIMA/Code/ACF PACF plots.png", plot = ACF_PACF, width = 18, height = 9, dpi = 300)

########################################################## ARIMA model ####################################################################
water_amount_diff1 <- dt.diff1$diff1
arima.data <- ts(water_amount_diff1, start = c(1935, 2), frequency = 12)
arima.data.df <- as.data.frame(arima.data)

model <- arima(arima.data.df$x, order = c(3, 1, 3)) # sensitivity analysis: ARIMA(3, 1, 0) and ARIMA(0, 1, 3)
summary(model)

fitted_values <- fitted(model)
class(arima.data)

################################################## 1st order difference data ##################################################
# Original data and fitted values
plot_data <- data.frame(
  Date = na.omit(dt.diff1$date),
  Actual = as.numeric(dt.diff1$diff1),
  Fitted = as.numeric(fitted_values)
)

# Perform forecasting and prepare forecast data
forecasted_values <- forecast(model, h = 20)
forecasted_values_df <- as.data.frame(forecasted_values)
forecasted_values_df <- forecasted_values_df %>% mutate(Date = rownames(forecasted_values_df))
date_converted <- as.Date(paste("01", forecasted_values_df$Date), format="%d %b %Y")
forecasted_values_df <- forecasted_values_df %>% mutate(Date = date_converted)

# Generate a sequence of dates for the forecast period
last_date <- max(plot_data$Date, na.rm = TRUE)
forecasted_values_df$Date <- seq(last_date + months(1), by = "month", length.out = nrow(forecasted_values_df))

# Plot the ARIMA plot
ARIMA_p1 <- ggplot() +
  geom_line(data = plot_data, aes(x = Date, y = Actual), color = "darkblue", size = 0.8, linetype = "solid") +
  geom_line(data = plot_data, aes(x = Date, y = Fitted), color = "red", size = 0.5, linetype = "solid") +
  geom_line(data = forecasted_values_df, aes(x = Date, y = `Point Forecast`), color = "darkgreen", size = 0.5, linetype = "solid") +
  geom_ribbon(data = forecasted_values_df, aes(x = Date, ymin = `Lo 95`, ymax = `Hi 95`), alpha = 0.5, fill = "lightgrey") +
  labs(title = "",
       x = "Time",
       y = "Water amount") +
  theme_classic()

ARIMA_p1

ggsave("/Users/frank/Desktop/extracurricular activities/competitions/mathematics modelling/03 Prediction Model/03-02 Time Series Analysis/ARIMA/Code/1st difference ARIMA.png", plot = ARIMA_p1, width = 10, height = 7, dpi = 300)
rm(list=ls()[!ls() %in% c("dt.diff1", "dt.diff2", "dt2")])

################################################## original data ##################################################
acf(na.omit(dt2$amount), main = "")
pacf(na.omit(dt2$amount), main = "")

water_amount_dt2 <- dt2$amount
arima.data1 <- ts(water_amount_dt2, start = c(1935, 2), frequency = 12)
arima.data.df1 <- as.data.frame(arima.data1)

model1 <- arima(arima.data.df1$x, order = c(5, 0, 10)) # sensitivity analysis: ARIMA(3, 1, 0) and ARIMA(0, 1, 3)
summary(model1)

fitted_values1 <- fitted(model1)

# Original data and fitted values
plot_data1 <- data.frame(
  Date = na.omit(dt2$date),
  Actual = as.numeric(dt2$amount),
  Fitted = as.numeric(fitted_values1)
)

# Perform forecasting and prepare forcast data
forecasted_values1 <- forecast(model1, h = 20)
forecasted_values1_df <- as.data.frame(forecasted_values1)
forecasted_values1_df <- forecasted_values1_df %>% mutate(Date = rownames(forecasted_values1_df))
date1_converted <- as.Date(paste("01", forecasted_values1_df$Date), format="%d %b %Y")
forecasted_values1_df <- forecasted_values1_df %>% mutate(Date = date1_converted)

# Plot the ARIMA plot
ARIMA_p2 <- ggplot() +
  geom_line(data = plot_data1, aes(x = Date, y = Actual), color = "darkblue", size = 0.8, linetype = "solid") +
  geom_line(data = plot_data1, aes(x = Date, y = Fitted), color = "red", size = 0.5, linetype = "solid") +
  geom_line(data = forecasted_values1_df, aes(x = Date, y = `Point Forecast`), color = "darkgreen", size = 0.5, linetype = "solid") +
  geom_ribbon(data = forecasted_values1_df, aes(x = Date, ymin = `Lo 95`, ymax = `Hi 95`), alpha = 0.5, fill = "lightgrey") +
  labs(title = "",
       x = "Time",
       y = "Water amount") +
  theme_classic()
ARIMA_p2
ggsave("/Users/frank/Desktop/extracurricular activities/competitions/mathematics modelling/03 Prediction Model/03-02 Time Series Analysis/ARIMA/Code/original ARIMA.png", plot = ARIMA_p2, width = 10, height = 7, dpi = 300)
rm(list=ls()[!ls() %in% c("dt.diff1", "dt.diff2", "dt2", "plot_data1", "forecasted_values1_df", "arima.data1")])

######################################## zooming in the last time of original data ########################################
plot_data2 <- tail(plot_data1, 50)

ARIMA_p3 <- ggplot() +
  geom_line(data = plot_data2, aes(x = Date, y = Actual), color = "darkblue", size = 0.8, linetype = "solid") +
  geom_line(data = plot_data2, aes(x = Date, y = Fitted), color = "red", size = 0.5, linetype = "solid") +
  geom_line(data = forecasted_values1_df, aes(x = Date, y = `Point Forecast`), color = "darkgreen", size = 0.5, linetype = "solid") +
  geom_ribbon(data = forecasted_values1_df, aes(x = Date, ymin = `Lo 95`, ymax = `Hi 95`), alpha = 0.5, fill = "lightgrey") +
  labs(title = "",
       x = "Time",
       y = "Water amount") +
  theme_classic()
ARIMA_p3
ggsave("/Users/frank/Desktop/extracurricular activities/competitions/mathematics modelling/03 Prediction Model/03-02 Time Series Analysis/ARIMA/Code/original ARIMA (zooming in).png", plot = ARIMA_p3, width = 10, height = 7, dpi = 300)
rm(list=ls()[!ls() %in% c("dt.diff1", "dt.diff2", "dt2", "plot_data1", "forecasted_values1_df", "arima.data1")])

################################################## components plots ##################################################
decomposed <- stl(arima.data1, s.window=12)

trend <- decomposed$time.series[, "trend"]
seasonal <- decomposed$time.series[, "seasonal"]
residual <- decomposed$time.series[, "remainder"]

components <- data.frame(
  Time = na.omit(dt2$date),
  Observed = arima.data1,
  Trend = trend,
  Seasonal = seasonal,
  Residual = residual
)

component.p1 <- ggplot(components, aes(x = Time)) +
  geom_line(aes(y = Observed), color = "darkblue", size = 1) +
  labs(title = "", y = "Data", x = "Time") +
  theme_gray()

component.p2 <- ggplot(components, aes(x = Time)) +
  geom_line(aes(y = Trend), color = "red", size = 1) +
  labs(title = "", y = "Trend", x = "Time") +
  theme_gray()

component.p3 <- ggplot(components, aes(x = Time)) +
  geom_line(aes(y = Seasonal + max(arima.data1)), color = "darkgreen", size = 1) +  
  labs(title = "", y = "Seasonality", x = "Time") +
  theme_gray()

component.p4 <- ggplot(components, aes(x = Time)) +
  geom_line(aes(y = Residual + max(arima.data1)), color = "darkorange", size = 1) + 
  labs(title = "", y = "Residual", x = "Time") +
  theme_gray()

component.plots <- grid.arrange(component.p1, component.p2, component.p3, component.p4, ncol = 1)
ggsave("/Users/frank/Desktop/extracurricular activities/competitions/mathematics modelling/03 Prediction Model/03-02 Time Series Analysis/ARIMA/Code/component plots.png", plot = component.plots, width = 10, height = 15, dpi = 300)
rm(list=ls(all=TRUE)) # clear the environment
