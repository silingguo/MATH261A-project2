library(readxl)
library(ggplot2)
library(np)
library(dplyr)
library(patchwork)
dataset <- read_excel("C:/Users/guosiling/Downloads/Data_Set_Final_LTD_Slope_Intercept.xlsx")
View(dataset)
summary(dataset)
dataset$LC <- dataset$`Lung Cancer`

#basic statistic
summary(dataset)
var_LC = var(dataset$LC)
var_PM2.5 = var(dataset$PM2.5)
var_Sociod = var(dataset$Sociod_EQI)

#MIN MAX sociod eqi
min_row <- dataset[which.min(dataset$Sociod_EQI),]
min_row
max_row <- dataset[which.max(dataset$Sociod_EQI),]
max_row

# PM2.5 quantile for min max sociod eqi
minpm2.5 <- min_row$PM2.5
minpm2.5_q <- mean(dataset$PM2.5 <= minpm2.5)
maxpm2.5 <- max_row$PM2.5
maxpm2.5_q <- mean(dataset$PM2.5 <= maxpm2.5)
# LC quantile
minlc <- min_row$LC
minlc_q <- mean(dataset$LC <= minlc)
maxlc <- max_row$LC
maxlc_q <- mean(dataset$LC <= maxlc)


#MODEL 1
#multiple linear regression model
md1 <- lm(LC ~ PM2.5 + Sociod_EQI + PM2.5 * Sociod_EQI, data = dataset)
summary(md1)

#assumption check
#residual vs fitted value plot
md1_resid <- residuals(md1)
md1_fitted <- fitted(md1)
md1_df <- data.frame(residual = md1_resid,fitted = md1_fitted)
g1 <- ggplot(md1_df, aes(x = md1_fitted, y = md1_resid))+
  geom_point()+
  geom_hline(yintercept = 0) +
  labs(
    title = "Residuals vs Fitted Value of Model 1",
    x = "Fitted value",
    y = "Residuals"
  )

#residual histogram
g2 <- ggplot(data = NULL, aes(x = md1_resid)) +
  geom_histogram(bins = 30, fill = "lightyellow", color = "black", alpha = 0.7) +
  labs(title = "Residual Histogram of Model 1",
       x = "residual",
       y = "frequency")

#qqplot
g3 <-ggplot(md1_df,aes(sample = residual))+
  geom_qq() +
  geom_qq_line() +
  labs(title = "Quantile-quantile Plot of residuals of Model 1",
       x = "Theoretical Quantile (Normal Distribution)",
       y = "Sample Quantile")

#linearity
g4 <- ggplot(data = dataset,aes(x = PM2.5, y = log(LC)))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "LC vs PM2.5 of Model 1",
       x = "PM2.5",
       y = "LC")

g5 <- ggplot(data = dataset,aes(x = Sociod_EQI, y = log(LC)))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "LC vs Sociod_EQI of Model 1",
       x = "Sociod_EQI",
       y = "LC")

g1/g3 
g4+g5

#MODEL2
logmd2 <- lm(log(LC) ~ PM2.5 + Sociod_EQI + PM2.5 * Sociod_EQI, data = dataset)
summary(logmd2)



#residual vs fitted value plot
logmd2_resid <- residuals(logmd2)
logmd2_fitted <- fitted(logmd2)
logmd2_df <- data.frame(residual = logmd2_resid,fitted = logmd2_fitted)
g6 <- ggplot(logmd2_df, aes(x = fitted, y = residual))+
  geom_point()+
  geom_hline(yintercept = 0) +
  labs(
    title = "Residuals vs Fitted Value of Model 2",
    x = "Fitted Value",
    y = "Residuals")

#residual histogram
g7 <- ggplot(data = NULL, aes(x = logmd2_resid)) +
  geom_histogram(bins = 30, fill = "lightyellow", color = "black", alpha = 0.7) +
  labs(title = "Residual Histogram of Model 2",
       x = "Residual",
       y = "Frequency")

#qqplot
g8 <- ggplot(logmd2_df,aes(sample = residual))+
  geom_qq() +
  geom_qq_line() +
  labs(title = "Quantile-quantile plot of residuals",
       x = "Theoretical quantile (Normal distribution)",
       y = "Sample quantile")

#linearity
g9 <- ggplot(data = dataset,aes(x = PM2.5, y = log(LC)))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "log(LC) vs PM2.5 of Model 2",
       x = "PM2.5",
       y = "Log(LC)")

g10 <- ggplot(data = dataset,aes(x = Sociod_EQI, y = log(LC)))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "LC vs Sociod_EQI of Model 2",
       x = "Sociod_EQI",
       y = "Log(LC)")

g6/g8 
g9+g10
#interaction plot
dataset$Sociod_EQI_qt <- ntile(dataset$Sociod_EQI, 4)
summary(dataset$Sociod_EQI)
g11 <- ggplot(dataset,aes(x = PM2.5,y = LC,color = as.factor(Sociod_EQI_qt),group = Sociod_EQI_qt)) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  scale_color_manual(
    values = c("pink", "orange", "lightgreen", "lightblue"),
    labels = c("Q1: Best social conditions\n(Lowest SociodEQI)", "Q2: Moderately good", "Q3: Moderately poor", "Q4: Worst social conditions\n(Highest SociodEQI)")) +
  labs(title = "Sociod_EQI of PM2.5 Effect on LC",
       x = "PM2.5",
       y = "LC",
       color = "SociodEQI Quartile")

#MODEL3- WLS/ 
reg_datafram <- data.frame(
  LC = dataset$LC,
  PM2.5 = dataset$PM2.5,
  Sociod_EQI = dataset$Sociod_EQI,
  PM2.5_Soc = dataset$PM2.5 * dataset$Sociod_EQI
)
str(reg_datafram)
lm_fit <- lm(log(LC) ~ PM2.5 + Sociod_EQI + PM2.5 * Sociod_EQI,data = reg_datafram)
var_fit <- npreg(residuals(lm_fit)^2 ~ PM2.5 + Sociod_EQI + PM2.5_Soc,data = reg_datafram,nmulti = 1)
wls_fit <- lm(log(LC) ~ PM2.5 + Sociod_EQI + PM2.5 * Sociod_EQI, weights = 1 / fitted(var_fit),data = reg_datafram)
summary(wls_fit)

wls_df <- data.frame(
  fitted = fitted(wls_fit),
  residual = residuals(wls_fit)
)

####Look very similar to logmd2. not using WLS for this project
ggplot(wls_df, aes(x = fitted, y = residual))+
  geom_point()+
  geom_hline(yintercept = 0) +
  labs(
    title = "residuals vs fitted value wls",
    x = "fitted value",
    y = "residuals")

