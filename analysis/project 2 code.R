library(readxl)
library(ggplot2)
dataset <- read_excel("C:/Users/guosiling/Downloads/Data_Set_Final_LTD_Slope_Intercept.xlsx")
View(dataset)
summary(dataset)

#MODEL 1
#multiple linear regression model
md1 <- lm(`Lung Cancer` ~ PM2.5 + Sociod_EQI, data = dataset)
summary(md1)

#log transformation
logmd1 <- lm(log(`Lung Cancer`) ~ PM2.5 + Sociod_EQI, data = dataset)
summary(logmd1)

#assumption check
#residual vs fitted value plot
md1_resid <- residuals(md1)
md1_fitted <- fitted(md1)
re_fi_df <- data.frame(residual = md1_resid,fitted = md1_fitted)
ggplot(re_fi_df, aes(x = md1_fitted, y = md1_resid))+
  geom_point()+
  geom_hline(yintercept = 0) +
  labs(
    title = "residuals vs fitted value",
    x = "fitted value",
    y = "residuals"
  )
#logtransformation residual vs fitted value
logmd1_resid <- residuals(logmd1)
logmd1_fitted <- fitted(logmd1)
logre_fi_df <- data.frame(residual = logmd1_resid,fitted = logmd1_fitted)
ggplot(logre_fi_df, aes(x = logmd1_fitted, y = logmd1_resid))+
  geom_point()+
  geom_hline(yintercept = 0) +
  labs(
    title = "residuals vs fitted value",
    x = "fitted value",
    y = "residuals"
  )

#MODEL2
md2 <- lm(`Lung Cancer` ~ PM2.5 + Sociod_EQI + PM2.5 * Sociod_EQI, data = dataset)
summary(md2)

logmd2 <- lm(log(`Lung Cancer`) ~ PM2.5 + Sociod_EQI + PM2.5 * Sociod_EQI, data = dataset)
summary(logmd2)


#residual vs fitted value plot
logmd2_resid <- residuals(logmd2)
logmd2_fitted <- fitted(logmd2)
logre_fi_df2 <- data.frame(residual = logmd2_resid,fitted = logmd2_fitted)
ggplot(logre_fi_df2, aes(x = fitted, y = residual))+
  geom_point()+
  geom_hline(yintercept = 0) +
  labs(
    title = "residuals vs fitted value",
    x = "fitted value",
    y = "residuals")

#residual histogram
ggplot(data = NULL, aes(x = logmd2_resid)) +
  geom_histogram(bins = 30, fill = "lightyellow", color = "black", alpha = 0.7) +
  labs(title = "residual histogram",
       x = "residual",
       y = "frequency")

#qqplot
ggplot(logre_fi_df2,aes(sample = residual))+
  geom_qq() +
  geom_qq_line() +
  labs(title = "Quantile-quantile plot of residuals",
       x = "Theoretical quantile (Normal distribution)",
       y = "Sample quantile")

#linearity
ggplot(data = dataset,aes(x = PM2.5, y = log(`Lung Cancer`)))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Lung vs PM2.5",
       x = "PM2.5",
       y = "log(Lung Cancer)")

ggplot(data = dataset,aes(x = Sociod_EQI, y = log(`Lung Cancer`)))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Lung vs Sociod_EQI",
       x = "Sociod_EQI",
       y = "log(Lung Cancer)")

#interaction plot


#?? Do I need to do this?? MODEL3- WLS/LOG

WLS_LOG_MD <- lm(log(`Lung Cancer`) ~ PM2.5 + Sociod_EQI + PM2.5 * Sociod_EQI, data = dataset)