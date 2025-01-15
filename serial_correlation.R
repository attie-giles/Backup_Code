# Load necessary packages
library(prais)     # For Prais-Winsten estimation
library(ggplot2)   # For visualization
library(stargazer) # For creating the regression result tables
library(dplyr)     # specifically uses lag()

# Set a seed for reproducibility
# set.seed(123)

# Simulate data
n <- 100                        # Number of time periods
time = 1:n 
x <- rnorm(n)  + time*.05 # Covariate x with trend
x <- x - mean(x) # center for good measure
rho <- 0.9                    # Autocorrelation coefficient
#eps <- arima.sim(list(ar=rho), n=n) # Serially correlated error term
# AR(1) errors
eps <- rnorm(n, 0, 5)
for (i in 2:n) {
  eps[i] <- eps[i] + rho * eps[i - 1]
}
beta0 <- 1                       # Intercept
beta1 <- 1                     # Slope coefficient

# Generate dependent variable y
y <- beta0 + beta1 * x + eps

# Create a data frame for modeling
serialdata <- data.frame(y = y, x = x, eps = eps, time)


# 1. Estimate the model using OLS
ols_model <- lm(y ~ x, data = serialdata)
serialdata$eps.hat <- residuals(ols_model)
serialdata <- serialdata %>% ungroup() %>%
  mutate(eps.hat_lag1=dplyr::lag(eps.hat,1),
         x_lag1=dplyr::lag(x,1),
         y_lag1=dplyr::lag(y,1)) #there are two lag() commands dplyr:: needed to call the right one

# 2. Estimate the model using Prais-Winsten
prais_model <- prais_winsten(y ~ x, data = serialdata, index="time")
prais_table <- summary(prais_model)

prais_coefs <- prais_table$coefficients[,"Estimate"]
prais_se <- prais_table$coefficients[,"Std. Error"]
prais_p <- prais_table$coefficients[,"Pr(>|t|)"]

# 2.5. Estimate the rho_hat
rho_model <- lm(eps.hat ~ eps.hat_lag1, data=serialdata)
rho.hat <- rho_model$coefficients[2]

# 3. Compare OLS and Prais-Winsten estimates in a table

stargazer(ols_model, ols_model, rho_model, type="text",
          coef = list(NULL, prais_coefs,     NULL),
          se =   list(NULL, prais_se,        NULL),
          p =    list(NULL, prais_p,         NULL),
          column.labels = 
                    c("OLS","Prais-Winsten", "Rho"),
          title = "OLS vs Prais-Winsten Estimates")

# 4. Plot the error term over time
ggplot(serialdata, aes(x = time, y = eps)) + 
  geom_line(color = "blue") + 
  labs(title = "Serially Correlated Errors Over Time",
       x = "Time", y = "Error Term (eps)") +
  theme_minimal()

# 4.1 Plot X over time
ggplot(serialdata, aes(x = time, y = x)) + 
  geom_line(color = "maroon") + 
  labs(title = "Trended X over time",
       x = "Time", y = "X") +
  theme_minimal()


# 5. Plot y on x with OLS and Prais-Winsten fitted values
serialdata$ols_fitted <- fitted(ols_model)                # OLS fitted values
serialdata$prais_fitted <- prais_model$fitted.values      # Prais-Winsten fitted values

ggplot(data=serialdata, aes(x = x, y = y))+   
  geom_path(alpha=.35, color='tomato3') +
  geom_point(alpha = 0.75, color='tomato3') + 
  geom_line(aes(y = ols_fitted, color = "OLS"), size = 1) + 
  geom_line(aes(y = prais_fitted, color = "Prais-Winsten"), linetype = "dashed", size = 1) + 
  labs(title = "y vs x with Fitted Values from OLS and Prais-Winsten",
       color="Model",
       x = "x", y = "y") + 
  scale_color_manual(values = c("OLS" = "tomato3", "Prais-Winsten" = "forestgreen")) +
  theme_minimal()

ggplot(data=serialdata, aes(x = x, y = y))+   
  geom_point(alpha = 0.6, color='tomato3') + 
  geom_point(aes(x=x-rho.hat*x_lag1, y=y-rho.hat*y_lag1), color="forestgreen")+
  geom_line(aes(y = ols_fitted, color = "OLS"), size = 1) + 
  geom_line(aes(y = prais_fitted, color = "Prais-Winsten"), linetype = "dashed", size = 1) + 
  labs(title = "y vs x with Fitted Values from OLS and Prais-Winsten",
       color="Model",
       x = "x", y = "y") + 
  scale_color_manual(values = c("OLS" = "tomato3", "Prais-Winsten" = "forestgreen")) +
  theme_minimal()

serialdata$scenario <- "OLS"
temp<-select(serialdata, c("x","y","time","x_lag1","y_lag1"))
temp$scenario <- "Prais-Winsten"
temp <- temp %>% 
  mutate(x=x-rho.hat*x_lag1, y=y-rho.hat*y_lag1)
serialdata.stacked <- bind_rows(serialdata, temp)

ggplot(data=serialdata.stacked, aes(x = x, y = y))+   
  geom_line(aes(group=time), color='forestgreen', alpha=.75, size=.3) +
  geom_point(aes(color=scenario), alpha = 0.95) +
  labs(color="Model") +
  geom_line(data=(serialdata), aes(y = ols_fitted, color = "OLS"), size = 1) + 
  geom_line(data=(serialdata), aes(y = prais_fitted, color = "Prais-Winsten"), linetype = "dashed", size = 1) + 
  labs(title = "Connecting transformed and actual data",
       color="Model",
       x = "x", y = "y") + 
  scale_color_manual(values = c("OLS" = "tomato3", 
                                "Prais-Winsten" = "forestgreen")) +
  theme_minimal()

time.select <- unique(subset(serialdata.stacked, y>20, select=c('time')))

# Combine both: paths and transformed-nontransformed but only for a subset of data
ggplot(data=subset(serialdata.stacked, time %in% time.select$time), aes(x = x, y = y))+   
  geom_line(aes(group=time), color='forestgreen', alpha=.75, size=.3) +
  geom_path(data=subset(serialdata.stacked, 
                        time %in% time.select$time &
                          scenario=="OLS"), 
                        alpha=.2, color='tomato3') +
  geom_point(aes(color=scenario), alpha = 0.5) +
  labs(color="Model") +
  geom_line(data=(serialdata), aes(y = ols_fitted, color = "OLS"), size = 1) + 
  geom_line(data=(serialdata), aes(y = prais_fitted, color = "Prais-Winsten"), linetype = "dashed", size = 1) + 
  labs(title = "Connecting transformed and actual data",
       color="Model",
       x = "x", y = "y") + 
  scale_color_manual(values = c("OLS" = "tomato3", 
                                "Prais-Winsten" = "forestgreen")) +
  theme_minimal()


