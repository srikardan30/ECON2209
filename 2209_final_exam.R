library(fpp3)
library(pander)
library(fppp)


########################### Q1 #########################################

library(readabs)
lfs_1 <- read_abs("6202.0", tables = 1) %>%
  filter(series_type=="Original") %>%
  mutate (Month = yearmonth(date)) %>%
  as_tsibble (
    index = Month,
    key = c (series_id)
  )


set.seed(5359063)
myseries <- lfs_1 %>%
  filter (`series_id` == sample (lfs_1$`series_id` , 1), year(Month)>=2000)

myseries %>% autoplot(value)

#part a

myseries %>% gg_season(value)

myseries %>% gg_subseries(value)

myseries %>% gg_lag(value, geom = 'point', lags = 1:12)

myseries %>% ACF(value, lag_max = 48) %>% autoplot()

myseries %>% features(value, features = guerrero)
#-0.281

myseries %>% autoplot(box_cox(value, -0.281)) + labs(title = "Box-Cox")


#part b 

#training data set 
myseries_train <- myseries %>% filter(year(Month) < 2015)

#to check i got the right data extracted
autoplot(myseries, value) +
  autolayer(myseries_train, value, colour = "red") +
  ggtitle("Training Data Set (RED) and myseries Data Set (BLACK)")

fit <- myseries_train %>% model(
  'Seasonal Naive' = SNAIVE(value)
)

fit %>% gg_tsresiduals()

fc <- fit %>%
  forecast(new_data = anti_join(myseries, myseries_train))
fc %>% autoplot(myseries)

fit %>% accuracy()
fc %>% accuracy(myseries)


#part c

#multiplicative and damped method

fit <- myseries %>% 
  model(
    'Multiplicative' = ETS(value ~ error("M") + trend("A") + season("M")), 
    'Damped Trend' = ETS(value ~ error("M") + trend("Ad") + season("M"))
  )

fc <- fit %>% forecast(h = 12)

fc %>% autoplot(filter(myseries, year(Month) > 2015))

pander(accuracy(fit) %>% select(.model, .type, RMSE:MAPE))


#part d
myseries_train <- myseries %>% filter(year(Month) < 2015)

fit <- myseries_train %>% 
  model(
    'Seasonal Naive Model' = SNAIVE(value), 
    'Drift Model' = RW(value ~ drift()), 
    'STL-ARIMA' = decomposition_model(STL(value), 
                                      ARIMA(season_adjust))
  )

pander(accuracy(fit) %>% select(.model, .type, RMSE:MAPE))
#STL-ARIMA is the best model hands down

########################### Q2 #########################################

#course project data 
#################
nadata <- read_abs("5206.0", tables="6", check_local=FALSE) %>%
  mutate(Quarter = yearquarter (date)) %>%
  as_tsibble(
    index = Quarter,
    key = c (series_id)
  )

#Statistical Discrepancy
nadata_vol <- nadata %>% filter(series_type == "Original") %>%
  filter(!(`series_id` %in% c("A2323348A", "A2302356K",
                              "A2302358R", "A2302459A", "A2529213W")))

#Selecting my unique data based on zID
set.seed(5359063)
myseries <- nadata_vol %>%
  filter (`series_id` == sample(nadata_vol$`series_id` , 1), year(Quarter)>=1995)

#Extracting seasonally adjusted series 
myseries_sa <- nadata %>%
  filter(series_type == "Seasonally Adjusted") %>%
  filter(series == myseries$series[1], year(Quarter)>=1995)
#################


#dynamic regression model with Fourier terms 
#no box-cox was needed in cp so no need to carry over here 

fit <- myseries %>%
  model(
    'K_1' = ARIMA(log(value) ~ fourier(K = 1) + pdq(0:2, 0, 0:2) + PDQ(0:1, 0, 0:1)),
    'K1' = ARIMA(log(value) ~ fourier(K = 1) + PDQ(0,0,0)),
    'K2' = ARIMA(log(value) ~ fourier(K = 2) + PDQ(0,0,0)),
    'K_2' = ARIMA(log(value) ~ fourier(K = 2) + pdq(0:2, 0, 0:2) + PDQ(0:1, 0, 0:1)), 
    'K_1_BoxCox' = ARIMA(box_cox(value, 1.61) ~ fourier(K = 1) + pdq(0:2, 0, 0:2) + PDQ(0:1, 0, 0:1)), 
    'K_2_BoxCox' = ARIMA(box_cox(value, 1.61) ~ fourier(K = 2) + pdq(0:2, 0, 0:2) + PDQ(0:1, 0, 0:1)), 
  )

glance(fit)

pander(
  glance(fit) %>%
    arrange(AICc) %>%
    select(.model, AICc))


fit %>%
  select(K_2) %>%
  gg_tsresiduals()


fit %>%
  select(K_2) %>%
  coefficients() %>%
  nrow()




best_fit %>% gg_tsresiduals()


########################### Q3 #########################################
view(gafa_stock)

#part a
AMZN_stock <- gafa_stock %>% filter(Symbol == "AMZN") %>% 
  mutate(day = row_number()) %>% update_tsibble(index = day) 

AMZN_stock %>% autoplot(Close)

AMZN_stockv2 <- gafa_stock %>% filter(Symbol == "AMZN")
AMZN_stockv2 %>% autoplot(Close)

AMZN_stock %>% gg_tsdisplay(Close, plot_type = "partial", lag_max = 50)

#The first 50 ACF plot values show a high correlation in data and is decreasing slowly. 
#PACF is showing a huge spike at lag 1 and there is no other visible pattern in the data. 
#this indicates that there is a strong correlation in stock price data from the previous day. 
#Both of these observations show strong non-stationary behaviors and differencing is needed to control and stabilise the data 

#part b

#test for stationary and number of differences needed. 
AMZN_stock %>% features(Close, unitroot_kpss)
#From the table above we can see that the p-value is 1%. Since the p-value is 
#less than 5% we can reject the null hypothesis, meaning the data is definitely not stationary.

#This code gives us the number of differences needed according to R
AMZN_stock %>% features(Close, unitroot_ndiffs)
#1 difference needed

#This code gives us the number of seasonal differences needed according to R
AMZN_stock %>% features(Close, unitroot_nsdiffs)
#no seasonal difference needed since there is no seasonality and the data is random



#part c
#since all 3 are within the critical values (blue lines) they all indicate white noise
#main difference between the graphs is our ability to see it in detail, as we add more information we get a 
#less clear picture and less detail. But looking at the graphs all3 lie in the range hence are see as white noise. 

#part d
#all 3 of the graphs have different critical values, indicating that the data is different from each other 
#The critical values are at different distance from the mean of zero because white noise is expected to lie within
#±2/T?????????. T is the number of random numbers, which is 36, 360 and 1,000. The range expected for 36 random is ± 0.33. 
#The range expected for 360 is ± 0.11. The range expected for 360 is ± 0.063.





########################### Q4 #########################################


view(aus_airpassengers)
aus_airpassengers %>% autoplot(Passengers)

#part a
fit <- aus_airpassengers %>% model(ARIMA(Passengers))
report(fit)
#ARIMA(0,2,1)

fit %>% gg_tsresiduals()
#yes its white noise with some outliers in the histogram 

#forecast
fit %>% forecast(h = 10) %>% autoplot(aus_airpassengers)


#part b 

#part c
fit <- aus_airpassengers %>% 
  model(
    'Auto' = ARIMA(Passengers), 
    'ARIMA010 w Drift' = ARIMA(Passengers ~ 1 + pdq(0,1,0))
  )
glance(fit)

fit %>% forecast(h = 10) %>% autoplot(aus_airpassengers)

#part d

fit <- aus_airpassengers %>% 
  model(
    'Auto' = ARIMA(Passengers), 
    'ARIMA010 w Drift' = ARIMA(Passengers ~ 1 + pdq(0,1,0)),
    'ARIMA212 w Drift' = ARIMA(Passengers ~ 1 + pdq(2,1,2))
  )
glance(fit)

fit %>% forecast(h = 10) %>% autoplot(aus_airpassengers)

#part e
fit <- aus_airpassengers %>% 
  model(
    'Auto' = ARIMA(Passengers),
    'ARIMA021 w Constant' = ARIMA(Passengers ~ 1 + pdq(0,2,1))
  ) 

fit %>% forecast(h = 10) %>% autoplot(aus_airpassengers)
















