# =================================
# EC 422 - Problem Set 3
# =================================

# Part 2 ---------------------------
# 2.2 R

#Did this to correct earlier renaming error
list.files("data")

library(tidyverse)

retail_raw <- read_csv("data/retail_sales_raw.csv")

head(retail_raw)
# A tibble: 6 × 3
#observation_date  RSAFS RSAFSNA
#<date>            <dbl>   <dbl>
#1 1992-01-01       159177  142051
#2 1992-02-01       159189  142498
#3 1992-03-01       158647  154439
#4 1992-04-01       159921  158430
#5 1992-05-01       160471  164788
#6 1992-06-01       161205  163417

#Answer to 2.1a: Observation date is considered a <date> class, while RSAFS and RSAFNA are the <dbl> class

#2.1b
retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

head(retail_raw)

#2.3
retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")

head(retail_raw_long)

# A tibble: 6 × 3
#observation_date series_id  sales
#<date>           <chr>      <dbl>
#1 1992-01-01       RSAFS     159177
#2 1992-01-01       RSAFSNA   142051
#3 1992-02-01       RSAFS     159189
#4 1992-02-01       RSAFSNA   142498
#5 1992-03-01       RSAFS     158647
#6 1992-03-01       RSAFSNA   154439

#Answer to 2.3a: Data went from wide to long, where now sales has both the RSAFS and the RSAFSNA values, and sales_id says which is came from.

#2.4

#2.4a
library(fpp3)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

#2.4b
head(retail_ts)
# A tsibble: 6 x 4 [1M]
# Key:       series_id [1]
#observation_date series_id  sales    Month
#<date>           <chr>      <dbl>    <mth>
#1 1992-01-01       RSAFS     159177 1992 Jan
#2 1992-02-01       RSAFS     159189 1992 Feb
#3 1992-03-01       RSAFS     158647 1992 Mar
#4 1992-04-01       RSAFS     159921 1992 Apr
#5 1992-05-01       RSAFS     160471 1992 May
#6 1992-06-01       RSAFS     161205 1992 Jun

has_gaps(retail_ts)
# A tibble: 2 × 2
#series_id .gaps
#<chr>     <lgl>
#1 RSAFS     FALSE
#2 RSAFSNA   FALSE

#Answer to 2.4b: No gaps.

#2.5

#2.5a
key_vars(canadian_gas)
index_var(canadian_gas)

#2.5b
can_gas <- canadian_gas |>
  select(Month, Volume)

has_gaps(can_gas)

head(can_gas)
# A tsibble: 6 x 2 [1M]
#Month Volume
  #<mth>  <dbl>
#1 1960 Jan   1.43
#2 1960 Feb   1.31
#3 1960 Mar   1.40
#4 1960 Apr   1.17
#5 1960 May   1.12
#6 1960 Jun   1.01

#2.6

#2.6a
can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)
# A tsibble: 6 x 2 [1M]
#Month Volume
#<mth>  <dbl>
#1 1990 Jan   12.5
#2 1990 Feb   11.2
#3 1990 Mar   11.6
#4 1990 Apr   11.1
#5 1990 May   11.0
#6 1990 Jun   10.1

#2.6b
can_gas_recent <- can_gas_recent |>
  mutate(year    = year(Month),
         month   = month(Month, label = TRUE),
         quarter = quarter(Month))

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

# A tibble: 12 × 2
#month avg_volume
#<ord>      <dbl>
#1 Jan         17.0
#2 Feb         15.3
#3 Mar         16.3
#4 Apr         15.7
#5 May         15.7
#6 Jun         14.9
#7 Jul         15.6
#8 Aug         15.6
#9 Sep         15.1
#10 Oct         16.2
#11 Nov         16.2
#12 Dec         17.1

#Answer 2.6b: December has highest production at an average volume of 17.1, while June has the lowest at 14.9.

#2.6c
can_gas_recent |>
  mutate(next_month           = Month + 1,
         same_month_last_year = Month - 12) |>
  head()
# A tsibble: 6 x 7 [1M]
#Month Volume  year month quarter next_month same_month_last_year
#<mth>  <dbl> <dbl> <ord>   <int>      <mth>                <mth>
#1 1990 Jan   12.5  1990 Jan         1   1990 Feb             1989 Jan
#2 1990 Feb   11.2  1990 Feb         1   1990 Mar             1989 Feb
#3 1990 Mar   11.6  1990 Mar         1   1990 Apr             1989 Mar
#4 1990 Apr   11.1  1990 Apr         2   1990 May             1989 Apr
#5 1990 May   11.0  1990 May         2   1990 Jun             1989 May
#6 1990 Jun   10.1  1990 Jun         2   1990 Jul             1989 Jun

#2.6d
can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)
# A tsibble: 6 x 2 [1Q]
#Quarter Volume
#<qtr>  <dbl>
#1 1960 Q1   4.14
#2 1960 Q2   3.30
#3 1960 Q3   2.97
#4 1960 Q4   4.26
#5 1961 Q1   5.01
#6 1961 Q2   4.17

can_gas_q |> autoplot(Volume)

#Answer to 2.6d:We use sum()instead of mean because we want the total volume (production) for the quarter, not the average for each quarter. 
#Also it does still seem to be seasonal, as Q1 and Q4 are always highest which points to higher production during the months in those quarters.

#2.7

#2.7a
retail_ts |> autoplot(sales)

#2.7b
retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

#Answer to 2.7b: Right around January of 2010 and 2020 there seems to be a steep drop in both the SA and NSA lines.
#The two series generally follow the same path but NSA is less smooth, and had steeper drops during the near January holiday season than SA did. 

#2.7c
can_gas |>
  autoplot(Volume) +
  labs(
    title = "Canadian Natural-Gas Production",
    x = NULL,
    y = "Cubic Meters (Billions)"
  ) +
  theme_minimal()

#2.7d
retail_ts |>
  autoplot(sales) +
  geom_smooth(se = FALSE) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    x = NULL,
    y = "Dollars (Millions)"
  ) +
  theme_minimal()

#Answer to 2.7d: I choose the retail_ts with geom_smooth to show more clearly the trends in retail sales and visualize the difference when there are steep drops.

# Part 3 ---------------------------

#3.1

#Answer to 3.1a: No, the seasonal swings do not get larger as the series grows, the swings seem the change in a different way. Generally, the graph is increasing and most swings seem unrelated to a seasonal explantion.

#3.1b
can_gas <- can_gas |>
  mutate(log_volume = log(Volume))

can_gas |>
  autoplot(log_volume) +
  labs(
    title = "Canadian Natural Gas Production (Log)",
    x = NULL,
    y = "Log of Production Volume"
  ) +
  theme_minimal()

#Answer to 3.1b: The shape became more curved, and seasonal swings became more consistant. The log didn't make the series fully linear because you can still sees changes in growth rate at the same periods as the original plot. I think it helped with variance but maybe has some other factors that logging can't fully solve. 

#3.1c
retail_nsa <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales))

#level
retail_nsa |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail Sales (NSA)",
    x = NULL,
    y = "Dollars (Millions)"
  ) +
  theme_minimal()

#log
retail_nsa |>
  autoplot(log_sales) +
  labs(
    title = "U.S. Retail Sales (NSA, Log)",
    x = NULL,
    y = "Log of Sales"
  ) +
  theme_minimal()

#Answer to 3.1c:i think the case is stronger because the logged version seems to make the season swings seen in the level plot more consttant overtime.

#3.2

#3.2a:
can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

can_gas |>
  autoplot(growth_direct) +
  labs(
    title = "Monthly Growth Rate of Canadian Gas Production",
    x = NULL,
    y = "Growth Rate (%)"
  ) +
  theme_minimal()

#other approach
can_gas <- can_gas |>
  mutate(
    growth_gap = abs(growth_direct - growth_logdiff)
  )

can_gas |>
  autoplot(growth_gap) +
  labs(
    title = "Difference Between Growth Rate Calculations",
    x = NULL,
    y = "Absolute Difference (Percentage Points)"
  ) +
  theme_minimal()

#Anwser to 3.2a: Mostly accurate, but there are certain months especially around January that still less accurate due to bigger changes in production.

#3.3

retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_ts |>
  autoplot(growth_mom) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL
  ) +
  theme_minimal()

#Answer to 3.3: Month to month, the NSA is wayyy nosier than the SA series with huger swings and spaced out spikes poiting to seasonal behavior. NSA has not been seasnonall adjusted, while SA has adjusted to removed calender effects, which are effects due to number of days in a month, and general holiday patterns. This is similar to 2.7c, where SA was smoother than NSA, which had generally more flutating. 

# Part 4 ---------------------------

#4.1a
retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Dollars (millions)"
  ) +
  theme_minimal()

#i like how seasonal plots are rainbow :)

#4.1b
retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in SA Retail Sales",
    y = "Dollars (millions)"
  ) +
  theme_minimal()

#Answer to 4.1b: The SA plot is straighter than the NSA plot due to it's adjustments, which bends upwards towards the end and has more pronounced ups and downs. 

#4.1c
can_gas |>
  gg_season(log_volume) +
  labs(
    title = "Seasonal Pattern in Canadian Natural Gas Production",
    y = "Log of Production Volume"
  ) +
  theme_minimal()

#Answer to 4.1c: The lines seem to bend down, revealing that the tails ends such as January and December having higher production most likely due to those being colder months requiring heat, while predicatlby, the summer months such as June, July, and August have the least since the are the warmest.

#4.2
can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)"
  )

#4.3
install.packages("seasonal")

#4.3a
retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()

#Answer to 4.3a: The highest seasonal factor hits at 1.2 or 20% above trend, and I believe that month is meant to be December, which makes sense because of the holiday. The seasonality swings however seem fairly consistant overtime.

#4.3b:
  sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL, y = "Difference (%)"
  ) +
  theme_minimal()

#Answer to 4.3b: It seems to match the official one pretty fairly, staying within -0.5 to 0.5 in differences. The exception being a few years after 2020, which makes sense since sales were affected during the pandemic and years after due to finicial difficulties. 

#4.4

#4.4a
can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))

report(can_gas_fit)

#Series: log_volume 
#Model: TSLM 

#Residuals:
#Min        1Q    Median        3Q       Max 
#-0.985307 -0.096451 -0.004525  0.131639  0.470564 

#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     1.150e+00  4.142e-02  27.761  < 2e-16 ***
#trend()         3.973e-03  6.886e-05  57.692  < 2e-16 ***
#season()year2  -1.121e-01  5.229e-02  -2.144  0.03245 *  
#season()year3  -5.238e-02  5.258e-02  -0.996  0.31963    
#season()year4  -1.334e-01  5.258e-02  -2.537  0.01146 *  
#season()year5  -1.738e-01  5.258e-02  -3.305  0.00101 ** 
#season()year6  -2.512e-01  5.258e-02  -4.777 2.30e-06 ***
#season()year7  -2.300e-01  5.258e-02  -4.374 1.47e-05 ***
#season()year8  -2.193e-01  5.258e-02  -4.170 3.56e-05 ***
#season()year9  -2.274e-01  5.258e-02  -4.325 1.83e-05 ***
#season()year10 -1.253e-01  5.258e-02  -2.383  0.01753 *  
#season()year11 -7.781e-02  5.258e-02  -1.480  0.13956    
#season()year12  7.202e-03  5.258e-02   0.137  0.89111    
---
  #Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  #Residual standard error: 0.2508 on 529 degrees of freedom
  #Multiple R-squared: 0.8652,	Adjusted R-squared: 0.8622
  #F-statistic:   283 on 12 and 529 DF, p-value: < 2.22e-16

  can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)

#|term           | estimate| std.error| statistic| p.value|
#|:--------------|--------:|---------:|---------:|-------:|
#|(Intercept)    |    1.150|     0.041|    27.761|   0.000|
#|trend()        |    0.004|     0.000|    57.692|   0.000|
#|season()year2  |   -0.112|     0.052|    -2.144|   0.032|
#|season()year3  |   -0.052|     0.053|    -0.996|   0.320|
#|season()year4  |   -0.133|     0.053|    -2.537|   0.011|
#|season()year5  |   -0.174|     0.053|    -3.305|   0.001|
#|season()year6  |   -0.251|     0.053|    -4.777|   0.000|
#|season()year7  |   -0.230|     0.053|    -4.374|   0.000|
#|season()year8  |   -0.219|     0.053|    -4.170|   0.000|
#|season()year9  |   -0.227|     0.053|    -4.325|   0.000|
#|season()year10 |   -0.125|     0.053|    -2.383|   0.018|
#|season()year11 |   -0.078|     0.053|    -1.480|   0.140|
#|season()year12 |    0.007|     0.053|     0.137|   0.891|
  
#Answer to 4.4a: Coefficent is 0.003973 multipled by 100 because of LOG = 0.3973% growth rate for gas. 
#June with 4.777 has the most significant seasonal effect.

#4.4b
can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
       y = "log(Volume)", color = NULL) +
  theme_minimal()

#Answer to 4.4b: The actual is curved, so the fitted line misses most of the dara, only capturing the center and tail end for the most part.  

#4.4c
can_gas_fit |> gg_tsresiduals()

can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)

# A tibble: 1 × 3
#.model                                lb_stat lb_pvalue
#<chr>                                   <dbl>     <dbl>
  #1 TSLM(log_volume ~ trend() + season())   7774.         0

#Answer to 4.4c: Resisudals do not look like just white noise, there is visible structure in the lags.
# With a reported p-value of 0, which is < the significance level 0.05, we reject the null hypothesis menaing residuals still have autocorrelation. 

# Part 5 ---------------------------

#5.1a
gas_cv_fc <- can_gas |>
  select(Month, Volume) |>
  stretch_tsibble(.init = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))

# A tibble: 2 × 4
#.model .type   MAE  RMSE
#<chr>  <chr> <dbl> <dbl>
#1 snaive Test  0.562 0.702
#2 tslm   Test  4.01  4.36 

#Answer to 5.1a: The first forecast evaluated was in January, 1980, and the last forecast evaluated was in Feburary, 2005 (a month after I was born!).
#The seasonal random walk had MAE = 0.562 and RMSE = 0.702, while the linear trend + dummies model had MAE = 4.01 and RMSE = 4.36. 
#Seasonal random walk has smaller forecast errors, so both metric show SNAIVE was more accurate.

#5.2a
errs <- gas_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(can_gas) |> select(Month, actual = Volume),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e) |>
  na.omit()

forecast::dm.test(errs$snaive, errs$tslm, h = 1, power = 1)

#Diebold-Mariano Test

#data:  errs$snaiveerrs$tslm
#DM = -31.278, Forecast horizon = 1, Loss function power = 1, p-value < 2.2e-16
#alternative hypothesis: two.sided

#Answer to 5.2a: The DM statistic is -31.278, and the p-value is very small, under 0. 
# The p-value is < the significance level of 0.05, thus we reject the null hypothesis the both models have equal forecast accuracy.
# -31.278 means that the SNAIVE has smaller forecast errors than the tslm

#5.3a
gas_roll_fc <- can_gas |>
  select(Month, Volume) |>
  slide_tsibble(.size = 240, .step = 1) |>
  model(
    snaive = SNAIVE(Volume),
    tslm = TSLM(log(Volume) ~ trend() + season())
  ) |>
  forecast(h = 1)

gas_roll_fc |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))

# A tibble: 2 × 4
#.model .type   MAE  RMSE
#<chr>  <chr> <dbl> <dbl>
#1 snaive Test  0.562 0.702
#2 tslm   Test  2.09  2.42 

#Answer to 5.3a: The seasonal random walk had MAE = 0.562 and RMSE = 0.702 (same as 5.1a), while the linear trend + dummies model had MAE = 2.09 and RMSE = 2.42. 
#TSLM changed while SNAIVE remained the same, showing that likely TSLM benefits from more recent data while older data used in expanding windows bogged it down.
#Comparing this to the fitted values in 4.4b, tslm struggles with capturing trends within a whole time period, and seems to perform better with recent data.