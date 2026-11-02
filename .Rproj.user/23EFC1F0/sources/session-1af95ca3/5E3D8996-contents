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


# Part 4 ---------------------------


# Part 5 ---------------------------