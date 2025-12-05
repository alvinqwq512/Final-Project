data clean
================
Huiyi Zhu
2025-12-05

## prepare

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.1     ✔ stringr   1.5.2
    ## ✔ ggplot2   4.0.0     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

## data clean

``` r
parkevent = read.csv("Parks_Special_Events_20251204.csv") |>
  janitor::clean_names() |>
  mutate(
    datetime = mdy_hms(date_and_time),
    year  = year(datetime),
    month = month(datetime, label = TRUE, abbr = TRUE),
    day = day(datetime),
    time = format(datetime, "%H:%M:%S"),
    season = case_when(
      month %in% c("Mar","Apr","May") ~ "Spring",
      month %in% c("Jun","Jul","Aug") ~ "Summer",
      month %in% c("Sep","Oct","Nov") ~ "Fall",
      month %in% c("Dec","Jan","Feb") ~ "Winter")) |> 
  select(
    -unit,
    -group_name_partner,
    -date_and_time,
    -location_type,
    -event_name,
    -classification,
    -source) |> 
  select(
    datetime, year, month, day, time, season,
    everything())
```
