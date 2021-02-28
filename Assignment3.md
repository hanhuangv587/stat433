Assignement 3
================
Yuhan Wang
2021/2/27

``` r
#load require package
library(nycflights13)
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## √ ggplot2 3.3.3     √ purrr   0.3.4
    ## √ tibble  3.0.6     √ dplyr   1.0.4
    ## √ tidyr   1.1.2     √ stringr 1.4.0
    ## √ readr   1.4.0     √ forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(ggplot2)
library(tidyr)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

Introduction: after summarized the dataset, I have following results: 1.
Weekends have less canceled flights and delay departure time compared
with weekdays, especially for EWR and LGA airport. 2. Among big
airlines(which has more than 10000 recording in the dataset), Delta
Airline has the lowest cancel rate and relatively shorter delay time. 3.
Morning have less canceled flights ratio and delay departure time
compared with afternoon and evening.

Following three section shows each finding

# 1. choose weekend especially your origin is EWR or LGA

``` r
# creat week variable
flights_new <-  flights %>% 
  mutate(departure = make_datetime(year, month, day, hour, minute),
         dayWeek = wday(departure),
         dayWeek = ifelse(dayWeek==1,'Sun',ifelse(dayWeek==2,'Mon',ifelse(dayWeek==3,'Tue',
                   ifelse(dayWeek==4,'Wed',ifelse(dayWeek==5,'Thu',ifelse(dayWeek==6,'Fri',
                   ifelse(dayWeek==7,'Sat','Missing')))))))) 

flights_new$dayWeek <- factor(flights_new$dayWeek, levels = c('Mon','Tue','Wed','Thu','Fri','Sat','Sun'))
flights_new$origin <- factor(flights_new$origin, levels = c('LGA','EWR','JFK'))

# look at the number of canceled flights across weekday
flights_new %>% group_by(dayWeek) %>%
  summarise(num_cancel = sum(is.na(dep_delay)),
            per_cancel = num_cancel/n(),
            ave_delay = mean(dep_delay,na.rm=T))
```

    ## # A tibble: 7 x 4
    ##   dayWeek num_cancel per_cancel ave_delay
    ## * <fct>        <int>      <dbl>     <dbl>
    ## 1 Mon           1220     0.0241     14.8 
    ## 2 Tue           1152     0.0228     10.6 
    ## 3 Wed           1202     0.0240     11.8 
    ## 4 Thu           1562     0.0311     16.1 
    ## 5 Fri           1608     0.0320     14.7 
    ## 6 Sat            797     0.0206      7.65
    ## 7 Sun            714     0.0154     11.6

``` r
#plot the number of cancelled flights by weekday and origin airports
flights_new %>% 
  filter(is.na(dep_delay)) %>% 
  ggplot(aes(x=dayWeek,fill = origin)) +
  geom_bar(position=position_dodge())
```

![](Assignment3_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

# 2. choose Delta Airline if possible

``` r
table(flights$carrier)
```

    ## 
    ##    9E    AA    AS    B6    DL    EV    F9    FL    HA    MQ    OO    UA    US 
    ## 18460 32729   714 54635 48110 54173   685  3260   342 26397    32 58665 20536 
    ##    VX    WN    YV 
    ##  5162 12275   601

``` r
# take airline that has more than 10000 flights records
big_airline_list <- c('9E','AA','B6','DL','EV','MQ','UA','US','WN')

flights2 <- flights %>%
  left_join(airlines, by = "carrier")

# look at the number of canceled flights across airlines
flights2 %>% filter(carrier %in% big_airline_list) %>% group_by(name) %>%
  summarise(num_cancel = sum(is.na(dep_delay)),
            per_cancel = num_cancel/n(),
            ave_delay = IQR(dep_delay,na.rm=T)) %>% 
  arrange(per_cancel)
```

    ## # A tibble: 9 x 4
    ##   name                     num_cancel per_cancel ave_delay
    ##   <chr>                         <int>      <dbl>     <dbl>
    ## 1 Delta Air Lines Inc.            349    0.00725        10
    ## 2 JetBlue Airways                 466    0.00853        17
    ## 3 United Air Lines Inc.           686    0.0117         15
    ## 4 Southwest Airlines Co.          192    0.0156         19
    ## 5 American Airlines Inc.          636    0.0194         10
    ## 6 US Airways Inc.                 663    0.0323          7
    ## 7 Envoy Air                      1234    0.0467         16
    ## 8 ExpressJet Airlines Inc.       2817    0.0520         30
    ## 9 Endeavor Air Inc.              1044    0.0566         23

``` r
# make a density plot dep_time across big airlines

flights_big <- flights2 %>% 
  filter(carrier %in% big_airline_list) 

flights_big %>%
 filter(dep_delay >= -43L & dep_delay <= 119L | is.na(dep_delay)) %>%
 ggplot() +
 aes(x = dep_delay, fill = name) +
 geom_density(adjust = 1L) +
 scale_fill_hue() +
 theme_minimal() +
 facet_wrap(vars(name))
```

![](Assignment3_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

# 3. choose morning to take your flights

``` r
# creat time period variable
flights3 <- flights %>% 
  mutate(time_period = ifelse(hour<=12,'morning',
                       ifelse(hour>12 & hour<19,'afternoon',
                       ifelse(hour>=19,'evening','missing'))))

flights3$time_period <- factor(flights3$time_period, levels = c('morning','afternoon','evening'))

# look at the number of canceled flights across time periods
flights3 %>% group_by(time_period) %>%
  summarise(num_cancel = sum(is.na(dep_delay)),
            per_cancel = num_cancel/n(),
            ave_delay = mean(dep_delay,na.rm=T)) %>% 
  arrange(per_cancel)
```

    ## # A tibble: 3 x 4
    ##   time_period num_cancel per_cancel ave_delay
    ##   <fct>            <int>      <dbl>     <dbl>
    ## 1 morning           2467     0.0165      4.51
    ## 2 afternoon         3791     0.0281     17.3 
    ## 3 evening           1997     0.0378     24.0

``` r
# show the violin polt of delay time across time period
flights3 %>% 
  filter(!is.na(dep_delay)) %>% 
  ggplot(aes(x=time_period,y = dep_delay, fill = time_period)) +
  geom_violin() + ylim(-30,100)
```

![](Assignment3_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
