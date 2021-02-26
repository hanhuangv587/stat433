In-class exercise
================
Yuhan Wang
2021/2/26

## load require package

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)

# install.packages("nycflights13")
library(nycflights13)
library(ggplot2)
library(lubridate) 
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

# 1. Make a table that describes each plane. It should have a column for tailnum, another column for average arrival delay, and another for the year the plane was manufactured.

``` r
flights %>% 
  group_by(tailnum) %>% 
  summarise(mean_arr_delay = mean(arr_delay, na.rm = T)) %>% 
  left_join(planes) %>% 
  select(tailnum:year)
```

    ## Joining, by = "tailnum"

    ## # A tibble: 4,044 x 3
    ##    tailnum mean_arr_delay  year
    ##    <chr>            <dbl> <int>
    ##  1 D942DN          31.5      NA
    ##  2 N0EGMQ           9.98     NA
    ##  3 N10156          12.7    2004
    ##  4 N102UW           2.94   1998
    ##  5 N103US          -6.93   1999
    ##  6 N104UW           1.80   1999
    ##  7 N10575          20.7    2002
    ##  8 N105UW          -0.267  1999
    ##  9 N107US          -5.73   1999
    ## 10 N108UW          -1.25   1999
    ## # ... with 4,034 more rows

# 2. Make a table where each row is a day of the year. The first column is the date. The 2:4 columns give the number of (scheduled) departures from EWR, LGA, and JFK.

``` r
flights %>% 
  mutate(date = as.Date(time_hour)) %>% 
  group_by(origin,date) %>% 
  summarise(deps = n()) %>% 
  pivot_wider(names_from = origin, values_from = deps)
```

    ## `summarise()` has grouped output by 'origin'. You can override using the `.groups` argument.

    ## # A tibble: 366 x 4
    ##    date         EWR   JFK   LGA
    ##    <date>     <int> <int> <int>
    ##  1 2013-01-01   255   236   218
    ##  2 2013-01-02   351   319   260
    ##  3 2013-01-03   336   320   261
    ##  4 2013-01-04   340   319   258
    ##  5 2013-01-05   262   303   203
    ##  6 2013-01-06   272   309   203
    ##  7 2013-01-07   348   307   277
    ##  8 2013-01-08   336   291   276
    ##  9 2013-01-09   336   289   279
    ## 10 2013-01-10   341   302   282
    ## # ... with 356 more rows

# 3. Make a table where each row is a day of the year. Each destination airport is a column. The elements (day x destination) give the number of flights to that destination. What should NA’s be?

``` r
flights %>% 
  mutate(date = as.Date(time_hour)) %>% 
  group_by(dest,date) %>% 
  summarise(deps = n()) %>% 
  pivot_wider(names_from = dest, values_from = deps, values_fill = 0)
```

    ## `summarise()` has grouped output by 'dest'. You can override using the `.groups` argument.

    ## # A tibble: 366 x 106
    ##    date         ABQ   ACK   ALB   ANC   ATL   AUS   AVL   BDL   BGR   BHM   BNA
    ##    <date>     <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ##  1 2013-04-22     1     0     0     0    54     7     0     2     1     1    18
    ##  2 2013-04-24     1     0     0     0    53     6     0     2     1     1    18
    ##  3 2013-04-25     1     0     0     0    53     7     0     2     1     1    18
    ##  4 2013-04-26     1     0     0     0    53     7     0     2     1     1    18
    ##  5 2013-04-27     1     0     0     0    40     6     0     2     2     0    15
    ##  6 2013-04-28     1     0     0     0    46     5     1     2     2     1    15
    ##  7 2013-04-29     1     0     0     0    54     7     0     2     1     1    18
    ##  8 2013-04-30     1     0     0     0    53     7     0     2     1     1    18
    ##  9 2013-05-01     1     0     1     0    51     8     1     2     1     1    19
    ## 10 2013-05-02     1     0     2     0    50     6     1     2     1     1    19
    ## # ... with 356 more rows, and 94 more variables: BOS <int>, BQN <int>,
    ## #   BTV <int>, BUF <int>, BUR <int>, BWI <int>, BZN <int>, CAE <int>,
    ## #   CAK <int>, CHO <int>, CHS <int>, CLE <int>, CLT <int>, CMH <int>,
    ## #   CRW <int>, CVG <int>, DAY <int>, DCA <int>, DEN <int>, DFW <int>,
    ## #   DSM <int>, DTW <int>, EGE <int>, EYW <int>, FLL <int>, GRR <int>,
    ## #   GSO <int>, GSP <int>, HDN <int>, HNL <int>, HOU <int>, IAD <int>,
    ## #   IAH <int>, ILM <int>, IND <int>, JAC <int>, JAX <int>, LAS <int>,
    ## #   LAX <int>, LEX <int>, LGA <int>, LGB <int>, MCI <int>, MCO <int>,
    ## #   MDW <int>, MEM <int>, MHT <int>, MIA <int>, MKE <int>, MSN <int>,
    ## #   MSP <int>, MSY <int>, MTJ <int>, MVY <int>, MYR <int>, OAK <int>,
    ## #   OKC <int>, OMA <int>, ORD <int>, ORF <int>, PBI <int>, PDX <int>,
    ## #   PHL <int>, PHX <int>, PIT <int>, PSE <int>, PSP <int>, PVD <int>,
    ## #   PWM <int>, RDU <int>, RIC <int>, ROC <int>, RSW <int>, SAN <int>,
    ## #   SAT <int>, SAV <int>, SBN <int>, SDF <int>, SEA <int>, SFO <int>,
    ## #   SJC <int>, SJU <int>, SLC <int>, SMF <int>, SNA <int>, SRQ <int>,
    ## #   STL <int>, STT <int>, SYR <int>, TPA <int>, TUL <int>, TVC <int>,
    ## #   TYS <int>, XNA <int>
