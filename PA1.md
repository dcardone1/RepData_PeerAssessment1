---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

``` r
data <- read.csv('activity.csv')
```


``` r
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## What is mean total number of steps taken per day?

``` r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 4.5.2
```

```
## 
## Adjuntando el paquete: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```


``` r
proc_data <- data %>% filter(!is.na(steps)) %>%
    mutate(date = as.Date(date, "%Y-%m-%d")) %>% 
    group_by(date) %>% summarise(total_steps = sum(steps), avg_steps = mean(steps), median_steps= median(steps))

    
proc_data
```

```
## # A tibble: 53 × 4
##    date       total_steps avg_steps median_steps
##    <date>           <int>     <dbl>        <dbl>
##  1 2012-10-02         126     0.438            0
##  2 2012-10-03       11352    39.4              0
##  3 2012-10-04       12116    42.1              0
##  4 2012-10-05       13294    46.2              0
##  5 2012-10-06       15420    53.5              0
##  6 2012-10-07       11015    38.2              0
##  7 2012-10-09       12811    44.5              0
##  8 2012-10-10        9900    34.4              0
##  9 2012-10-11       10304    35.8              0
## 10 2012-10-12       17382    60.4              0
## # ℹ 43 more rows
```

``` r
library(ggplot2)
```


``` r
g <- ggplot(data[!is.na(data$steps),], aes(steps))
g + geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

## What is the average daily activity pattern?

``` r
daily_pattern <- data %>% filter(!is.na(steps)) %>%
    mutate(date = as.Date(date, "%Y-%m-%d")) %>% 
    group_by(interval) %>% summarise(avg_steps = mean(steps))

    
head(daily_pattern, 20)
```

```
## # A tibble: 20 × 2
##    interval avg_steps
##       <int>     <dbl>
##  1        0    1.72  
##  2        5    0.340 
##  3       10    0.132 
##  4       15    0.151 
##  5       20    0.0755
##  6       25    2.09  
##  7       30    0.528 
##  8       35    0.868 
##  9       40    0     
## 10       45    1.47  
## 11       50    0.302 
## 12       55    0.132 
## 13      100    0.321 
## 14      105    0.679 
## 15      110    0.151 
## 16      115    0.340 
## 17      120    0     
## 18      125    1.11  
## 19      130    1.83  
## 20      135    0.170
```


## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
