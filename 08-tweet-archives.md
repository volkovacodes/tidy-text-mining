# Case study: comparing Twitter archives {#twitter}

One type of text that has gotten a good deal of attention in recent years is text shared online via Twitter. In fact, several of the sentiment lexicons used in this book (and commonly used in general) were designed for use with and validated on tweets. Both of the authors of this book are on Twitter and are fairly regular users of it so in this case study, let's compare the entire Twitter archives of Julia and David.

## Getting the data and distribution of tweets

An individual can download their own Twitter archive by following [directions available here](https://support.twitter.com/articles/20170160). We each downloaded ours and will now open them up. Let's use lubridate to convert the string timestamps to date-time objects and just take a look at our tweeting patterns overall.


```r
library(lubridate)
```

```
## Loading required package: methods
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(ggplot2)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
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

```r
tweets_julia <- read.csv("data/tweets_julia.csv", stringsAsFactors = FALSE)
tweets_dave <- read.csv("data/tweets_dave.csv", stringsAsFactors = FALSE)
tweets_julia$timestamp <- with_tz(ymd_hms(tweets_julia$timestamp), 
                                  "America/Denver")
tweets_dave$timestamp <- with_tz(ymd_hms(tweets_dave$timestamp), 
                                 "America/New_York")
tweets <- bind_rows(tweets_julia %>% mutate(person = "Julia"),
                    tweets_dave %>% mutate(person = "David"))
ggplot(tweets, aes(x = timestamp, fill = person)) +
  geom_histogram(alpha = 0.5, position = "identity")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="08-tweet-archives_files/figure-html/setup-1.png" width="768" />
David and Julia tweet at about the same rate currently and joined Twitter about a year apart from each other, but there good 5 or so years where David was not active on Twitter and Julia was. In total, Julia has about 4 times as many tweets as David.

## Word frequencies

Let's use `unnest_tokens` to make a tidy dataframe of all the words in our tweets, and remove the common English stop words.


```r
library(tidytext)
tidy_tweets <- tweets %>% unnest_tokens(word, text) %>% anti_join(stop_words)
```

```
## Joining, by = "word"
```

Now we can calculate word frequencies for each person


```r
frequency <- tidy_tweets %>% group_by(person) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_tweets %>% group_by(person) %>% summarise(total = n())) %>%
  mutate(freq = n/total)
```

```
## Joining, by = "person"
```

```r
frequency
```

```
## Source: local data frame [25,210 x 5]
## Groups: person [2]
## 
##    person       word     n total        freq
##     <chr>      <chr> <int> <int>       <dbl>
## 1   Julia       t.co  2218 85215 0.026028281
## 2   Julia       http  1438 85215 0.016874963
## 3   David       t.co  1344 27338 0.049162338
## 4   David      https  1045 27338 0.038225181
## 5   Julia      https  1040 85215 0.012204424
## 6   David         rt   909 27338 0.033250421
## 7   Julia         rt   867 85215 0.010174265
## 8   Julia       time   568 85215 0.006665493
## 9   Julia selkie1970   565 85215 0.006630288
## 10  Julia    skedman   518 85215 0.006078742
## # ... with 25,200 more rows
```

This is a lovely, tidy data frame but we would actually like to plot those frequencies on the x- and y-axes of a plot, so we will need to use an `inner_join` and make a different dataframe.


```r
frequency <- inner_join(frequency %>% filter(person == "Julia") %>% rename(Julia = freq),
                        frequency %>% filter(person == "David") %>% rename(David = freq),
                        by = "word") %>% 
  ungroup() %>% select(word, Julia, David)
frequency
```

```
## # A tibble: 3,674 x 3
##     word       Julia        David
##    <chr>       <dbl>        <dbl>
## 1   t.co 0.026028281 0.0491623381
## 2   http 0.016874963 0.0150340186
## 3  https 0.012204424 0.0382251811
## 4     rt 0.010174265 0.0332504207
## 5   time 0.006665493 0.0035115956
## 6    day 0.005515461 0.0012071110
## 7   baby 0.004811360 0.0001463165
## 8   love 0.003555712 0.0017192187
## 9  house 0.003403157 0.0001097374
## 10   amp 0.003356217 0.0015363231
## # ... with 3,664 more rows
```

```r
library(scales)
ggplot(frequency, aes(Julia, David)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.4, height = 0.4) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")
```

<img src="08-tweet-archives_files/figure-html/spread-1.png" width="672" />


It is at this point that we should perhaps share some personal details. Although the authors of this book enjoy working together very much, it could be argued that their lives are quite different. To start with, David is about 10 years younger than Julia. Also, Julia has three children, two of who were born during the years she has been active on Twitter; we could find the tweets where she complained about being very pregnant or announced their births if we wanted to. David has used his Twitter account almost exclusively for professional purposes since he became more active, while Julia used it for entirely personal purposes until late 2015. We see these differences immediately in this plot exploring word frequencies, and they will continue to be obvious.

TODO: lots
