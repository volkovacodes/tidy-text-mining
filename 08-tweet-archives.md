# Case study: comparing Twitter archives {#twitter}



One type of text that has received its share of attention in recent years is text shared online via Twitter. In fact, several of the sentiment lexicons used in this book (and commonly used in general) were designed for use with and validated on tweets. Both of the authors of this book are on Twitter and are fairly regular users of it so in this case study, let's compare the entire Twitter archives of [Julia](https://twitter.com/juliasilge) and [David](https://twitter.com/drob).

## Getting the data and distribution of tweets

An individual can download their own Twitter archive by following [directions available here](https://support.twitter.com/articles/20170160). We each downloaded ours and will now open them up. Let's use the lubridate package to convert the string timestamps to date-time objects and initially take a look at our tweeting patterns overall.


```r
library(lubridate)
library(ggplot2)
library(dplyr)

tweets_julia <- read.csv("data/tweets_julia.csv", stringsAsFactors = FALSE)
tweets_dave <- read.csv("data/tweets_dave.csv", stringsAsFactors = FALSE)
# take out the timezone thing if we don't do anything with times of day
tweets_julia$timestamp <- with_tz(ymd_hms(tweets_julia$timestamp), 
                                  "America/Denver")
tweets_dave$timestamp <- with_tz(ymd_hms(tweets_dave$timestamp), 
                                 "America/New_York")
tweets <- bind_rows(tweets_julia %>% 
                      mutate(person = "Julia"),
                    tweets_dave %>% 
                      mutate(person = "David"))

ggplot(tweets, aes(x = timestamp, fill = person)) +
  geom_histogram(alpha = 0.5, position = "identity")
```

<img src="08-tweet-archives_files/figure-html/setup-1.png" width="768" />

David and Julia tweet at about the same rate currently and joined Twitter about a year apart from each other, but there about 5 years where David was not active on Twitter and Julia was. In total, Julia has about 4 times as many tweets as David.

## Word frequencies

Let's use `unnest_tokens` to make a tidy dataframe of all the words in our tweets, and remove the common English stop words. There are certain conventions in how people use text on Twitter, so we will do a bit more work with our text here than, for example, we did with the narrative text from Project Gutenberg. The first `mutate` line below removes links and cleans out some characters that we don't want. In the call to `unnest_tokens`, we unnest using a regex pattern, instead of just looking for single unigrams (words). This regex pattern is very useful for dealing with Twitter text; it retains hashtags and mentions of usernames with the `@` symbol. Because we have kept these types of symbols in the text, we can't use a simple `anti_join` to remove stop words. Instead, we can take the approach shown in the `filter` line that uses `str_detect` from the stringr library.


```r
library(tidytext)
library(stringr)

reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets <- tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))
```

Now we can calculate word frequencies for each person


```r
frequency <- tidy_tweets %>% 
  group_by(person) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_tweets %>% 
              group_by(person) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency
```

```
## Source: local data frame [23,086 x 5]
## Groups: person [2]
## 
##    person           word     n total        freq
##     <chr>          <chr> <int> <int>       <dbl>
## 1   Julia           time   567 76504 0.007411377
## 2   Julia    @selkie1970   565 76504 0.007385235
## 3   Julia       @skedman   518 76504 0.006770888
## 4   Julia            day   470 76504 0.006143470
## 5   Julia           baby   410 76504 0.005359197
## 6   David        #rstats   359 22074 0.016263477
## 7   Julia     @doctormac   342 76504 0.004470354
## 8   David @hadleywickham   306 22074 0.013862463
## 9   Julia           love   303 76504 0.003960577
## 10  Julia   @haleynburke   291 76504 0.003803723
## # ... with 23,076 more rows
```

This is a lovely, tidy data frame but we would actually like to plot those frequencies on the x- and y-axes of a plot, so we will need to use an `inner_join` and make a different dataframe.


```r
frequency <- inner_join(frequency %>% 
                          filter(person == "Julia") %>% 
                          rename(Julia = freq),
                        frequency %>% 
                          filter(person == "David") %>% 
                          rename(David = freq),
                        by = "word") %>% 
  ungroup() %>% 
  select(word, Julia, David)

frequency
```

```
## # A tibble: 3,520 × 3
##       word       Julia        David
##      <chr>       <dbl>        <dbl>
## 1     time 0.007411377 0.0043490079
## 2      day 0.006143470 0.0014949715
## 3     baby 0.005359197 0.0001812087
## 4     love 0.003960577 0.0020385974
## 5    house 0.003790651 0.0001359065
## 6  morning 0.003659939 0.0004077195
## 7   people 0.003372373 0.0033523602
## 8     feel 0.003124020 0.0013137628
## 9   pretty 0.002954094 0.0010872520
## 10  school 0.002875667 0.0002265108
## # ... with 3,510 more rows
```

```r
library(scales)
ggplot(frequency, aes(Julia, David)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")
```

<img src="08-tweet-archives_files/figure-html/spread-1.png" width="672" />


This may not even need to be pointed out, but David and Julia have used their Twitter accounts rather differently over the course of the past several years. David has used his Twitter account almost exclusively for professional purposes since he became more active, while Julia used it for entirely personal purposes until late 2015. We see these differences immediately in this plot exploring word frequencies, and they will continue to be obvious in the rest of this chapter. Words near the red line in this plot are used with about equal frequencies by David and Julia, while words far away from the line are used much more by one person compared to the other. Because of the inner join we did above, words, hashtags, and usernames that appear in this plot are ones that we have both used at least once.

## Comparing word usage 

We just made a plot comparing raw word frequencies, but now let's find which words are more or less likely to come from each person's account using the log odds ratio. First, let's use `str_detect` to remove Twitter usernames from the `word` column, because otherwise, the results here are dominated only by people who Julia or David know and the other does not. After removing these, we count how many times each person uses each word and keep only the words used more than 5 times. After a `spread` operation, we can calculate the log odds ratio for each word, using




where $n$ is the number of times the word in question is used by each person and the total indicates the total words for each person.


```r
library(tidyr)
word_ratios <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, person) %>%
  filter(sum(n) >= 5) %>%
  spread(person, n, fill = 0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -word) %>%
  mutate(logratio = log(David / Julia)) %>%
  arrange(desc(logratio))
```

What are some words that are about equally likely to come from David or Julia's account?


```r
word_ratios %>% 
  arrange(abs(logratio))
```

```
## # A tibble: 3,483 × 4
##           word        David        Julia    logratio
##          <chr>        <dbl>        <dbl>       <dbl>
## 1        alive 0.0001932118 0.0001920358 0.006105552
## 2       fallen 0.0001932118 0.0001920358 0.006105552
## 3        focus 0.0001932118 0.0001920358 0.006105552
## 4    forgotten 0.0001932118 0.0001920358 0.006105552
## 5  information 0.0001932118 0.0001920358 0.006105552
## 6         nice 0.0023185419 0.0023044290 0.006105552
## 7     painting 0.0001932118 0.0001920358 0.006105552
## 8        phone 0.0007728473 0.0007681430 0.006105552
## 9       random 0.0001932118 0.0001920358 0.006105552
## 10      system 0.0003864236 0.0003840715 0.006105552
## # ... with 3,473 more rows
```

Which words are most likely to be from Julia's account or from David's account? Let's just take the top 15 most distinctive words for each account and plot them.


```r
word_ratios %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_bar(alpha = 0.8, stat = "identity") +
  coord_flip() +
  ylab("log odds ratio (David/Julia)") +
  scale_fill_discrete(name = "", labels = c("David", "Julia"))
```

<img src="08-tweet-archives_files/figure-html/plotratios-1.png" width="576" />

So David has tweeted about bioinformatics and Stack Overflow while Julia has been tweeting about preschool, naps, and the snow.

## Sentiment analysis

## Words that contribute to sentiment in tweets
