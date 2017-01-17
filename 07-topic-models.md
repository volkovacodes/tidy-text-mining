# Topic modeling {#topicmodeling}



Topic modeling is a method for unsupervised classification of documents, by modeling each document as a mixture of topics and each topic as a mixture of words. [Latent Dirichlet allocation](https://en.wikipedia.org/wiki/Latent_Dirichlet_allocation) is a particularly popular method for fitting a topic model.

We can use tidy text principles, as described in [Chapter 2](#tidytext), to approach topic modeling using consistent and effective tools. In particular, we'll be using tidying functions for LDA objects from the [topicmodels package](https://cran.r-project.org/package=topicmodels).

## The great library heist

Suppose a vandal has broken into your study and torn apart four of your books:

* *Great Expectations* by Charles Dickens
* *The War of the Worlds* by H.G. Wells
* *Twenty Thousand Leagues Under the Sea* by Jules Verne
* *Pride and Prejudice* by Jane Austen

This vandal has torn the books into individual chapters, and left them in one large pile. How can we restore these disorganized chapters to their original books? This is a challenging problem since the individual chapters are **unlabeled**: we don't know what words might distinguish them into groups. We'll thus use topic modeling to discover how chapters cluster into distinct topics, each of them representing one of the words.

We'll retrieve the text of these four books using the gutenbergr package:


```r
library(dplyr)

titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
            "Pride and Prejudice", "Great Expectations")
```


```r
library(gutenbergr)

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")
```



As pre-processing, we divide these into chapters, use tidytext's `unnest_tokens` to separate them into words, then remove `stop_words`, similar to the analyses we've done in Chapter \@ref{sentiment}. We're treating every chapter as a separate "document", each with a name like `Great Expectations_1` or `Pride and Prejudice_11`. (In practice, each document might be one newspaper article, or one blog post).


```r
library(tidytext)
library(stringr)
library(tidyr)

# Divide into documents, each representing one chapter
by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

# Split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

# Find document-word counts
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()

word_counts
```

```
## # A tibble: 104,721 × 3
##                    document    word     n
##                       <chr>   <chr> <int>
## 1     Great Expectations_57     joe    88
## 2      Great Expectations_7     joe    70
## 3     Great Expectations_17   biddy    63
## 4     Great Expectations_27     joe    58
## 5     Great Expectations_38 estella    58
## 6      Great Expectations_2     joe    56
## 7     Great Expectations_23  pocket    53
## 8     Great Expectations_15     joe    50
## 9     Great Expectations_18     joe    50
## 10 The War of the Worlds_16 brother    50
## # ... with 104,711 more rows
```

word_counts

## Latent Dirichlet allocation with the topicmodels package

Right now this data frame is in a tidy form, with one-term-per-document-per-row. However, the topicmodels package requires a `DocumentTermMatrix` (from the tm package). As described in Chapter \@ref{#dtm}, we can cast a one-token-per-row table into a `DocumentTermMatrix` with tidytext's `cast_dtm`:


```r
chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)

chapters_dtm
```

```
## <<DocumentTermMatrix (documents: 193, terms: 18215)>>
## Non-/sparse entries: 104721/3410774
## Sparsity           : 97%
## Maximal term length: 19
## Weighting          : term frequency (tf)
```

We're now ready ready to use the [topicmodels](https://cran.r-project.org/package=topicmodels) package, specifically the `LDA` function, to create a four topic Latent Dirichlet Allocation model. (In this case we know there are four topics because there are four books: in other problems we may need to try a few different values of `k`).


```r
library(topicmodels)

# setting a seed so that the output is predictable
chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))
chapters_lda
```

```
## A LDA_VEM topic model with 4 topics.
```

```r
class(chapters_lda)
```

```
## [1] "LDA_VEM"
## attr(,"package")
## [1] "topicmodels"
```

This produces an LDA model of class `LDA_VEM`, containing the 

The tidytext package now gives us the option of *returning* to a tidy analysis, using the `tidy` and `augment` verbs borrowed from the [broom package](https://github.com/dgrtwo/broom). Note the similarity with the tidying functions in Chapter \@ref{#casting}: each . In particular, we start with the `tidy` verb.


```r
chapters_lda_td <- tidy(chapters_lda)
chapters_lda_td
```

```
## # A tibble: 72,860 × 3
##    topic    term         beta
##    <int>   <chr>        <dbl>
## 1      1     joe 5.830326e-17
## 2      2     joe 3.194447e-57
## 3      3     joe 4.162676e-24
## 4      4     joe 1.445030e-02
## 5      1   biddy 7.846976e-27
## 6      2   biddy 4.672244e-69
## 7      3   biddy 2.259711e-46
## 8      4   biddy 4.767972e-03
## 9      1 estella 3.827272e-06
## 10     2 estella 5.316964e-65
## # ... with 72,850 more rows
```

Notice that this has turned the model into a one-topic-per-term-per-row format. For each combination the model has $\beta$, the probability of that term being generated from that topic.

We could use dplyr's `top_n` to find the top 5 terms within each topic:


```r
top_terms <- chapters_lda_td %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms
```

```
## # A tibble: 20 × 3
##    topic      term        beta
##    <int>     <chr>       <dbl>
## 1      1 elizabeth 0.014107538
## 2      1     darcy 0.008814258
## 3      1      miss 0.008706741
## 4      1    bennet 0.006947431
## 5      1      jane 0.006497512
## 6      2   captain 0.015507696
## 7      2  nautilus 0.013050048
## 8      2       sea 0.008850073
## 9      2      nemo 0.008708397
## 10     2       ned 0.008030799
## 11     3    people 0.006797400
## 12     3  martians 0.006512569
## 13     3      time 0.005347115
## 14     3     black 0.005278302
## 15     3     night 0.004483143
## 16     4       joe 0.014450300
## 17     4      time 0.006847574
## 18     4       pip 0.006817363
## 19     4    looked 0.006365257
## 20     4      miss 0.006228387
```

This model lends itself to a visualization:


```r
library(ggplot2)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
```

<img src="07-topic-models_files/figure-html/top_terms_plot-1.png" width="672" />

These topics are pretty clearly associated with the four books! There's no question that the topic of "nemo", "sea", and "nautilus" belongs to *Twenty Thousand Leagues Under the Sea*, and that "jane", "darcy", and "elizabeth" belongs to *Pride and Prejudice*. We see "pip" and "joe" from *Great Expectations* and "martians", "black", and "night" from *The War of the Worlds*.

## Per-document classification

Each chapter was a "document" in this analysis. Thus, we may want to know which topics are associated with each document. Can we put the chapters back together in the correct books?


```r
chapters_lda_gamma <- tidytext:::tidy.LDA(chapters_lda, matrix = "gamma")
chapters_lda_gamma
```

```
## # A tibble: 772 × 3
##                    document topic        gamma
##                       <chr> <int>        <dbl>
## 1     Great Expectations_57     1 1.351886e-05
## 2      Great Expectations_7     1 1.470726e-05
## 3     Great Expectations_17     1 2.117127e-05
## 4     Great Expectations_27     1 1.919746e-05
## 5     Great Expectations_38     1 3.544403e-01
## 6      Great Expectations_2     1 1.723723e-05
## 7     Great Expectations_23     1 5.507241e-01
## 8     Great Expectations_15     1 1.682503e-02
## 9     Great Expectations_18     1 1.272044e-05
## 10 The War of the Worlds_16     1 1.084337e-05
## # ... with 762 more rows
```

Setting `matrix = "gamma"` returns a tidied version with one-document-per-topic-per-row. Now that we have these document classifiations, we can see how well our unsupervised learning did at distinguishing the four books. First we re-separate the document name into title and chapter:


```r
chapters_lda_gamma <- chapters_lda_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)
chapters_lda_gamma
```

```
## # A tibble: 772 × 4
##                    title chapter topic        gamma
## *                  <chr>   <int> <int>        <dbl>
## 1     Great Expectations      57     1 1.351886e-05
## 2     Great Expectations       7     1 1.470726e-05
## 3     Great Expectations      17     1 2.117127e-05
## 4     Great Expectations      27     1 1.919746e-05
## 5     Great Expectations      38     1 3.544403e-01
## 6     Great Expectations       2     1 1.723723e-05
## 7     Great Expectations      23     1 5.507241e-01
## 8     Great Expectations      15     1 1.682503e-02
## 9     Great Expectations      18     1 1.272044e-05
## 10 The War of the Worlds      16     1 1.084337e-05
## # ... with 762 more rows
```

Then we examine what fraction of chapters we got right for each:


```r
ggplot(chapters_lda_gamma, aes(gamma, fill = factor(topic))) +
  geom_histogram() +
  facet_wrap(~ title, nrow = 2)
```

<img src="07-topic-models_files/figure-html/chapters_lda_gamma_plot-1.png" width="768" />

We notice that almost all of the chapters from *Pride and Prejudice*, *War of the Worlds*, and *Twenty Thousand Leagues Under the Sea* were uniquely identified as a single topic each.


```r
chapter_classifications <- chapters_lda_gamma %>%
  group_by(title, chapter) %>%
  top_n(1, gamma) %>%
  ungroup() %>%
  arrange(gamma)

chapter_classifications
```

```
## # A tibble: 193 × 4
##                 title chapter topic     gamma
##                 <chr>   <int> <int>     <dbl>
## 1  Great Expectations      54     3 0.4803234
## 2  Great Expectations      22     4 0.5356506
## 3  Great Expectations      31     4 0.5464851
## 4  Great Expectations      23     1 0.5507241
## 5  Great Expectations      33     4 0.5700737
## 6  Great Expectations      47     4 0.5802089
## 7  Great Expectations      56     4 0.5984806
## 8  Great Expectations      38     4 0.6455341
## 9  Great Expectations      11     4 0.6689600
## 10 Great Expectations      44     4 0.6777974
## # ... with 183 more rows
```

We can determine this by finding the consensus book for each, which we note is correct based on our earlier visualization:


```r
book_topics <- chapter_classifications %>%
  count(title, topic) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = title, topic)

book_topics
```

```
## # A tibble: 4 × 2
##                               consensus topic
##                                   <chr> <int>
## 1                    Great Expectations     4
## 2                   Pride and Prejudice     1
## 3                 The War of the Worlds     3
## 4 Twenty Thousand Leagues under the Sea     2
```

Then we see which chapters were misidentified:


```r
chapter_classifications %>%
  inner_join(book_topics, by = "topic") %>%
  count(title, consensus)
```

```
## Source: local data frame [6 x 3]
## Groups: title [?]
## 
##                                   title                             consensus     n
##                                   <chr>                                 <chr> <int>
## 1                    Great Expectations                    Great Expectations    57
## 2                    Great Expectations                   Pride and Prejudice     1
## 3                    Great Expectations                 The War of the Worlds     1
## 4                   Pride and Prejudice                   Pride and Prejudice    61
## 5                 The War of the Worlds                 The War of the Worlds    27
## 6 Twenty Thousand Leagues under the Sea Twenty Thousand Leagues under the Sea    46
```

We see that only a few chapters from *Great Expectations* were misclassified. Not bad for unsupervised clustering!

## By word assignments: `augment`

One important step in the topic modeling expectation-maximization algorithm is assigning each word in each document to a topic. The more words in a document are assigned to that topic, generally, the more weight (`gamma`) will go on that document-topic classification.

We may want to take the original document-word pairs and find which words in each document were assigned to which topic. This is the job of the `augment` verb.


```r
assignments <- tidytext:::augment.LDA(chapters_lda, data = chapters_dtm)
```

We can combine this with the consensus book titles to find which words were incorrectly classified.


```r
assignments <- assignments %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE) %>%
  inner_join(book_topics, by = c(".topic" = "topic"))

assignments
```

```
## # A tibble: 104,721 × 6
##                 title chapter  term count .topic          consensus
##                 <chr>   <int> <chr> <dbl>  <dbl>              <chr>
## 1  Great Expectations      57   joe    88      4 Great Expectations
## 2  Great Expectations       7   joe    70      4 Great Expectations
## 3  Great Expectations      17   joe     5      4 Great Expectations
## 4  Great Expectations      27   joe    58      4 Great Expectations
## 5  Great Expectations       2   joe    56      4 Great Expectations
## 6  Great Expectations      23   joe     1      4 Great Expectations
## 7  Great Expectations      15   joe    50      4 Great Expectations
## 8  Great Expectations      18   joe    50      4 Great Expectations
## 9  Great Expectations       9   joe    44      4 Great Expectations
## 10 Great Expectations      13   joe    40      4 Great Expectations
## # ... with 104,711 more rows
```

We can, for example, create a "confusion matrix" using dplyr's `count` and tidyr's `spread`:


```r
assignments %>%
  count(title, consensus, wt = count) %>%
  spread(consensus, n, fill = 0)
```

```
## Source: local data frame [4 x 5]
## Groups: title [4]
## 
##                                   title `Great Expectations` `Pride and Prejudice`
## *                                 <chr>                <dbl>                 <dbl>
## 1                    Great Expectations                49770                  3876
## 2                   Pride and Prejudice                    1                 37229
## 3                 The War of the Worlds                    0                     0
## 4 Twenty Thousand Leagues under the Sea                    0                     5
##   `The War of the Worlds` `Twenty Thousand Leagues under the Sea`
## *                   <dbl>                                   <dbl>
## 1                    1845                                      77
## 2                       7                                       5
## 3                   22561                                       7
## 4                       0                                   39629
```

We notice that almost all the words for *Pride and Prejudice*, *Twenty Thousand Leagues Under the Sea*, and *War of the Worlds* were correctly assigned, while *Great Expectations* had a fair amount of misassignment.

What were the most commonly mistaken words?


```r
wrong_words <- assignments %>%
  filter(title != consensus)

wrong_words
```

```
## # A tibble: 4,535 × 6
##                                    title chapter     term count .topic
##                                    <chr>   <int>    <chr> <dbl>  <dbl>
## 1                     Great Expectations      38  brother     2      1
## 2                     Great Expectations      22  brother     4      1
## 3                     Great Expectations      23     miss     2      1
## 4                     Great Expectations      22     miss    23      1
## 5  Twenty Thousand Leagues under the Sea       8     miss     1      1
## 6                     Great Expectations      31     miss     1      1
## 7                     Great Expectations       5 sergeant    37      1
## 8                     Great Expectations      46  captain     1      2
## 9                     Great Expectations      32  captain     1      2
## 10                 The War of the Worlds      17  captain     5      2
##                                consensus
##                                    <chr>
## 1                    Pride and Prejudice
## 2                    Pride and Prejudice
## 3                    Pride and Prejudice
## 4                    Pride and Prejudice
## 5                    Pride and Prejudice
## 6                    Pride and Prejudice
## 7                    Pride and Prejudice
## 8  Twenty Thousand Leagues under the Sea
## 9  Twenty Thousand Leagues under the Sea
## 10 Twenty Thousand Leagues under the Sea
## # ... with 4,525 more rows
```

```r
wrong_words %>%
  count(title, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))
```

```
## # A tibble: 3,500 × 4
##                 title             consensus     term     n
##                 <chr>                 <chr>    <chr> <dbl>
## 1  Great Expectations   Pride and Prejudice     love    44
## 2  Great Expectations   Pride and Prejudice sergeant    37
## 3  Great Expectations   Pride and Prejudice     lady    32
## 4  Great Expectations   Pride and Prejudice     miss    26
## 5  Great Expectations The War of the Worlds     boat    25
## 6  Great Expectations   Pride and Prejudice   father    19
## 7  Great Expectations The War of the Worlds    water    19
## 8  Great Expectations   Pride and Prejudice     baby    18
## 9  Great Expectations   Pride and Prejudice  flopson    18
## 10 Great Expectations   Pride and Prejudice   family    16
## # ... with 3,490 more rows
```

Notice the word "flopson" here; these wrong words do not necessarily appear in the novels they were misassigned to. Indeed, we can confirm "flopson" appears only in *Great Expectations*:


```r
word_counts %>%
  filter(word == "flopson")
```

```
## # A tibble: 3 × 3
##                document    word     n
##                   <chr>   <chr> <int>
## 1 Great Expectations_22 flopson    10
## 2 Great Expectations_23 flopson     7
## 3 Great Expectations_33 flopson     1
```

The algorithm is stochastic and iterative, and it can accidentally land on a topic that spans multiple books.

## Alternative LDA implementations

The `LDA` function in the topicmodels package is only one implementation of . For example, the [mallet](https://cran.r-project.org/package=mallet) package implements a wrapper around the  [MALLET](http://mallet.cs.umass.edu/)

The inputs and outputs of the tidying functions are almost identical to the tidiers described in this rest of this chapter. In particular, they take


```r
library(mallet)

collapsed <- by_chapter_word %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = str_replace(word, "'", "")) %>%
  group_by(document) %>%
  summarize(text = paste(word, collapse = " "))

# The mallet package requires a file of stopwords
# Since we've already filtered them, we can give it an empty file
file.create(empty_file <- tempfile())
docs <- mallet.import(collapsed$document, collapsed$text, empty_file)

mallet_model <- MalletLDA(num.topics = 4)
mallet_model$loadDocuments(docs)
mallet_model$train(200)

# word-topic pairs
tidy(mallet_model)

# document-topic pairs
tidy(mallet_model, matrix = "gamma")

# column needs to be named "term" for "augment"
word_counts <- rename(word_counts, term = word)
augment(mallet_model, word_counts)
```
