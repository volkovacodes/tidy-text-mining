# Topic modeling {#topicmodeling}



In text mining, we often have collections of documents, such as blog posts or news articles, that we'd like to divide into natural groups so that we can understand them separately. Topic modeling is a method for unsupervised classification of such documents, similar to "clustering" on numerical data, which finds natural groups of items even when we're not sure what we're looking for.

[Latent Dirichlet allocation](https://en.wikipedia.org/wiki/Latent_Dirichlet_allocation) is a particularly popular method for fitting a topic model. It treats each document as a mixture of topics, and each topic as a mixture of words. This allows documents to "overlap" each other in terms of content, rather than being separated into discrete groups, in a way that mirrors typical use of natural language.

We can use tidy text principles to approach topic modeling using consistent and effective tools. In particular, we'll be tidying LDA objects from the [topicmodels package](https://cran.r-project.org/package=topicmodels). In this chapter, we'll use the example of clustering chapters from several books, where we can see that a topic model *learns* to tell the difference based on the text content.

## The great library heist

Suppose a vandal has broken into your study and torn apart four of your books:

* *Great Expectations* by Charles Dickens
* *The War of the Worlds* by H.G. Wells
* *Twenty Thousand Leagues Under the Sea* by Jules Verne
* *Pride and Prejudice* by Jane Austen

This vandal has torn the books into individual chapters, and left them in one large pile. How can we restore these disorganized chapters to their original books? This is a challenging problem since the individual chapters are **unlabeled**: we don't know what words might distinguish them into groups. We'll thus use topic modeling to discover how chapters cluster into distinct topics, each of them representing one of the words.

We'll retrieve the text of these four books using the gutenbergr package introduced in Chapter \ref{tfidf}.


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



As pre-processing, we divide these into chapters, use tidytext's `unnest_tokens()` to separate them into words, then remove `stop_words`. We're treating every chapter as a separate "document", each with a name like `Great Expectations_1` or `Pride and Prejudice_11`. (In practice, each document might be one newspaper article, or one blog post).


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

### Latent Dirichlet allocation with the topicmodels package

Latent Dirichlet allocation is one of the most common algorithms for topic modeling. Without diving into the math behind the model, we can understand it using two principles.

* **Every topic is a mixture of words.** for example, we could imagine a two-topic model of American news, with one topic for "politics" and one for "entertainment." The most common words in the politics topic might be "President", "Congress", and "government", while the entertainment topic may be made up of words such as "movies", "television", and "actor". Importantly, words can be shared between topics- a word like "budget" might appear in both equally.
* **Every document is a mixture of topics.** for example, in a two-topic model we could say "Document 1 is 90% topic A and 10% topic B, while Document 2 is 30% topic A and 70% topic B."

LDA is a mathematical method for estimating both of these at the same time: finding the mixture of words that is associated with each topic, while also determining the mixture of topics that describes each word. It has a number of existing implementations, and we'll explore one of them.

Right now our data frame `word_counts`, is in a tidy form, with one-term-per-document-per-row. However, the topicmodels package requires a `DocumentTermMatrix` (from the tm package). As described in Chapter \ref{dtm}, we can cast a one-token-per-row table into a `DocumentTermMatrix` with tidytext's `cast_dtm()`.


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

We're now ready to use the [topicmodels](https://cran.r-project.org/package=topicmodels) package, specifically the `LDA()` function, to create a four-topic Latent Dirichlet allocation model. (In this case we know there are four topics because there are four books: in other problems we may need to try a few different values of `k`).


```r
library(topicmodels)

# setting a seed so that the output is predictable
chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))
chapters_lda
```

```
## A LDA_VEM topic model with 4 topics.
```

This step fits an LDA model, and returns an object containing the full details of the fit, such as how words are associated with topics and how topics are associated with documents. Fitting the model was the "easy part": the remainder of the analysis will involve exploring and interpreting the model using tidying functions.

### Tidying an LDA model

In Chapter \ref{dtm} we introduced the `tidy()` method, originally from the [broom package](https://github.com/dgrtwo/broom), for tidying model objects. The tidytext package provides this method for extracting the per-topic-per-word probabilities, called $\beta$ ("beta"), from the model.


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

Notice that this has turned the model into a one-topic-per-term-per-row format. For each combination, the model computes $\beta$ (), the probability of that term being generated from that topic. For example, the term "joe" has an almost zero probability of being generated from topics 1, 2, or 3, but it makes up 1.45% chance of being generated from topic 4.

We could use dplyr's `top_n()` to find the top 5 terms within each topic:


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

This output lends itself well to a ggplot2 visualization (Figure \ref{toptermsplot}).


```r
library(ggplot2)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
```

<div class="figure">
<img src="07-topic-models_files/figure-html/toptermsplot-1.png" alt="The terms that are most common within each topic" width="672" />
<p class="caption">(\#fig:toptermsplot)The terms that are most common within each topic</p>
</div>

These topics are pretty clearly associated with the four books! There's no question that the topic of "nemo", "sea", and "nautilus" belongs to *Twenty Thousand Leagues Under the Sea*, and that "jane", "darcy", and "elizabeth" belongs to *Pride and Prejudice*. We see "pip" and "joe" from *Great Expectations* and "martians", "black", and "night" from *The War of the Worlds*.

We also notice that there can be words in common between multiple topics, such as "miss" in topics 1 and 4, and "time" in topics 3 and 4. This shows how LDA is a "fuzzy clustering" method: rather than giving particular words to each topic, it treats them as a mixture of all words, with different proportions.

## Per-document classification

Each chapter was a "document" in this analysis. Thus, we may want to know which topics are associated with each document. Can we put the chapters back together in the correct books?

We already examined the per-word-per-topic probabilities returned by `tidy()`. But we can also examine the per-document-per-topic probabilities, called $$\gamma$$ ("gamma"), using the `matrix = "gamma"` argument.


```r
chapters_lda_gamma <- tidy(chapters_lda, matrix = "gamma")
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

Each of these values is an estimated proportion of words from that document that are generated from that topic. For example, the model estimates that each word in the Great Expectations_57 document has only a 0.00135% probability of coming from topic 1.

Now that we have these document classifiations, we can see how well our unsupervised learning did at distinguishing the four books. We'd expect that chapters within a book would be found to be mostly (or entirely), generated from the corresponding topic.

First we re-separate the document name into title and chapter, after which we can visualize the per-document-per-topic probability for each (Figure \ref{fig:chaptersldagamma}). 


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


```r
ggplot(chapters_lda_gamma, aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title)
```

<div class="figure">
<img src="07-topic-models_files/figure-html/chaptersldagamma-1.png" alt="The gamma probabilities for each chapter within each book" width="768" />
<p class="caption">(\#fig:chaptersldagamma)The gamma probabilities for each chapter within each book</p>
</div>

We notice that almost all of the chapters from *Pride and Prejudice*, *War of the Worlds*, and *Twenty Thousand Leagues Under the Sea* were uniquely identified as a single topic each.

It does look like some chapters from Great Expectations (which should be topic 4) were somewhat associated with other topics. Are there any cases where the topic most associated with a chapter belonged to another book? First we'd find the topic that was most associated with each chapter using `top_n()`, which is effectively the "classification" of that chapter.


```r
chapter_classifications <- chapters_lda_gamma %>%
  group_by(title, chapter) %>%
  top_n(1, gamma) %>%
  ungroup()

chapter_classifications
```

```
## # A tibble: 193 × 4
##                  title chapter topic     gamma
##                  <chr>   <int> <int>     <dbl>
## 1   Great Expectations      23     1 0.5507241
## 2  Pride and Prejudice      43     1 0.9999610
## 3  Pride and Prejudice      18     1 0.9999654
## 4  Pride and Prejudice      45     1 0.9999038
## 5  Pride and Prejudice      16     1 0.9999466
## 6  Pride and Prejudice      29     1 0.9999300
## 7  Pride and Prejudice      10     1 0.9999203
## 8  Pride and Prejudice       8     1 0.9999134
## 9  Pride and Prejudice      56     1 0.9999337
## 10 Pride and Prejudice      47     1 0.9999506
## # ... with 183 more rows
```

We can then compare each to the "consensus" topic for each book (the most common topic among its chapters), and see which were most often misidentified.


```r
book_topics <- chapter_classifications %>%
  count(title, topic) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = title, topic)

chapter_classifications %>%
  inner_join(book_topics, by = "topic") %>%
  filter(title != consensus)
```

```
## # A tibble: 2 × 5
##                title chapter topic     gamma             consensus
##                <chr>   <int> <int>     <dbl>                 <chr>
## 1 Great Expectations      23     1 0.5507241   Pride and Prejudice
## 2 Great Expectations      54     3 0.4803234 The War of the Worlds
```

We see that only two chapters from *Great Expectations* were misclassified, as LDA described one as coming from the "Pride and Prejudice" topic (topic 1) and one from The War of the Worlds (topic 3). That's not bad for unsupervised clustering!

## By word assignments: `augment`

One step that LDA performs is assigning each word in each document to a topic. The more words in a document are assigned to that topic, generally, the more weight (`gamma`) will go on that document-topic classification.

We may want to take the original document-word pairs and find which words in each document were assigned to which topic. This is the job of the `augment()` function, which also originated in the broom package as a way of tidying model output. While `tidy()` retrieves the statistical components of the model, `augment()` uses a model to add information to each observation in the original data.


```r
assignments <- augment(chapters_lda, data = chapters_dtm)
assignments
```

```
## # A tibble: 104,721 × 4
##                 document  term count .topic
##                    <chr> <chr> <dbl>  <dbl>
## 1  Great Expectations_57   joe    88      4
## 2   Great Expectations_7   joe    70      4
## 3  Great Expectations_17   joe     5      4
## 4  Great Expectations_27   joe    58      4
## 5   Great Expectations_2   joe    56      4
## 6  Great Expectations_23   joe     1      4
## 7  Great Expectations_15   joe    50      4
## 8  Great Expectations_18   joe    50      4
## 9   Great Expectations_9   joe    44      4
## 10 Great Expectations_13   joe    40      4
## # ... with 104,711 more rows
```

This returns a tidy data frame of book-term counts, but adds an extra column: `.topic`, with the topic each term was assigned to within each document. (Extra columns added by `augment` always start with `.`, to prevent overwriting existing columns). We can combine this assignments with the consensus book titles to find which words were incorrectly classified.


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

We can, for example, create a "confusion matrix," showing how often words from one book were assigned to another, using dplyr's `count()` and tidyr's `spread()`.


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

We notice that almost all the words for *Pride and Prejudice*, *Twenty Thousand Leagues Under the Sea*, and *War of the Worlds* were correctly assigned, while *Great Expectations* had a fair amount of misassignment (which, as we saw above, led to two chapters getting misclassified).

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

We can see that a number of words were often assigned to the Pride and Prejudice or War of the Worlds cluster even when they appeared in Great Expectations. For some of these words, such as "love" and "lady", that's because they're more common in Pride and Prejudice (we could confirm that by examining the counts).

On the other hand, there are a few wrongly classified words that never appeared in the novel they were misassigned to. For example, we can confirm "flopson" appears only in *Great Expectations*, even though it's assigned to the "Pride and Prejudice" cluster.


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

TODO: this section is not complete.

The `LDA` function in the topicmodels package is only one implementation of the latent Dirichlet allocation algorithm. For example, the [mallet](https://cran.r-project.org/package=mallet) package implements a wrapper around the [MALLET](http://mallet.cs.umass.edu/) package for text classification tools.

The way an algorithm is run with this algorithm is very different from LDA: it takes non-tokenized documents and performs the tokenization itself, and requires a separate file of stopwords.


```r
library(mallet)

# Create a vector with one string per chapter
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
mallet_model$train(100)
```

Once the model is created, however, are almost identical to the tidiers described in this rest of this chapter.


```r
# word-topic pairs
tidy(mallet_model)
```

```
## # A tibble: 71,064 × 3
##    topic    term         beta
##    <int>   <chr>        <dbl>
## 1      1 limping 2.624902e-07
## 2      2 limping 2.888532e-07
## 3      3 limping 2.385345e-07
## 4      4 limping 9.847097e-05
## 5      1  pirate 2.624902e-07
## 6      2  pirate 2.888532e-07
## 7      3  pirate 2.385345e-07
## 8      4  pirate 9.847097e-05
## 9      1  gibbet 2.624902e-07
## 10     2  gibbet 2.888532e-07
## # ... with 71,054 more rows
```

```r
# document-topic pairs
tidy(mallet_model, matrix = "gamma")
```

```
## # A tibble: 772 × 3
##                 document topic     gamma
##                    <chr> <int>     <dbl>
## 1   Great Expectations_1     1 0.2387737
## 2  Great Expectations_10     1 0.3073366
## 3  Great Expectations_11     1 0.3375767
## 4  Great Expectations_12     1 0.3431721
## 5  Great Expectations_13     1 0.3205056
## 6  Great Expectations_14     1 0.3084016
## 7  Great Expectations_15     1 0.2118029
## 8  Great Expectations_16     1 0.2525381
## 9  Great Expectations_17     1 0.3800562
## 10 Great Expectations_18     1 0.3459066
## # ... with 762 more rows
```

```r
# column needs to be named "term" for "augment"
term_counts <- rename(word_counts, term = word)
augment(mallet_model, term_counts)
```

```
## # A tibble: 104,721 × 4
##                    document    term     n .topic
##                       <chr>   <chr> <int>  <int>
## 1     Great Expectations_57     joe    88      4
## 2      Great Expectations_7     joe    70      4
## 3     Great Expectations_17   biddy    63      4
## 4     Great Expectations_27     joe    58      4
## 5     Great Expectations_38 estella    58      4
## 6      Great Expectations_2     joe    56      4
## 7     Great Expectations_23  pocket    53      4
## 8     Great Expectations_15     joe    50      4
## 9     Great Expectations_18     joe    50      4
## 10 The War of the Worlds_16 brother    50      1
## # ... with 104,711 more rows
```
