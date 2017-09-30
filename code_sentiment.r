> # required packages
> library(twitteR)
> library(sentiment)
Loading required package: tm
Loading required package: NLP
Loading required package: Rstem
> library(plyr)

Attaching package: ‘plyr’

The following object is masked from ‘package:twitteR’:

    id

> library(ggplot2)

Attaching package: ‘ggplot2’

The following object is masked from ‘package:NLP’:

    annotate

> library(wordcloud)
Loading required package: RColorBrewer
> library(RColorBrewer)
> setup_twitter_oauth('45NnOSZTY2LzswH5FIXyz7QJn','nScg6Ea3ojWVHJAQf1BE93MnKhq0MT1Eq2rqfDXuw1Bt1CyNaR', access_token=NULL, access_secret=NULL)
[1] "Using browser based authentication"
> # harvest some tweets
> some_tweets = searchTwitter("Trump", n=1500, lang="en")
> 
> # get the text
> some_txt = sapply(some_tweets, function(x) x$getText())
> # remove retweet entities
> some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
> # remove at people
> some_txt = gsub("@\\w+", "", some_txt)
> # remove punctuation
> some_txt = gsub("[[:punct:]]", "", some_txt)
> # remove numbers
> some_txt = gsub("[[:digit:]]", "", some_txt)
> # remove html links
> some_txt = gsub("http\\w+", "", some_txt)
> # remove unnecessary spaces
> some_txt = gsub("[ \t]{2,}", "", some_txt)
> some_txt = gsub("^\\s+|\\s+$", "", some_txt)
> # define "tolower error handling" function 
> try.error = function(x)
+ {
+     # create missing value
+     y = NA
+     # tryCatch error
+     try_error = tryCatch(tolower(x), error=function(e) e)
+     # if not an error
+     if (!inherits(try_error, "error"))
+         y = tolower(x)
+     # result
+     return(y)
+ }
> # remove NAs in some_txt
> some_txt = some_txt[!is.na(some_txt)]
> names(some_txt) = NULL
> # classify emotion
> class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
> # get emotion best fit
> emotion = class_emo[,7]
> # substitute NA's by "unknown"
> emotion[is.na(emotion)] = "unknown"
> 
> # classify polarity
> class_pol = classify_polarity(some_txt, algorithm="bayes")
> # get polarity best fit
> polarity = class_pol[,4]
> # data frame with results
> sent_df = data.frame(text=some_txt, emotion=emotion,
+                      polarity=polarity, stringsAsFactors=FALSE)
> 
> # sort data frame
> sent_df = within(sent_df,
+                  emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
> head(sent_df)
                                                                                                                    text
1                     donald trump also owes billions of dollars to America maybe we should take away his drinking water
2       Courtney Weaver As Puerto Rico faced an urgent humanitarian disaster Donald Trump focused attention on the NFL …
3 Great speech by Trump at Alabama Rally for Luther Strange Valuable insights with regards to primaries incl the wining…
4                     Why Was the anthem playing Cuz you know trump says its about disrespecting the flag and the anthem
5           US may be largest donor of foreign aid—but it gives one of the lowest percentages of GNP George Ingram tells
6                          TRUMP WORLD \nLets Show Support To\n \n\nMAGA trump potus\nfox California news\nUSA America …
  emotion polarity
1 unknown positive
2 unknown negative
3     joy positive
4 unknown  neutral
5 unknown positive
6 unknown positive
#plot distribution of emotions
> ggplot(sent_df, aes(x=emotion)) +
+ geom_bar(aes(y=..count.., fill=emotion)) +
+ scale_fill_brewer(palette="Dark2") +
+ labs(x="emotion categories", y="number of tweets") +
+ ggtitle("Sentiment Analysis of Tweets about Trump\n(classification by emotion)")

> # plot distribution of polarity
> ggplot(sent_df, aes(x=polarity)) +
+     geom_bar(aes(y=..count.., fill=polarity)) +
+     scale_fill_brewer(palette="RdGy") +
+     labs(x="polarity categories", y="number of tweets") +
+     ggtitle("Sentiment Analysis of Tweets about Starbucks\n(classification by polarity)")
> # separating text by emotion
> emos = levels(factor(sent_df$emotion))
> nemo = length(emos)
> emo.docs = rep("", nemo)
> for (i in 1:nemo)
+ {
+     tmp = some_txt[emotion == emos[i]]
+     emo.docs[i] = paste(tmp, collapse=" ")
+ }
> 
> # remove stopwords
> emo.docs = removeWords(emo.docs, stopwords("english"))
> # create corpus
> corpus = Corpus(VectorSource(emo.docs))
> tdm = TermDocumentMatrix(corpus)
> tdm = as.matrix(tdm)
> colnames(tdm) = emos
> 
> # comparison word cloud
> comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
+                  scale = c(3,.5), random.order = FALSE, title.size = 1.5)
