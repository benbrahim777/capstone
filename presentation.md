

Text Prediction in R
========================================================
author: Kevin Thompson
date: April 20, 2015

For the capstone course we developed an application that attempts to predict the next word given a set of english words.

https://blackfist.shinyapps.io/capstone-deploy

Output
========================================================

The application will provide 5 suggested words based on the input text. Also, the app will provide several plots to show the frequency of
predicted words based on bigrams, trigrams, and 4-grams.

I started with a corpus of tweets, blog posts, and news articles and extracted n-grams in sizes of 2, 3, and 4.

I also calculated the most frequently used words in the whole corpus

Also calcualted the most frequently used words to start a sentence.

Runtime
=======

At runtime, the application takes the user input and extracts the last largest n-gram from the input up to n-grams of size 3.

"This is a sample sentence for you to consider" would result in a last largest tri-gram of "you to consider"

The application looks up the five most frequently seen 4-grams that start with "you to consider"

If there are not five then the app takes the last bigram from the input ("to consider" in this example) and adds those results onto whatever
results were found in the 4-grams table.

Continues reducing until it has to add the most frequently seen words.

Cool plots, bro
===============
For extra coolness, the app also returns plots of the most frequently encountered words based on the bi-grams, tri-grams, and 4-grams.
"This is a sample sentence for you to"


```r
results <- fourGrams[list("for","you","to")]
results
```

```
Source: local data table [5 x 5]
Groups: 

   p1  p2 p3     o1  n
1 for you to     be 82
2 for you to    see 40
3 for you to   come 54
4 for you to    get 69
5 for you to follow 66
```

Cool plots, bro
===============
![plot of chunk unnamed-chunk-3](presentation-figure/unnamed-chunk-3-1.png) 

Here's that link one more time: https://blackfist.shinyapps.io/capstone-deploy
