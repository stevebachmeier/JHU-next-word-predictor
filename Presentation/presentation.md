<style>
/* slide titles */
.reveal h3 { 
  font-size: 40px;
  color: black;
  font-weight: bold;
}
</style>

Next-word Predictor
========================================================
author: Steve Bachmeier
date: 2018-10-22
transition: zoom
autosize: true

Background
========================================================
Natural language processing (NLP) is a challenging field that has many applications, including relevant text extraction, sentiment analysis, etc. The application presented here provides a solution to another related problem: next-word prediction.

Predicting the next word to a user's text input can decrease search time when using a web browser increase text accuracy/speed when using the technology on a phone. It's an exciting problem to solve!

The app can be found online at https://stevebachmeier.shinyapps.io/nextWordPredictor/.

Approach
========================================================
<small><span style="color:red;">NLP of any type is difficult for a variety of reasons, including (but not limited to) how do deal with: mis-spelled words and/or foreign words, sentiment, special characters, and commonly used words that may or may not matter to the problem at hand (called 'stopwords').</span></small>

<font size="1"><span style="color:red;">While there are many ways to build up a next-word predictor algorithm, we've chosen to use the so-called Stupid Backoff method. We started by combining and randomly sampling three text corpora - one of tweets, one of blogs, and one of news articles - generously provided by Swiftkey and cleaning/processing the text. Then we tokenized it and created data sets of 4-grams, trigrams, bigrams, and unigrams.</span>
***
Steps:
1. Sample text corpora.

2. Clean data set.

3. Tokenize.

4. Create n-gram sets.

5. Apply Stupid Backoff.
- Check 4-grams set for trigrams plus prediction. Score.
- Check trigrams set for bigrams plus prediction. Score with knockdown factor.
- Check bigrams set for unigrams plus prediction. Score with (knockdown factor)^2.
- Check unigrams set for top predictions. Score with (knockdown factor)^3.
6. Offer three highest predicted words.

Example
========================================================
![plot of chunk unnamed-chunk-1](./Capture-Output.png)

Performance
========================================================
It's very important that the app is both accurate as well as fast. As a comparison, SwiftKey's next-word prediction accuracy is often mentioned as being ~30%; we would like at least 10% accuracy to start for this app. As for speed, it qualitatively must be fast enough to be usable as an app hosted on shinyapps.io with the free subscription.

Using a benchmark tool that another student developed - which runs through 599 blogs and 793 tweets - we currently have the following:

Suggestions
========================================================


