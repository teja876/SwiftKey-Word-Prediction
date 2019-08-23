Data Science Capstone: SwiftKey Word Prediction
========================================================
author: teja
date: 15 August, 2019
autosize: true

Objective
========================================================
This presentation is created as part of Capstone Project from Data Science Specialization by John Hopkins University in Coursera.

The main goal of this project is to build a Language model which take an input Key words entered by user through our Shiny App and to predict the next word and displays it.

Used text corpora of size of around 600MB which contains Twitter, News, Blogs text files.

Used n-gram language model to predict the next word.


Data cleaning and Preprocessing
========================================================

* Sampled 5% of data from Twitter, News, Blogs to build the model and can be generalised to the whole population.
* Used `tm`, `RWeka`, `SnowballC` packages to perform Natural Language Processing tasks.
* Built a corpus using the sampled data from the three text files mentioned above.
* performed preprocessing tasks like Punctuations removal, lower case conversion, Profanity filtering, etc.
* Used unigram, Bigram, Trigram, Quadgram model to tokenize the Corpus and saved as .rds files to perform predictions.

Modelling and Prediction
========================================================

* After tokenizing the Corpus with the 4 n-gram models, we use Back-off model to predict the right word.
* The model Back-offs from Quadgram to Bigram and if not matched the app just displays the most frequence word.
* First the model checks the last 3 words from user input with the quadgram and then Back-off to the Bigram
* Then the model checks the last 2 words with Trigram and if no match then Back-offs to the Bigram model.
* Then finally model checks the last word with the Bigrams and if no match then it Back-offs  to the Unigarm and displays the most frequent word.

Shiny App and Instruction
========================================================

In this app, Users are allowed to enter the text then model takes the text and predicts the next word and then displays it. It also displays which n-gram is used to predict the word. when no input is given the output displayed is `NULL`.

Shiny Application done by:
* *Sai Teja* with the support of Coursera.

Link for the Shiny App: https://myfirstshinyapp1.shinyapps.io/WordPrediction/

Source Code can be found in the link below.
* GitHub link : https://github.com/teja876/SwiftKey-Word-Prediction










