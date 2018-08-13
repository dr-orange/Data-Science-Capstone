# Data-Science-Capstone
[Data Science Capstone on Coursera](https://www.coursera.org/specializations/jhu-data-science)

# Project Overview

Around the world, people are spending an increasing amount of time on their mobile devices for email, social networking, banking and a whole range of other activities. But typing on mobile devices can be a serious pain. SwiftKey, our corporate partner in this capstone, builds a smart keyboard that makes it easier for people to type on their mobile devices. One cornerstone of their smart keyboard is predictive text models. When someone types:

I went to the

the keyboard presents three options for what the next word might be. For example, the three words might be gym, store, restaurant. In this capstone you will work on understanding and building predictive text models like those used by SwiftKey.

# Smart Keyboard (Shiny App)

- **Prediction** - Apply "Katz's Back-off" model and "Good Turing Estimate" smoothing with saved parameters.
- **Autocomplete** - Apply the model using the previous word. And output the candidate words by matching the characters.
- **Autocorrect** - If no N-gram is matched, calculate the word distance to all 1-gram. Apply the model with the nearest word and predict next word.

[Smart Keyboard - Data Science Capstone | Coursera](http://rpubs.com/dr_orange_jr/SmartKey) (RPubs)

**Efficiency:** <0.1 sec each prediction

**Accuracy:**  21.4% accuracy
which the top 3 candidates have correct word.

**Perplexity:** 2864 Test / 63.2 Train

**Memory Usage:** 350.5 Mb

**Experience:**  30% less typing time.

![](./FinalProjectSubmission/Rpresentation/res/App_large.png)

## App

[Smart Keyboard Shiny App](https://dr-orange-jr.shinyapps.io/SmartKeyboardApp/)

## Features

- Fast & Inclemental Prediction
- Autocomplete
- Autocorrect
- Applicable by tapping word or by pressing TAB key

![](./FinalProjectSubmission/Rpresentation/res/flow.png)

# References
- [Data Science Specialization - Capstone | Community Site](http://datasciencespecialization.github.io/capstone/)
- [Quick Start Guide • quanteda](http://docs.quanteda.io/articles/pkgdown/quickstart.html)
- [Lecture Slides from the Stanford Coursera course | Natural Language Processing](https://web.stanford.edu/~jurafsky/NLPCourseraSlides.html)
by Dan Jurafsky and Christopher Manning
- [Text Mining Infrastructure in R | Journal of Statistical Software](https://www.jstatsoft.org/article/view/v025i05/v25i05.pdf)
by Ingo Feinerer, Kurt Hornik, David Meyer, Wirtschaftsuniversita ̈t Wie
- [Speech and Language Processing (3rd ed. draft)](https://web.stanford.edu/~jurafsky/slp3/)
by Daniel Jurafsky, Stanford University, James H. Martin, University of Colorado at Boulder
- [Useful Resources from Luis von Ahn's Research Group](https://www.cs.cmu.edu/~biglou/resources/)
- [https://abicky.net/2011/01/30/204409/](https://abicky.net/2011/01/30/204409/)
- [Good-Turing smoothing without tears (1995)](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.110.8518)
by William A. Gale

- [Perplexity](https://en.wikipedia.org/wiki/Perplexity)
- [generation of ngrams is producing string of prefix, suffix and elsewhere underscores #923](https://github.com/quanteda/quanteda/issues/923)
- [Text Mining with R](https://www.tidytextmining.com)

- [An introduction to data cleaning with R](https://cran.r-project.org/doc/contrib/de_Jonge+van_der_Loo-Introduction_to_data_cleaning_with_R.pdf)
