<style>
.section .reveal .state-background {
        background: #33aa00;
}
.reveal a:not(.image) {
  color: #33aa00;
}
</style>

Smart Keyboard App
========================================================
author: Koji
date: Aug 5, 2018
autosize: true
transition: rotate

![](./res/App_large.png)
------------

Introduction and Goal
========================================================

Introduction
-------------
- The goal of this project is to build an interactive Shiny application that can predict the next word following a phrase of input text.
- We used N-Gram model and the "Katz's Back-off" model with "Good Turing Estimation" smoothing.
- We use a dataset from a corpus called HC Corpora (downsampling).

Links
-------------
- [Smart Keyboard App](https://dr-orange-jr.shinyapps.io/HealthImpactOfTornadoApp/)
- [GitHub repo](https://github.com/dr-orange/Data-Science-Capstone/tree/master/FinalProjectSubmission/ShinyApplication)

Predicitive Model Algorithm
========================================================

![](./res/flow.png)

Quantitative Predictive Performance
========================================================


Save your typing time.
========================================================
[Smart Keyboard Shiny App](https://dr-orange-jr.shinyapps.io/HealthImpactOfTornadoApp/)
-------------
- Fast & Inclemental Prediction (sacrificing a bit of accuracy)
- Autocomplete
- Autocorrect
- Applicable by tapping word or by pressing TAB key

![](./res/App_small.png)
