# Code 

This directory contains analysis code called by the `.Rmd` files in the `analysis` directory. 

File contents, organized by the rmarkdown files which call chunks in the files and order of dependencies.

0. `chunk-option`: Global options from the rmarkdown files.

### tf-idf.Rmd
1. `tf-idf.R`: TF-IDF analylsis and set plots

### train-model.Rmd
2. `perplexity-cv.R`: 5-fold cross validation of perplexity scores
3. `ldatuning-scores`: Scores and plot based on method from the `ldatuing` package
4. `coherence-scores`: UMass internal coherence scores and plot
5. `gamma-distributions`: Distribution of sets over topic probabilitlies

### topic-term-distributions.Rmd

6. `compare-models.R`: Plots of color distributions for select models and plot and tables of sets most associated with some topics. 

### final-model.Rmd

7. `final-model-plots.R`: Bar plot and time series of topic counts of the final topic model
8. `topic-names`: Generate topic names for the final model
9. `final-model-grid.R`: Waffle plots of the final topics
