---
title: "About"
output:
  html_document:
    toc: false
---

# Note 

I need to update the data access method since the `legolda::connect` method isn't included in the repo. You can replace this method by downloading the [da taset](https://www.kaggle.com/rtatman/lego-database) loading the tables into one of the SQL databases for which a DBI adaptor had been made (Postges, MySQL, SQLite). You then need a `connect()` method that returns a connection to this database.

# Installing and running the code 

The code is organized as a package with some additional structure based on the `workflowr` package. Both this repo and the `workflowr` package need to be installed to rebuild the analysis. (This could also be done directly with `rmarkdown`)

```
install.packages('workflowr')
install.packages('devtools')
devtools::install_github('nateaff/legolda')
```

The analysis also uses the following packages: 

```
purrr, tidyr, forcats, stringr, ggplot2, dplyr

# For text mining
topicmodels, SpeedReader, tidytext, ldatuning, clues, waffle
```

After installing required packages and setting your working directory to the `legolda` package you need to run:

```
library(legolda)
workflowr::wflow_build()
```
This builds the website version of the analysis you are reading now. 

There are a few intermediate cached files that are produced by the LDA function. These take a long time to run and to re-run you will need to set `from_cache` to `FALSE` in the `perplexity-cv.R` and `train-model.R` files. (I might update how this option is changed.) This takes 8 hours or so to run on a larger AWS instance. You also have the option of passing the `load_data` calls the `sample_data = 1000` parameter to run on a subset of the data. The sample number refers to the number of lego sets used to build the models.


