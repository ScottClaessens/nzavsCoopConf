# Cooperative and conformist behavioural preferences predict the dual dimensions of political ideology

## Getting Started

### Installing

To run this code, you will need to [install R](https://www.r-project.org/) and the following R packages:

```
install.packages(c("brms", "cowplot", "drake", "english", "fastDummies", 
                   "ggrepel", "grid", "gridExtra", "haven", "irr", 
                   "kableExtra", "lavaan", "lm.beta", "magick",
                   "maps", "papaja", "psych", "sjlabelled", "tidyverse"))
```

and the following R package from GitHub:

```
install.packages("devtools")
devtools::install_github("ddueber/BifactorIndicesCalculator")
```

Since the data underlying this project are not publicly available, you will also need to contact me to access the data file (scott.claessens@gmail.com). Without it, it will not be possible to run the pipeline or generate the manuscript.

### Executing code

1. Add the data file to the `data` folder
2. In a fresh R session, set the working directory to this code repository `setwd("myPath")`
3. Source the R script `make.R` to run the code
4. To load individual targets into your environment, run `drake::loadd(targetName)`

## Help

Any issues, please email scott.claessens@gmail.com.

## Authors

Scott Claessens, scott.claessens@gmail.com
