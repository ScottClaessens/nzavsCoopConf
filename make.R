# make file
library(tidyverse)
source("R/packages.R")
source("R/functions.R")
source("R/plan.R")
make(plan, lock_envir = FALSE)