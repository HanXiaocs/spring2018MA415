library(tidyverse)
library(knitr)

tab1 <- tibble(
  person = c("JS","JD","MJ"),
  treatmenta = c(NA, 16,3),
  treatmentb = c(2,11,1)
)
tab1

tab2 <- tibble(
  treatment = c("A","B"),
  JS = c(NA,2),
  JD = c(16,11),
  MJ = c(3,1)
)

library(stringr)
str1 <- "this is a string"
str_detect(str1,"a*")
str_count(str1," ")
