library(tidyverse)
library(stats)
library(ggplot2)
library(lmtest)
library(car)

# Load data into project

load(url("https://www.dropbox.com/s/cnwtcr096szm8im/omsba_5112_birthweight.rdata?raw=1"))

my_data <- birthweight

head(my_data)
Summary(my_data)