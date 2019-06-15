# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  UCB
# Purpose:      Main of UCB
# programmer:   Zhe Liu
# Date:         11-06-2019
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


options(scipen = 200,
        uri = "http://59.110.31.50:8082",
        stringsAsFactors = FALSE)

library(plyr)
library(dplyr)
library(tidyr)
library(DT)
library(jsonlite)
library(curl)
library(uuid)

source("./Kafka.R", encoding = "UTF-8")
source("./Functions.R", encoding = "UTF-8")
source("./Calculation.R", encoding = "UTF-8")

load("./intermedia.RData")

main <- function() {
  start(callRConsumer, "", "")
}

main()




