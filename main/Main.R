# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  UCB
# Purpose:      Main of UCB
# programmer:   Zhe Liu
# Date:         11-06-2019
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


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
envir <- read_json("./envir.json")

options(scipen = 200,
        uri = envir$uri,
        groupName = envir$groupName,
        receiveTopics = envir$receiveTopics,
        sendTopics = envir$sendTopics,
        stringsAsFactors = FALSE)

main <- function() {
  start(callRConsumer, "", "")
}

main()




