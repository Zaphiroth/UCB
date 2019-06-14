# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  UCB
# Purpose:      Calculation of UCB
# programmer:   Zhe Liu
# Date:         29-05-2019
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

calculation <- function(receive) {
  
  if (length(fromJSON(receive)) != 0) {
    
    receive <<- receive
    
    send_data <- tryCatch({
      
      dat <- preprocess(receive = receive)
      result <- get_result(dat = dat, curves = curves, weightages = weightages)
      report <- get_report(result = result, competitor_data = dat$competitor_data)
      assessment <- get_assessment(result = result, dat = dat)
      
      send_data <- postprocess(report = report, assessment = assessment, dat = dat)
      
    }, error = function(e) {
      
      send_data <- list("error" = list("code" = 500,
                                       "msg" = "Calculation error. "))
    })
    
    send <- toJSON(list(records = list(list(value = as.list(send_data)))),auto_unbox = TRUE)
    sendResultMessage(paste0(options()$uri, "/topics"), "RReturnResult", send)
    print(prettify(send))  ## 非必要
  }
  
  print("null")  ## 非必要
}

