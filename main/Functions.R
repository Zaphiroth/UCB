# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  UCB
# Purpose:      Functions of UCB
# programmer:   Zhe Liu
# Date:         29-05-2019
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

##--- Curve
curve_func <- function(curve, curves, input) {
  
  curve_data <- curves[[curve]]
  
  if (input < min(curve_data$x))
    return(curve_data[which.min(curve_data$x), 2])
  
  if (input > max(curve_data$x))
    return(curve_data[which.max(curve_data$x), 2])
  
  left <- curve_data[which.min(abs(input - curve_data$x)), ]
  tmp <- curve_data[-which.min(abs(input - curve_data$x)), ]
  right <- tmp[which.min(abs(input - tmp$x)), ]
  
  y <- ifelse(left$x <= right$x,
              (1 - (input - left$x) / (right$x - left$x)) * left$y + (1 - (right$x - input) / (right$x - left$x)) * right$y, 
              (1 - (input - right$x) / (left$x - right$x)) * right$y + (1 - (left$x - input) / (left$x - right$x)) * left$y)
  
  return(y)
}

##--- Preprocess
preprocess <- function(receive) {
  
  dat_json <- fromJSON(receive, simplifyVector = FALSE)[[1]][["value"]] %>% 
    toJSON(auto_unbox = TRUE) %>% 
    fromJSON(simplifyDataFrame = TRUE)
  
  current_phase <- dat_json[["currentScenario"]][["phase"]]
  
  p_data <- do.call(data.frame, dat_json[["body"]][["histories"]]) %>% 
    rename("phase" = "scenario.phase",
           "hospital" = "hospital.name",
           "representative" = "representative.name",
           "product" = "product.name",
           "quota" = "sales.quota",
           "ytd_sales" = "ytd.sales",
           "status" = "drug.entrance.info")
  
  p_data1 <- p_data %>% 
    filter(phase == current_phase - 1)
  
  p_data2 <- p_data %>% 
    filter(phase == current_phase - 2)
  
  p_data3 <- p_data %>% 
    filter(phase == current_phase - 3)
  
  p_data4 <- p_data %>% 
    filter(phase == current_phase - 4)
  
  product_data <- dat_json[["body"]][["inputs"]][["products"]]
  hospital_data <- select(dat_json[["body"]][["inputs"]], -`products`)
  
  bind_data <- data.frame()
  for (i in 1:nrow(hospital_data)) {
    m <- hospital_data[i, ][rep(1, nrow(product_data[[i]])), ] %>% 
      cbind(product_data[[i]])
    bind_data <- rbind(bind_data, m)
  }
  
  input_data <- do.call(data.frame, bind_data) %>%
    as.data.frame(row.names = c(1:nrow(bind_data))) %>% 
    rename("city_id" = "city.id",
           "city" = "city.name",
           "hospital_id" = "hospital.id",
           "hospital_level" = "hospital.level",
           "hospital" = "hospital.name",
           "representative_id" = "representative.representative.id",
           "representative" = "representative.representative.name",
           "patient" = "patient.count",
           "product_id" = "product.id",
           "product" = "product.name",
           "product_area" = "treatment.area",
           "quota" = "sales.target") %>%
    select(`city_id`, `city`, `hospital_id`, `hospital_level`, `hospital`, `representative_id`, `representative`, 
           `product_id`, `product`, `product_area`, `quota`, `budget`, `potential`, `patient`)
  
  competitor_data <- do.call(data.frame, dat_json[["body"]][["competitions"]]) %>% 
    rename("product_id" = "product.id",
           "product" = "product.name",
           "product_area" = "treatment.area")
  
  dat <- list("header" = dat_json[["header"]],
              "account" = dat_json[["account"]],
              "proposal" = dat_json[["proposal"]],
              "paperInput" = dat_json[["paperInput"]],
              "scenario" = dat_json[["currentScenario"]],
              "scenarios" = dat_json[["scenarios"]],
              "phase" = current_phase,
              "p_data1" = p_data1,
              "p_data2" = p_data2,
              "p_data3" = p_data3,
              "p_data4" = p_data4,
              "input_data" = input_data,
              "competitor_data" = competitor_data)
  
  return(dat)
}

##--- Postprocess
postprocess <- function(report, assessment, dat) {
  
  send_data <- list("header" = dat[["header"]],
                    "account" = dat[["account"]],
                    "proposal" = dat[["proposal"]],
                    "scenario" = dat[["scenario"]][["id"]],
                    "paperInput" = dat[["paperInput"]],
                    "body" = list("hospitalSalesReports" = report$hospital_report,
                                  "representativeSalesReports" = report$representative_report,
                                  "productSalesReports" = report$product_report,
                                  "citySalesReports" = report$city_report,
                                  "simplifyReport" = assessment))
  
  return(send_data)
}

##--- Calculation
get_result <- function(input_data, p_data1, p_data4, current_phase, curves, weightages) {
  
  p_data1 <- p_data1 %>% 
    rename("p_representative" = "representative",
           "p_sales" = "sales",
           "p_quota" = "quota",
           "p_budget" = "budget",
           "p_ytd_sales" = "ytd_sales")
  
  p_data4 <- p_data4 %>% 
    rename("pppp_representative" = "representative",
           "pppp_sales" = "sales",
           "pppp_quota" = "quota",
           "pppp_budget" = "budget",
           "pppp_ytd_sales" = "ytd_sales")
  
  cal_data <- input_data %>% 
    left_join(p_data1, by = c("hospital", "product")) %>% 
    left_join(p_data4[c("hospital", "product", "pppp_sales")], by = c("hospital", "product")) %>% 
    mutate(p_ytd_sales = ifelse(current_phase %% 4 == 1,
                                0,
                                p_ytd_sales),
           status = ifelse(is.na(status),
                           "未开发",
                           status)) %>% 
    select(`city_id`, `city`, `hospital_id`, `hospital`, `hospital_level`, `representative_id`, 
           `representative`, `product_id`, `product`, `product_area`, `potential`, `patient`, 
           `status`, `p_quota`, `p_sales`, `pppp_sales`, `p_ytd_sales`, `quota`, `budget`) %>% 
    mutate_all(function(x) {ifelse(is.na(x) | is.infinite(x), 0, x)})
  
  market <- list()
  for (i in unique(cal_data$product)) {
    market[[i]] <- cal_data[which(cal_data$product == i), ] %>% 
      mutate(budget_prop = budget / sum(budget, na.rm = TRUE) * 100)
  }
  
  result <- data.frame()
  
  for (i in names(market)) {
    
    data01 <- market[[i]] %>% 
      filter(status == "已开发") %>% 
      mutate(level_base = 0.8 * potential / sum(potential, na.rm = TRUE) + 0.2 * p_sales / sum(p_sales, na.rm = TRUE)) %>% 
      arrange(-`level_base`) %>% 
      mutate(level = row_number())
    
    if (nrow(data01) > 0) {
      data01 <- data01 %>% 
        mutate(oa_factor_base = ifelse(product == "开拓来" | product == "威芃可",
                                       ifelse(level <= 7,
                                              sapply(budget_prop, function(x) {curve_func("curve02", curves, x)}),
                                              ifelse(level > 7 & level <= 23,
                                                     sapply(budget_prop, function(x) {curve_func("curve03", curves, x)}),
                                                     ifelse(level > 23,
                                                            sapply(budget_prop, function(x) {curve_func("curve04", curves, x)}),
                                                            0))),
                                       ifelse(product == "优派西",
                                              ifelse(level <= 4,
                                                     sapply(budget_prop, function(x) {curve_func("curve02", curves, x)}),
                                                     ifelse(level > 4 & level <= 18,
                                                            sapply(budget_prop, function(x) {curve_func("curve03", curves, x)}),
                                                            ifelse(level > 18,
                                                                   sapply(budget_prop, function(x) {curve_func("curve04", curves, x)}),
                                                                   0))),
                                              0))) %>% 
        mutate(hospital_quota_potential_factor = ks.test(.$quota/sum(.$quota), .$potential/sum(.$potential), exact = TRUE)$p.value,
               hospital_quota_potential_factor = ifelse(hospital_quota_potential_factor < 0,
                                                        0,
                                                        hospital_quota_potential_factor),
               hospital_quota_sales_factor = ks.test(.$quota/sum(.$quota), .$p_sales/sum(.$p_sales), exact = TRUE)$p.value,
               hospital_quota_sales_factor = ifelse(hospital_quota_sales_factor < 0,
                                                    0,
                                                    hospital_quota_sales_factor),
               hospital_product_quota_growth_factor = 1 / (abs((quota - p_sales) / (potential - p_sales) - 
                                                                 (sum(quota, na.rm = TRUE) - sum(p_sales, na.rm = TRUE)) / 
                                                                 (sum(potential, na.rm = TRUE) - sum(p_sales, na.rm = TRUE))) + 1),
               factor1 = 
                 hospital_quota_potential_factor * weightages[["weightage01"]]$hospital_quota_potential_factor * weightages[["weightage02"]]$factor1 + 
                 hospital_quota_sales_factor * weightages[["weightage01"]]$hospital_quota_sales_factor * weightages[["weightage02"]]$factor1 + 
                 hospital_product_quota_growth_factor * weightages[["weightage02"]]$hospital_product_quota_growth_factor) %>% 
        group_by(representative) %>% 
        mutate(potential_dist = sum(potential, na.rm = TRUE),
               sales_dist = sum(p_sales, na.rm = TRUE),
               district_cross_factor = ifelse(length(unique(city)) == 1,
                                              1,
                                              0),
               hospital_num_dist = n()) %>% 
        ungroup() %>% 
        mutate(district_potential_factor = sum(market[[i]]$potential, na.rm = TRUE)/6 / (abs(potential_dist - sum(market[[i]]$potential, na.rm = TRUE)/6) 
                                                                                         + sum(market[[i]]$potential, na.rm = TRUE)/6),
               district_sales_factor = sum(market[[i]]$p_sales, na.rm = TRUE)/6 / (abs(sales_dist - sum(market[[i]]$p_sales, na.rm = TRUE)/6) 
                                                                                   + sum(market[[i]]$p_sales, na.rm = TRUE)/6),
               district_hospital_factor = 1 / (abs(hospital_num_dist - 5) + 1),
               factor2 = district_potential_factor * weightages[["weightage03"]]$district_potential_factor * weightages[["weightage04"]]$factor2 + 
                 district_sales_factor * weightages[["weightage03"]]$district_sales_factor * weightages[["weightage04"]]$factor2 + 
                 district_cross_factor * weightages[["weightage04"]]$district_cross_factor + 
                 district_hospital_factor * weightages[["weightage04"]]$district_hospital_factor) %>% 
        mutate(factor = factor1 * weightages[["weightage09"]]$factor1 + factor2 * weightages[["weightage09"]]$factor2,
               adjust_factor = ifelse(factor >= 0.85,
                                      0,
                                      ifelse(factor >= 0.75 & factor < 0.85,
                                             0.3,
                                             ifelse(factor >= 0.5 & factor < 0.75,
                                                    0.6,
                                                    ifelse(factor < 0.5,
                                                           1,
                                                           999)))),
               p_offer_attractiveness = sapply(p_sales/potential * 100, function(x) {curve_func("curve09", curves, x)}),
               max_oa = ifelse(product == "开拓来",
                               100,
                               ifelse(product == "威芃可" | product == "优派西",
                                      40,
                                      0)),
               offer_attractiveness = ifelse(oa_factor_base >= 0,
                                             p_offer_attractiveness + (max_oa - p_offer_attractiveness) * oa_factor_base,
                                             ifelse(oa_factor_base < 0,
                                                    p_offer_attractiveness * (1 + oa_factor_base),
                                                    0)),
               offer_attractiveness = ifelse(product == "开拓来",
                                             offer_attractiveness - 5 * adjust_factor,
                                             ifelse(product == "威芃可" | product == "优派西",
                                                    offer_attractiveness - 2 * adjust_factor,
                                                    0)),
               market_share = ifelse(representative == 0,
                                     0,
                                     sapply(offer_attractiveness, function(x) {curve_func("curve01", curves, x)})),
               market_share = market_share / 100,
               sales = potential * market_share) %>% 
        select(names(cal_data), "market_share", "sales")
    } 
    
    data02 <- market[[i]] %>%
      filter(status == "正在开发") %>% 
      mutate(level_base = 0.8 * potential / sum(potential, na.rm = TRUE) + 0.2 * p_sales / sum(p_sales, na.rm = TRUE)) %>% 
      arrange(-`level_base`) %>% 
      mutate(level = row_number())
    
    if (nrow(data02) > 0) {
      data02 <- data02 %>% 
        mutate(oa_factor_base = ifelse(product == "威芃可",
                                       ifelse(level <= 7,
                                              sapply(budget_prop, function(x) {curve_func("curve02", curves, x)}),
                                              ifelse(level > 7 & level <= 23,
                                                     sapply(budget_prop, function(x) {curve_func("curve03", curves, x)}),
                                                     ifelse(level > 23,
                                                            sapply(budget_prop, function(x) {curve_func("curve04", curves, x)}),
                                                            0))),
                                       ifelse(product == "优派西",
                                              ifelse(level <= 4,
                                                     sapply(budget_prop, function(x) {curve_func("curve02", curves, x)}),
                                                     ifelse(level > 4 & level <= 18,
                                                            sapply(budget_prop, function(x) {curve_func("curve03", curves, x)}),
                                                            ifelse(level > 18,
                                                                   sapply(budget_prop, function(x) {curve_func("curve04", curves, x)}),
                                                                   0))),
                                              0))) %>% 
        mutate(hospital_quota_potential_factor = ks.test(.$quota/sum(.$quota), .$potential/sum(.$potential), exact = TRUE)$p.value,
               hospital_quota_potential_factor = ifelse(hospital_quota_potential_factor < 0,
                                                        0,
                                                        hospital_quota_potential_factor),
               hospital_product_quota_growth_factor = 1 / (abs(quota / potential - sum(quota, na.rm = TRUE) / sum(potential, na.rm = TRUE)) + 1),
               factor1 = (hospital_quota_potential_factor * weightages[["weightage06"]]$hospital_quota_potential_factor + 
                            hospital_product_quota_growth_factor * weightages[["weightage06"]]$hospital_product_quota_growth_factor)) %>% 
        group_by(representative) %>% 
        mutate(potential_dist = sum(potential, na.rm = TRUE),
               district_cross_factor = ifelse(length(unique(city)) == 1,
                                              1,
                                              0),
               hospital_num_dist = n()) %>% 
        ungroup() %>% 
        mutate(district_potential_factor = sum(market[[i]]$potential, na.rm = TRUE)/6 / (abs(potential_dist - sum(market[[i]]$potential, na.rm = TRUE)/6) 
                                                                                         + sum(market[[i]]$potential, na.rm = TRUE)/6),
               district_hospital_factor = 1 / (abs(hospital_num_dist - 5) + 1),
               factor2 = district_potential_factor * weightages[["weightage07"]]$district_potential_factor + 
                 district_cross_factor * weightages[["weightage07"]]$district_cross_factor + 
                 district_hospital_factor * weightages[["weightage07"]]$district_hospital_factor) %>% 
        mutate(factor = factor1 * weightages[["weightage09"]]$factor1 + factor2 * weightages[["weightage09"]]$factor2,
               adjust_factor = ifelse(factor >= 0.85,
                                      0,
                                      ifelse(factor >= 0.75 & factor < 0.85,
                                             0.3,
                                             ifelse(factor >= 0.5 & factor < 0.75,
                                                    0.6,
                                                    ifelse(factor < 0.5,
                                                           1,
                                                           999)))),
               p_offer_attractiveness = sapply(oa_factor_base, function(x) {runif(1, 3, 7)}),
               offer_attractiveness = ifelse(oa_factor_base >= 0,
                                             p_offer_attractiveness + (40 - p_offer_attractiveness) * oa_factor_base,
                                             ifelse(oa_factor_base < 0,
                                                    p_offer_attractiveness * (1 + oa_factor_base),
                                                    0)),
               offer_attractiveness = offer_attractiveness - 2 * adjust_factor,
               market_share = ifelse(representative == 0,
                                     0,
                                     sapply(offer_attractiveness, function(x) {curve_func("curve05", curves, x)})),
               market_share = market_share / 100,
               sales = potential * market_share,
               status = "已开发") %>% 
        select(names(cal_data), "market_share", "sales")
    }
    
    data03 <- market[[i]] %>% 
      filter(status == "未开发") %>% 
      arrange(-`potential`) %>% 
      mutate(level = row_number())
    
    if (nrow(data03) > 0) {
      data03 <- data03 %>% 
        left_join(p_data1[c("hospital", "product", "p_budget")], by = c("hospital", "product")) %>% 
        mutate_all(function(x) {ifelse(is.na(x) | is.infinite(x), 0, x)}) %>% 
        mutate(budget = budget + p_budget,
               market_share = 0,
               sales = 0,
               status = ifelse(product == "威芃可" & budget > potential * 0.005,
                               "正在开发",
                               ifelse(product == "优派西" & budget > potential * 0.006,
                                      "正在开发",
                                      "未开发"))) %>% 
        select(names(cal_data), "market_share", "sales")
    }
    
    market_result <- bind_rows(data01, data02, data03) %>% 
      mutate(quota_achievement = sales / quota,
             sales_growth = sales / p_sales - 1,
             quota_growth = quota / p_sales - 1,
             ytd_sales = p_ytd_sales + sales,
             sales_year_on_year = sales / pppp_sales - 1,
             sales_month_on_month = sales / p_sales - 1) %>% 
      mutate_all(function(x) {ifelse(is.na(x) | is.infinite(x), 0, x)})
    
    result <- bind_rows(result, market_result) %>% 
      arrange(product, hospital)
  }
  
  return(result)
}

get_report <- function(result, competitor_data) {
  
  out_hospital_report <- result %>% 
    group_by(product_id) %>% 
    summarise(potential = sum(potential, na.rm = TRUE) * 0.01) %>% 
    ungroup() %>% 
    mutate(hospital_id = "-1",
           representative_id = "-1",
           sales = potential * 0.02)
  
  hospital_report <- result %>% 
    bind_rows(out_hospital_report) %>% 
    group_by(product_id) %>% 
    mutate(quota_sum = sum(quota, na.rm = TRUE),
           sales_sum = sum(sales, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(quota_contribute = quota / quota_sum,
           sales_contribute = sales / sales_sum) %>% 
    select(hospital_id, product_id, representative_id, potential, quota, market_share, sales, 
           quota_achievement, sales_growth, quota_contribute, quota_growth, ytd_sales, sales_contribute, 
           sales_year_on_year, sales_month_on_month, status, patient) %>% 
    mutate_all(function(x) {ifelse(is.na(x) | is.infinite(x), 0, x)})
  names(hospital_report) <- c("hospital-id", "product-id", "representative-id", "potential", "sales-quota", 
                              "share", "sales", "quota-achievement", "sales-growth", "quota-contribute", 
                              "quota-growth", "ytd-sales", "sales-contribute", "sales-year-on-year", 
                              "sales-month-on-month", "drug-entrance-info", "patient-count")
  
  representative_report <- result %>% 
    select(representative_id, product_id, potential, pppp_sales, p_sales, p_quota, sales, ytd_sales, quota, patient) %>% 
    group_by(product_id, representative_id) %>% 
    summarise(potential = sum(potential, na.rm = TRUE),
              pppp_sales = sum(pppp_sales, na.rm = TRUE),
              p_sales = sum(p_sales, na.rm = TRUE),
              p_quota = sum(p_quota, na.rm = TRUE),
              sales = sum(sales, na.rm = TRUE),
              ytd_sales = sum(ytd_sales, na.rm = TRUE),
              quota = sum(quota, na.rm = TRUE),
              patient = sum(patient, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(product_id) %>% 
    mutate(quota_sum = sum(quota, na.rm = TRUE),
           sales_sum = sum(sales, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(market_share = sales / potential,
           quota_achievement = sales / quota,
           sales_growth = sales / p_sales - 1,
           quota_contribute = quota / quota_sum,
           quota_growth = quota / p_sales - 1,
           sales_contribute = sales / sales_sum,
           sales_year_on_year = sales / pppp_sales - 1,
           sales_month_on_month = sales / p_sales - 1) %>% 
    select(representative_id, product_id, potential, quota, market_share, sales, quota_achievement, sales_growth, 
           quota_contribute, quota_growth, ytd_sales, sales_contribute, sales_year_on_year, 
           sales_month_on_month, patient) %>% 
    mutate_all(function(x) {ifelse(is.na(x) | is.infinite(x), 0, x)})
  names(representative_report) <- c("representative-id", "product-id", "potential", "sales-quota", "share", "sales", 
                                    "quota-achievement", "sales-growth", "quota-contribute", "quota-growth", "ytd-sales", 
                                    "sales-contribute", "sales-year-on-year", "sales-month-on-month", "patient-count")
  
  competitor_report <- result %>% 
    group_by(product_area) %>% 
    summarise(potential = sum(potential, na.rm = TRUE),
              sales = sum(sales, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(market_share_p = sales / potential) %>% 
    right_join(competitor_data, by = c("product_area")) %>% 
    mutate(market_share = ifelse(product == "癫痫竞品1",
                                 (1 - market_share_p) * sample(seq(0.3, 0.5, 0.01), 1),
                                 ifelse(product == "癫痫竞品2",
                                        (1 - market_share_p) * sample(seq(0.05, 0.2, 0.01), 1),
                                        ifelse(product == "帕金森竞品1",
                                               (1 - market_share_p) * sample(seq(0.45, 0.6, 0.01), 1),
                                               0))),
           sales = potential * market_share) %>% 
    select(`product_id`, `market_share`, `sales`)
  
  product_report <- result %>% 
    select(product_id, potential, pppp_sales, p_sales, p_quota, sales, ytd_sales, quota, patient) %>% 
    group_by(product_id) %>% 
    summarise(potential = sum(potential, na.rm = TRUE),
              pppp_sales = sum(pppp_sales, na.rm = TRUE),
              p_sales = sum(p_sales, na.rm = TRUE),
              p_quota = sum(p_quota, na.rm = TRUE),
              sales = sum(sales, na.rm = TRUE),
              ytd_sales = sum(ytd_sales, na.rm = TRUE),
              quota = sum(quota, na.rm = TRUE),
              patient = sum(patient, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(market_share = sales / potential,
           quota_achievement = sales / quota,
           sales_growth = sales / p_sales - 1,
           quota_contribute = quota / sum(quota, na.rm = TRUE),
           quota_growth = quota / p_sales - 1,
           sales_contribute = sales / sum(sales, na.rm = TRUE),
           sales_year_on_year = sales / pppp_sales - 1,
           sales_month_on_month = sales / p_sales - 1) %>% 
    select(product_id, quota, market_share, sales, quota_achievement, sales_growth, quota_contribute, quota_growth, 
           ytd_sales, sales_contribute, sales_year_on_year, sales_month_on_month, patient) %>% 
    bind_rows(competitor_report) %>% 
    mutate_all(function(x) {ifelse(is.na(x) | is.infinite(x), 0, x)})
  names(product_report) <- c("product-id", "sales-quota", "share", "sales", "quota-achievement", "sales-growth", 
                             "quota-contribute", "quota-growth", "ytd-sales", "sales-contribute", "sales-year-on-year", 
                             "sales-month-on-month", "patient-count")
  
  city_report <- result %>% 
    select(city_id, product_id, potential, pppp_sales, p_sales, p_quota, sales, ytd_sales, quota, patient) %>% 
    group_by(city_id, product_id) %>% 
    summarise(potential = sum(potential, na.rm = TRUE),
              pppp_sales = sum(pppp_sales, na.rm = TRUE),
              p_sales = sum(p_sales, na.rm = TRUE),
              p_quota = sum(p_quota, na.rm = TRUE),
              sales = sum(sales, na.rm = TRUE),
              ytd_sales = sum(ytd_sales, na.rm = TRUE),
              quota = sum(quota, na.rm = TRUE),
              patient = sum(patient, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(product_id) %>% 
    mutate(quota_sum = sum(quota, na.rm = TRUE),
           sales_sum = sum(sales, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(market_share = sales / potential,
           quota_achievement = sales / quota,
           sales_growth = sales / p_sales - 1,
           quota_contribute = quota / quota_sum,
           quota_growth = quota / p_sales - 1,
           sales_contribute = sales / sales_sum,
           sales_year_on_year = sales / pppp_sales - 1,
           sales_month_on_month = sales / p_sales - 1) %>% 
    select(city_id, product_id, quota, market_share, sales, quota_achievement, sales_growth, quota_contribute, 
           quota_growth, ytd_sales, sales_contribute, sales_year_on_year, sales_month_on_month, patient) %>% 
    mutate_all(function(x) {ifelse(is.na(x) | is.infinite(x), 0, x)})
  names(city_report) <- c("city-id", "product-id", "sales-quota", "share", "sales", "quota-achievement", 
                          "sales-growth", "quota-contribute", "quota-growth", "ytd-sales", "sales-contribute", 
                          "sales-year-on-year", "sales-month-on-month", "patient-count")
  
  report <- list("hospital_report" = hospital_report,
                 "representative_report" = representative_report,
                 "product_report" = product_report,
                 "city_report" = city_report)
  
  return(report)
}

get_assessment <- function(result, p_data2, scenarios) {
  
  p_data2 <- p_data2 %>% 
    rename("pp_representative" = "representative",
           "pp_sales" = "sales",
           "pp_quota" = "quota",
           "pp_budget" = "budget",
           "pp_ytd_sales" = "ytd_sales")
  
  pp_quota_achievement <- sum(p_data2$sales, na.rm = TRUE) / sum(p_data2$quota, na.rm = TRUE)
  
  p_quota_achievement <- sum(result$p_sales, na.rm = TRUE) / sum(result$p_quota, na.rm = TRUE)
  
  quota_achievement <- sum(result$sales, na.rm = TRUE) / sum(result$quota, na.rm = TRUE)
  
  total_quota_achievement <- sum(c(p_data2$sales, result$p_sales, result$sales), na.rm = TRUE) / 
    sum(p_data2$quota, result$p_quota, result$quota, na.rm = TRUE)
  
  assessment <- result %>% 
    left_join(p_data2, by = c("hospital")) %>% 
    select(`sales`, `quota`, `p_sales`, `p_quota`, `pp_sales`, `pp_quota`) %>% 
    summarise(sales = sum(sales, na.rm = TRUE),
              quota = sum(quota, na.rm = TRUE),
              p_sales = sum(p_sales, na.rm = TRUE),
              p_quota = sum(p_quota, na.rm = TRUE),
              pp_sales = sum(pp_sales, na.rm = TRUE),
              pp_quota = sum(pp_quota, na.rm = TRUE)) %>% 
    mutate(pp_quota_achievement = pp_sales / pp_quota,
           p_quota_achievement = p_sales / p_quota,
           quota_achievement = sales / quota,
           total_quota_achievement = (pp_sales + p_sales + sales) / (pp_quota + p_quota + quota),
           level = ifelse(total_quota_achievement >= 0.9,
                          1,
                          ifelse(total_quota_achievement >= 0.7 & total_quota_achievement < 0.9,
                                 2,
                                 ifelse(total_quota_achievement >= 0 & total_quota_achievement < 0.7,
                                        3,
                                        0)))) %>% 
    select(pp_quota_achievement, p_quota_achievement, quota_achievement, total_quota_achievement, level) %>% 
    mutate_all(function(x) {ifelse(is.na(x) | is.infinite(x), 0, x)})
  
  assessment_report <- list("level" = assessment$level,
                            "total-quota-achievement" = assessment$total_quota_achievement,
                            "scenarioResult" = list(list("scenario-id" = scenarios$id[which(scenarios$phase == 1)],
                                                         "quota-achievement" = assessment$pp_quota_achievement),
                                                    list("scenario-id" = scenarios$id[which(scenarios$phase == 2)],
                                                         "quota-achievement" = assessment$p_quota_achievement),
                                                    list("scenario-id" = scenarios$id[which(scenarios$phase == 3)],
                                                         "quota-achievement" = assessment$quota_achievement)))
  
  return(assessment_report)
}

