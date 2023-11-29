dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)
library(tidyverse)
rm(dir)

#### Preprocessing MIMIC lab data & lab test selection ####

# reading laboratory table of mimic-IV v2.0
mimic_iv <- readRDS("mimic-iv 2.0 labevents.rds")
mimic_iv <- mimic_iv %>% select(itemid, valuenum, valueuom, subject_id, specimen_id)

# creating lab test list
mimic_itemid_count <- mimic_iv %>% select(itemid, valuenum) %>%
  group_by(itemid) %>%
  summarise(n = sum(!is.na(valuenum) & valuenum != "")) 

# filtering out tests with <10,000 numeric result records (n=712)
mimic_itemid_count <- mimic_itemid_count %>% filter(n > 10000)
mimic_iv <- mimic_iv %>% filter(itemid %in% mimic_itemid_count$itemid)

# making sure that each lab itemid has one unit 
mimic_item_unit <- mimic_iv %>% 
  select(itemid, valueuom) %>%
  group_by(itemid, valueuom) %>% 
  summarise(n = n(), .groups = 'drop')
mimic_item_unit <- mimic_item_unit %>% filter(mimic_item_unit$valueuom != "" & mimic_item_unit$valueuom != "N/A")
mimic_item_unit_distinct <- mimic_item_unit[!(duplicated(mimic_item_unit$itemid) | duplicated(mimic_item_unit$itemid, fromLast = TRUE)), ]
mimic_item_unit_duplicates <- mimic_item_unit %>% filter(!(mimic_item_unit$itemid %in% mimic_item_unit_distinct$itemid))
mimic_item_unit_dd <- mimic_item_unit %>% distinct(itemid, .keep_all = TRUE) %>% select(-n)
mimic_item_unit_dd$valueuom[mimic_item_unit_dd$itemid == "50915"] <- "ng/mL"
mimic_item_unit_dd$valueuom[mimic_item_unit_dd$itemid == "50993"] <- "uIU/mL"
mimic_item_unit_dd$valueuom[mimic_item_unit_dd$itemid == "51099"] <- "Ratio"
mimic_item_unit_dd$valueuom[mimic_item_unit_dd$itemid == "51249"] <- "g/dL"
mimic_item_unit_dd$valueuom[mimic_item_unit_dd$itemid == "51282"] <- "m/uL"
rm(mimic_item_unit, mimic_item_unit_distinct, mimic_item_unit_duplicates)

# Mapping local item id to LOINC
mimic_to_loinc <- read.csv("d_labitems_to_loinc mapping list.csv", sep =",")
# Downloaded from https://github.com/MIT-LCP/mimic-code/blob/a88fc5d27337d9a76400d1a94895211554d28025/mimic-iv/concepts/concept_map/d_labitems_to_loinc.csv
#Latest commit 9630a06 on May 27
#Accessed on November 29th
names(mimic_to_loinc)[1] <- "itemid"
loinc_table <- read.csv("LoincTableCore 2.74 - Mar23.csv", sep =",")
names(loinc_table)[1] <- "loinc_code"
loinc_table <- loinc_table %>% select(loinc_code, PROPERTY, CLASSTYPE)
names(loinc_table)[2] <- "loinc_property"
names(loinc_table)[3] <- "loinc_classtype"
mimic_to_loinc <- mimic_to_loinc %>% left_join(loinc_table, by = "loinc_code")  
rm(loinc_table)

# reading itemid details
mimic_labitem_details <- read.csv("d_labitems.csv", sep =",") %>%
  left_join(mimic_to_loinc, by = "itemid") %>%
  left_join(mimic_item_unit_dd, by = "itemid") %>%
  left_join(mimic_itemid_count, by = "itemid") %>%
  filter(!is.na(n))
rm(mimic_item_unit_dd, mimic_to_loinc, mimic_itemid_count)

# excluding 18 itemids not mapped to LOINC
mimic_labitem_details <- mimic_labitem_details %>% filter(mimic_labitem_details$loinc_code != "")
sum(mimic_labitem_details$n)
# excluding 7 itemids not lab tests (loinc_classtype != 1)
mimic_labitem_details <- mimic_labitem_details %>% filter(mimic_labitem_details$loinc_classtype == "1")
sum(mimic_labitem_details$n)
# excluding 3 itemids which are not real lab tests e.g. collection duration of urine (n=3)
mimic_labitem_details <- mimic_labitem_details %>% filter(!(mimic_labitem_details$itemid %in% c("51108", "51087", "51613")))
# excluding 26 itemids with loinc fractional scale type
mimic_labitem_details <- mimic_labitem_details %>% filter(mimic_labitem_details$loinc_property != "NFr" & mimic_labitem_details$loinc_property != "VFr")
sum(mimic_labitem_details$n)

# creating mimic-iv subset for the result values of the itemids selected for limits setting
mimic_iv_subset <- mimic_iv %>%
  filter(itemid %in% mimic_labitem_details$itemid & !is.na(valuenum))

# Excluding 6 tests with less than 15 distinct result values
mimic_itemid_distinct_count <- mimic_iv_subset %>% select(itemid, valuenum) %>%
  group_by(itemid) %>%
  summarise(n = n_distinct(valuenum)) %>% 
  filter(n < 15)
mimic_iv_subset <- mimic_iv_subset %>%
  filter(!(itemid %in% mimic_itemid_distinct_count$itemid))
mimic_labitem_details <- mimic_labitem_details %>% filter(!(itemid %in% mimic_itemid_distinct_count$itemid))
rm(mimic_itemid_distinct_count)
n_distinct(mimic_labitem_details$loinc_code)

# checking for duplicates of itemids mapped to same loinc code
duplicate_loinc_labitem <- mimic_labitem_details[(duplicated(mimic_labitem_details$loinc_code) | duplicated(mimic_labitem_details$loinc_code, fromLast = T)), ]
duplicate_loinc_labitem <- duplicate_loinc_labitem %>% arrange(desc(loinc_code), desc(n))
duplicate_loinc_labitem <- duplicate_loinc_labitem %>% distinct(loinc_code, fluid, valueuom, .keep_all = T)

# unit conversion of itemid 52769, 51253
# 2 duplicate of itemids were for the same test, but needs unit conversion (itemid_51133 = itemid_52769 /1000)
mimic_labitem_details$valueuom[mimic_labitem_details$itemid == "52769"] <- "K/uL"
mimic_labitem_details$valueuom[mimic_labitem_details$itemid == "51253"] <- "K/uL"
mimic_iv_unit_converted <- mimic_iv_subset %>% filter(mimic_iv_subset$itemid == "52769")
mimic_iv_unit_converted$valuenum <- as.numeric(as.character(mimic_iv_unit_converted$valuenum)) / 1000
mimic_iv_unit_same <- mimic_iv_subset %>% filter(mimic_iv_subset$itemid != "52769")
mimic_iv_subset <- rbind(mimic_iv_unit_same, mimic_iv_unit_converted)
rm(duplicate_loinc_labitem, mimic_iv_unit_same, mimic_iv_unit_converted)

# merging 12 similar loinc codes
itemid_loinc_unit <- mimic_labitem_details %>% select(itemid, loinc_code, valueuom)
itemid_loinc_unit$loinc_code <- gsub(itemid_loinc_unit$loinc_code, pattern = "1959-6", replacement = "1963-8")
itemid_loinc_unit$loinc_code <- gsub(itemid_loinc_unit$loinc_code, pattern = "2069-3", replacement = "2075-0")
itemid_loinc_unit$loinc_code <- gsub(itemid_loinc_unit$loinc_code, pattern = "18262-6", replacement = "13457-7")
itemid_loinc_unit$loinc_code <- gsub(itemid_loinc_unit$loinc_code, pattern = "2339-0", replacement = "2345-7")
itemid_loinc_unit$loinc_code <- gsub(itemid_loinc_unit$loinc_code, pattern = "6298-4", replacement = "2823-3")
itemid_loinc_unit$loinc_code <- gsub(itemid_loinc_unit$loinc_code, pattern = "2947-0", replacement = "2951-2")
n_distinct(itemid_loinc_unit$loinc_code)

# finalizing data subset for analysis 
mimic_iv_subset <- mimic_iv_subset %>% select(-valueuom)
mimic_iv_subset <- mimic_iv_subset %>% left_join(itemid_loinc_unit, by = "itemid")
mimic_iv_subset <- mimic_iv_subset %>% relocate(loinc_code, .after="itemid")
mimic_iv_subset <- mimic_iv_subset %>% relocate(valueuom, .after="valuenum")
rm(itemid_loinc_unit)
mimic_loinc_stats <- mimic_iv_subset %>% select(loinc_code, valuenum, subject_id) %>%
  group_by(loinc_code) %>%
  summarise(n_results = sum(!is.na(valuenum)), n_patients = n_distinct(subject_id), .groups = 'drop')
mimic_labitem_details <- mimic_labitem_details %>% distinct(loinc_code, fluid, valueuom, .keep_all = T)
mimic_loinc_details <- mimic_loinc_stats %>% left_join((mimic_labitem_details %>% select(-n)), by = "loinc_code")
sum(mimic_loinc_details$n_results)
rm(mimic_labitem_details, mimic_loinc_stats)

# stats of selected lab tests
(nrow(mimic_iv_subset) / nrow(mimic_iv)) *100
rm(mimic_iv)
length(unique(mimic_iv_subset$subject_id))
length(unique(mimic_iv_subset$specimen_id))
length(unique(mimic_iv_subset$itemid))
length(unique(mimic_iv_subset$loinc_code))

saveRDS(mimic_iv_subset, "mimic_iv_subset.rds")
write.csv(mimic_loinc_details, "mimic_loinc_details.csv", row.names = F, na = "")

#### Establishing Reportable Interval ####
mimic_iv_subset <- readRDS("mimic_iv_subset.rds")
mimic_loinc_details <- read.csv("mimic_loinc_details.csv") %>% 
  select(-loinc_classtype, -itemid, -label) %>% 
  relocate(n_results, n_patients, .after = "valueuom") %>%
  relocate(fluid, category, .before = "loinc_property")
mimic_iv_subset <- mimic_iv_subset %>% select(loinc_code, valuenum)

# cleaning negative results allowing only for base excess & anion gap lab test to have negative results
allowed_negatives <- c("11555-0", "1863-0")
loinc_negataive_implausibe <- mimic_iv_subset %>% 
  filter(!(mimic_iv_subset$loinc_code %in% allowed_negatives) & mimic_iv_subset$valuenum < 0)%>%
  group_by(loinc_code) %>%
  summarise(n = n(), .groups = 'drop')
sum(loinc_negataive_implausibe$n)
names(loinc_negataive_implausibe)[names(loinc_negataive_implausibe) == "n"] <- "n_negative_implausible"
mimic_loinc_details <- mimic_loinc_details %>% left_join(loinc_negataive_implausibe, by = "loinc_code")
# discarding negative implausible results
mimic_iv_subset <- mimic_iv_subset %>% filter((loinc_code %in% allowed_negatives) | (valuenum >= 0))
rm(allowed_negatives, loinc_negataive_implausibe)
sum(mimic_loinc_details$n_results)

# numeric result statistics
mimic_result_quantiles <- mimic_iv_subset %>%
  group_by(loinc_code) %>%
  summarise_all(list(P0.05 = ~quantile(., probs=0.0005, type=6), 
                     P25 = ~quantile(., probs=0.25, type=6),                      
                     median = ~quantile(., probs=0.5, type=6), 
                     P75 = ~quantile(., probs=0.75, type=6),                      
                     P99.95 = ~quantile(., probs=0.9995, type=6)), 
                .groups = 'drop') 
mimic_result_quantiles <- mimic_result_quantiles %>% mutate_at(vars(2,3,5,6), list(~round(., 3)))

# Ordering the distinct result values
mimic_subset_ordered <- mimic_iv_subset %>% 
  arrange(loinc_code, valuenum) %>%
  group_by(loinc_code, valuenum) %>%
  summarise(n = n(), .groups = 'drop')

# getting the lowest 5 values (including min)
mimic_lowest_5 <- mimic_subset_ordered %>% group_by(loinc_code) %>% slice(head(row_number(), 5))
mimic_lowest_5$order <- rep(1:5,nrow(mimic_loinc_details))
mimic_lowest_5 <- mimic_lowest_5 %>% pivot_wider(names_from = "order", values_from = c("valuenum", "n"), names_glue = '{order}_{.value}', names_sort = TRUE)
names(mimic_lowest_5)[2:11] <- c("min","min+1","min+2","min+3","min+4","min_n","min+1_n","min+2_n","min+3_n","min+4_n")
mimic_lowest_5 <- mimic_lowest_5[,c("loinc_code","min","min_n","min+1","min+1_n","min+2","min+2_n","min+3","min+3_n","min+4", "min+4_n")]

loinc_statistical_checks <- mimic_loinc_details %>%
  left_join(mimic_lowest_5, by = "loinc_code") %>%
  left_join(mimic_result_quantiles, by = "loinc_code")
rm(mimic_lowest_5, mimic_result_quantiles, mimic_loinc_details)

# getting the highest 10 values (including max)
mimic_highest_10 <- mimic_subset_ordered %>% group_by(loinc_code) %>% slice(tail(row_number(), 10))
mimic_highest_10$order <- rep(1:10, n_distinct(mimic_highest_10$loinc_code))
mimic_highest_10 <- mimic_highest_10 %>% pivot_wider(names_from = "order", values_from = c("valuenum", "n"), names_glue = '{order}_{.value}', names_sort = TRUE)
names(mimic_highest_10)[2:21] <- c("max_9","max_8","max_7","max_6","max_5","max_4","max_3","max_2","max_1","max",
                                   "max_9_n","max_8_n","max_7_n","max_6_n","max_5_n", "max_4_n","max_3_n","max_2_n","max_1_n","max_n")
mimic_highest_10 <- mimic_highest_10[,c("loinc_code","max_9","max_9_n","max_8","max_8_n","max_7","max_7_n","max_6","max_6_n",
                                      "max_5", "max_5_n", "max_4","max_4_n","max_3","max_3_n","max_2","max_2_n","max_1","max_1_n","max", "max_n")]
high_reportable_limit <- loinc_statistical_checks %>% 
  select(loinc_code, min, P99.95) %>% 
  left_join(mimic_highest_10, by = "loinc_code")
rm(mimic_highest_10)

# Outlier check 1
high_reportable_limit$outlier_check1 <- round(((high_reportable_limit$max - high_reportable_limit$P99.95)/(high_reportable_limit$max - high_reportable_limit$min)), 3)
# Loop to create and relocate outlier check columns (Check 2:10)
for (i in 2:10) {
  max_current_col <- sym(ifelse(i == 2, "max", paste0("max_", i - 2)))
  max_next_col <- sym(paste0("max_", i - 1))
  outlier_col_name <- paste0("outlier_check", i)
  
  high_reportable_limit <- high_reportable_limit %>%
    mutate(!!outlier_col_name := round((!!max_current_col - !!max_next_col) / (!!max_current_col - min), 3)) %>%
    relocate(!!outlier_col_name, .before = !!max_current_col)
}
# Ordering by the first two outlier checks
high_reportable_limit <- high_reportable_limit %>%
  arrange(outlier_check1, outlier_check2)

# Define a function for outlier checks
# Function to update highest non-outlier status based on multiple checks
update_outlier_status <- function(df, value_checked) {
  df_subset <- df[is.na(df$highest_non_outlier),]
  df_notchecked <- df[!is.na(df$highest_non_outlier),]
  # Determine the indices for checks
  index_suffix <- as.numeric(sub("max_", "", value_checked))
  index_suffix <- ifelse(is.na(index_suffix), 2, index_suffix + 2)  # Adjust index for checks
  
  # Main and Secondary Condition Checks
  main_condition <- df_subset$outlier_check1 < 0.333
  secondary_check <- paste0("outlier_check", index_suffix)
  secondary_condition <- df_subset[[secondary_check]] < 0.333
  
  # Other Conditions Checks
  other_checks_start <- index_suffix + 1
  other_checks <- sapply(other_checks_start:10, function(i) df_subset[[paste0("outlier_check", i)]] < 0.333)
  any_other_condition_high <- sapply(other_checks_start:10, function(i) df_subset[[paste0("outlier_check", i)]] >= 0.333)
  
  # Non-Outlier and Expert Check Cases
  non_outlier_case1 <- main_condition & secondary_condition
  non_outlier_case2 <- secondary_condition & apply(other_checks, 1, all)
  expert_check_case1 <- main_condition & !secondary_condition
  expert_check_case2 <- secondary_condition & apply(any_other_condition_high, 1, any)
  
  # Update Dataframe
  df_subset$highest_non_outlier[non_outlier_case1 | non_outlier_case2] <- value_checked
  df_subset$highest_non_outlier[expert_check_case1 | expert_check_case2] <- paste0(value_checked, "?")
  df_subset$highest_non_outlier_value <- ifelse(df_subset$highest_non_outlier %in% c(value_checked, paste0(value_checked, "?")), 
                                         df_subset[[value_checked]], 
                                         df_subset$highest_non_outlier_value)
  df_subset$highest_non_outlier_n <- ifelse(df_subset$highest_non_outlier == value_checked, df_subset[[paste0(value_checked, "_n")]], df_subset$highest_non_outlier_n)
  df_subset$outlier_n <- ifelse(df_subset$highest_non_outlier %in% c("max", "max?"), NA, 
                         ifelse(df_subset$highest_non_outlier %in% c("max_1", "max_1?"), df_subset$max_n,
                                ifelse(df_subset$highest_non_outlier %in% c("max_2", "max_2?"), (df_subset$max_n + df_subset$max_1_n),
                                       ifelse(df_subset$highest_non_outlier %in% c("max_3", "max_3?"), (df_subset$max_n + df_subset$max_1_n+ df_subset$max_2_n), 
                                              df_subset$outlier_n))))
  df <- rbind(df_subset, df_notchecked)
  return(df)
}
high_reportable_limit$highest_non_outlier <- NA
high_reportable_limit$highest_non_outlier_value <- NA
high_reportable_limit$highest_non_outlier_n <- NA
high_reportable_limit$outlier_n <- NA
# Checking values
high_reportable_limit <- update_outlier_status(high_reportable_limit, "max")
high_reportable_limit$outlier_check1 <- ifelse(is.na(high_reportable_limit$highest_non_outlier), 
                                                round(((high_reportable_limit$max_1 - high_reportable_limit$P99.95)/(high_reportable_limit$max_1 - high_reportable_limit$min)), 3), 
                                                high_reportable_limit$outlier_check1)
high_reportable_limit <- update_outlier_status(high_reportable_limit, "max_1")
high_reportable_limit$outlier_check1 <- ifelse(is.na(high_reportable_limit$highest_non_outlier), 
                                                round(((high_reportable_limit$max_2 - high_reportable_limit$P99.95)/(high_reportable_limit$max_2 - high_reportable_limit$min)), 3), 
                                                high_reportable_limit$outlier_check1)
high_reportable_limit <- update_outlier_status(high_reportable_limit, "max_2")
high_reportable_limit$outlier_check1 <- ifelse(is.na(high_reportable_limit$highest_non_outlier), 
                                                round(((high_reportable_limit$max_3 - high_reportable_limit$P99.95)/(high_reportable_limit$max_3 - high_reportable_limit$min)), 3), 
                                                high_reportable_limit$outlier_check1)
high_reportable_limit <- update_outlier_status(high_reportable_limit, "max_3")

high_reportable_limit$high_hypoth_outlier <- ifelse(!is.na(high_reportable_limit$highest_non_outlier), round((((3*high_reportable_limit$highest_non_outlier_value)-high_reportable_limit$min)/2), 3), NA)
loinc_statistical_checks <- loinc_statistical_checks %>% left_join(high_reportable_limit %>% select(-min, -P99.95), by = "loinc_code")
rm(high_reportable_limit)

write.csv(loinc_statistical_checks, "loinc statistical checks.csv", na="", row.names=FALSE)