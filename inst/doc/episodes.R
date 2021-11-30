## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include=FALSE------------------------------------------------------------
library(diyar)

## ----warning=FALSE------------------------------------------------------------
event_dt <- seq(from = as.Date("2021-01-01"), to = as.Date("2021-01-11"), by = 1)
event_dt <- data.frame(date = event_dt)
event_dt$attr_1 <- c("BSI", "UTI", "RTI", "RTI", "BSI", "BSI", "BSI", "RTI", "RTI", "BSI", "RTI")
event_dt$attr_2 <- c("Ward 1", "Ward 1", "Ward 3", "Ward 3", "Ward 2", "Ward 2", 
                     "Ward 1", "Ward 1", "Ward 3","Ward 3", "Ward 2")

event_dt

## ----warning=FALSE------------------------------------------------------------
event_dt$ep1 <- episodes(event_dt$date, episode_type = "fixed")
event_dt

## ----warning=FALSE------------------------------------------------------------
event_dt$ep2 <- episodes(event_dt$date, 3, episode_type = "fixed")
event_dt

## ----warning=FALSE------------------------------------------------------------
event_dt$ep3 <- episodes(event_dt$date, 3, episode_type = "rolling")
event_dt$ep4 <- episodes(event_dt$date, 3, episode_type = "recursive")
event_dt

## ----warning=FALSE, fig.width=7, message=FALSE--------------------------------
schema(event_dt$ep1, seed = 2, show_labels = c("case_nm", "length_arrow"))
schema(event_dt$ep3, seed = 2, show_labels = c("case_nm", "length_arrow"))
schema(event_dt$ep4, seed = 2, show_labels = c("case_nm", "length_arrow"))

## ----warning=FALSE, fig.width=7-----------------------------------------------
event_dt$ep5 <- episodes(event_dt$date, 3, episode_type = "rolling", from_last = TRUE)
event_dt

## ----warning=FALSE, fig.width=7-----------------------------------------------
custom_pref <- ifelse(event_dt$attr_1 == "RTI", 1, 2)
event_dt$ep6 <- episodes(event_dt$date, 3, custom_sort = custom_pref, 
                         skip_order = 1)
event_dt$ep7 <- episodes(event_dt$date, 3, custom_sort = custom_pref, 
                         skip_order = 1, from_last = TRUE)

## ----warning=FALSE, fig.width=7-----------------------------------------------
schema(event_dt$ep6, seed = 2, show_labels = c("length_arrow"), 
       custom_label = paste0(decode(event_dt$ep6@case_nm), " (", event_dt$attr_1, ")"))
schema(event_dt$ep7, seed = 2, show_labels = c("length_arrow"), 
       custom_label = paste0(decode(event_dt$ep7@case_nm), " (", event_dt$attr_1, ")"))

## ----warning=FALSE, fig.width=7-----------------------------------------------
event_dt$ep8 <- episodes(event_dt$date, number_line(-3, 3), episode_type = "fixed", 
                         custom_sort = custom_pref, skip_order = 1)
event_dt$ep9 <- episodes(event_dt$date, list(number_line(-2, -1),
                                             number_line(4, 5),
                                             number_line(7, 9)), episode_type = "fixed", 
                         custom_sort = custom_pref, 
                         skip_order = 1, 
                         skip_if_b4_lengths = TRUE)

## ----warning=FALSE, fig.width=7-----------------------------------------------
schema(event_dt$ep8, seed = 2, show_labels = c("length_arrow"), 
       custom_label = paste0(decode(event_dt$ep8@case_nm), " (", event_dt$attr_1, ")"))
schema(event_dt$ep9, seed = 2, show_labels = c("length_arrow", "length_label"))

## ----warning=FALSE------------------------------------------------------------
# Dummy data of hospital stays
data(hospital_admissions)
dfr <- hospital_admissions[c("admin_dt", "discharge_dt")]
dfr$admin_period <- number_line(dfr$admin_dt, dfr$discharge_dt)
                                  
# Group overlapping hospital stays
dfr$ep_len1 <- index_window(dfr$admin_period)
dfr$ep1 <- episodes(date = dfr$admin_period, case_length = dfr$ep_len1)

# Group overlapping hospital stays and those within 21 days of the end point of an index hospital stay 
dfr$ep_len2 <- expand_number_line(index_window(dfr$admin_period), 20, "right")
dfr$ep2 <- episodes(date = dfr$admin_period, case_length =  dfr$ep_len2)

## ----warning=FALSE, fig.width=7-----------------------------------------------
dfr[c("admin_period", "ep_len1", "ep_len2", "ep1", "ep2")]
schema(dfr$ep1, seed = 2, show_labels = "length_arrow")
schema(dfr$ep2, seed = 2, show_labels = "length_arrow")

## ----warning=FALSE------------------------------------------------------------
# Wrapper function for a fixed episode
episodes_wf <- function(x){
  epids <- episodes(date = dfr$admin_period, 
                    sn = dfr$rd_id, 
                    case_length = index_window(dfr$admin_period), 
                    case_overlap_methods = x)
  return(epids)
}

# Methods 
methods <- list(
  # Identical intervals
  exact = "exact",
  # Intervals with their start or end points within another
  across = "across",
  # Intervals with aligned start points
  aligns_start = "aligns_start",
  # Intervals with aligned end points
  aligns_end = "aligns_end",
  # Intervals with start points that align with the end point of another, and vice versa
  chain = "chain",
  # Intervals occurring completely within others
  inbetween = "inbetween",
  # A combination of `chain` and `inbetween` methods
  cb1 = "chain|inbetween",
  # A combination of `exact`, `chain` and `inbetween` methods
  cb2 = "exact|chain|inbetween",
  # A combination of `across`, `chain` and `aligns_end` methods
  cb3 = "across|chain|aligns_end"
)

epids <- lapply(methods, episodes_wf)
names(epids) <- methods

# Use `schema()` to visualise each.
epids

## ----warning=FALSE------------------------------------------------------------
match_funx1 <- function(x, y) y == "RTI"
match_funx2 <- function(x, y) y == "Ward 2"
# Sub-criteria 1 - Matching source of infection OR patient location
sub_cri_1 <- sub_criteria(event_dt$attr_1, event_dt$attr_2, 
                          operator = "or", 
                          match_funcs = c(match_funx1, match_funx2))

event_dt$ep10 <- episodes(event_dt$date, case_length = 5, case_sub_criteria = sub_cri_1)

## ----warning=FALSE, fig.width=7-----------------------------------------------
schema(event_dt$ep10, seed = 2, show_labels = c("length_arrow"),
       custom_label = paste0(event_dt$attr_1, " in ", event_dt$attr_2))

## ----warning=FALSE------------------------------------------------------------
match_funx3 <- function(x, y) !(x$attr_1 == "BSI" | x$attr_2 == "Ward 1")
equal_funx <- function(x, y) TRUE
# Sub-criteria 1 - Matching source of infection OR patient location
sub_cri_2 <- sub_criteria(attrs(attr_1 = event_dt$attr_1, 
                                attr_2 = event_dt$attr_2), 
                          operator = "and", 
                          match_funcs = match_funx3, 
                          equal_funcs = equal_funx)

event_dt$ep11 <- episodes(event_dt$date, case_length = 5, case_sub_criteria = sub_cri_2)

## ----warning=FALSE, fig.width=7-----------------------------------------------
schema(event_dt$ep11, seed = 2, show_labels = c("length_arrow"),
       custom_label = paste0(event_dt$attr_1, " in ", event_dt$attr_2))

## ----warning=FALSE------------------------------------------------------------
combined_sub_cri <- sub_criteria(sub_cri_1, sub_cri_2, operator = "and")
event_dt$ep12 <- episodes(event_dt$date, case_length = 5, 
                          case_sub_criteria = combined_sub_cri)

## ----warning=FALSE, fig.width=7-----------------------------------------------
schema(event_dt$ep12, seed = 2, show_labels = c("length_arrow"),
       custom_label = paste0(event_dt$attr_1, " in ", event_dt$attr_2))

## ----warning=FALSE------------------------------------------------------------
event_dt$ep13 <- episodes(event_dt$date, episode_type = "fixed", 
                          strata = event_dt$attr_2, data_source = event_dt$attr_1, 
                          group_stats = TRUE)

as.data.frame(event_dt$ep13)

