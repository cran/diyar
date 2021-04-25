## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include=FALSE------------------------------------------------------------
library(diyar)
# library(cowplot)
# library(ggplot2)
# date <- function(x) as.Date(x, "%d/%m/%Y")
# dttm <- function(x) as.POSIXct(x, "UTC", format = "%d/%m/%Y %H:%M:%S")

## ----warning=FALSE------------------------------------------------------------
# Events
event_dt <- seq(from = as.Date("2021-01-01"), to = as.Date("2021-01-11"), by = 1)
s_data <- data.frame(date = event_dt)
# Attribute 1 - Source of infection
attr_1 <- c("BSI", "UTI", "RTI", "RTI", "BSI", "BSI", "BSI", "RTI", "RTI", "BSI", "RTI")
# Attribute 2 - Location 
attr_2 <- c("Ward 1", "Ward 1", "Ward 3", "Ward 3", "Ward 2", "Ward 2", 
            "Ward 1", "Ward 1", "Ward 3","Ward 3", "Ward 2")
s_data$attr <- attr_1
# Fixed episodes
s_data$ep1 <- episodes(event_dt, case_length = 5, episode_type = "fixed")
# Rolling episodes
s_data$ep2 <- episodes(event_dt, case_length = 5, episode_type = "rolling",
                       group_stats = TRUE, data_source = attr_1)
# Recursive episodes
s_data$ep3 <- episodes(event_dt, case_length = 5, episode_type = "recursive")
# Panes
s_data$pn1 <- partitions(event_dt, length.out = 2, separate = TRUE)

# Identifiers
s_data

## ----warning=FALSE------------------------------------------------------------
# Components of an episode identifier
as.data.frame(s_data$ep2)

## ----warning=FALSE, include=FALSE---------------------------------------------
# plt1 <- schema(s_data$ep1, seed = 2, show_label = c("length_label", "length_arrow"), theme = "light")
# plt2 <- schema(s_data$ep2, seed = 2, show_label = c("length_label", "length_arrow"), theme = "light")
# plt3 <- schema(s_data$ep3, seed = 2, show_label = c("length_label", "length_arrow"), theme = "light")
# plt4 <- schema(s_data$pn1, seed = 2, show_label = c("window_label"), theme = "light")
# f <- plot_grid(plt1 + theme(plot.background = element_rect(color = "black")),
#                plt2 + theme(plot.background = element_rect(color = "black")),
#                plt3 + theme(plot.background = element_rect(color = "black")),
#                plt4 + theme(plot.background = element_rect(color = "black")),
#                labels = c("Fixed episodes",
#                           "Rolling episodes",
#                           "Recursive episodes",
#                           "Panes"),
#                label_colour = "black",
#                label_size = 12,
#                label_x = c(-.065, -.065, -.0615, -.015))
# ggsave(dpi = 100, plot = f, filename = "fig_e1.png", width = 15, height = 9, units = "in")

## ----warning=FALSE------------------------------------------------------------
# Matching clinical criteria
ep1 <- episodes(event_dt, strata = attr_1, case_length = 5)
# Matching geographical criteria
ep2 <- episodes(event_dt, strata = attr_2, case_length = 5)

## ----warning=FALSE, include=FALSE---------------------------------------------
# plt1 <- schema(ep1, seed = 2,
#                show_label = "sn",
#                custom_label = attr_1, theme = "light")
# plt2 <- schema(ep2, seed = 2,
#                show_label = "sn",
#                custom_label = attr_2, theme = "light")
# 
# f <- plot_grid(plt1 + theme(plot.background = element_rect(color = "black")),
#                plt2 + theme(plot.background = element_rect(color = "black")),
#                labels = c("ep1", "ep2"),
#                label_colour = "black",
#                label_size = 12,
#                label_x = c(0, 0))
# ggsave(dpi = 100, plot = f, filename = "fig_e2.png", width = 15, height = 4.5, units = "in")

## ----warning=FALSE------------------------------------------------------------
# Attribute 3 - Patient sex
attr_3 <- c(rep("Female", 9), "Male", "Female")

# Sub-criteria 1 - Matching source of infection OR patient location
sub_cri_1 <- sub_criteria(attr_1, attr_2, operator = "or")
# Sub-criteria 2 - Matching source of infection AND patient location
sub_cri_2 <- sub_criteria(attr_1, attr_2, operator = "and")
# Sub-criteria 3 - (Matching source of infection AND patient location) OR (Matching patient sex)
sub_cri_3 <- sub_criteria(sub_cri_2, attr_3, operator = "or")
# Sub-criteria 4 - (Matching source of infection AND patient location) AND (Matching patient sex)
sub_cri_4 <- sub_criteria(sub_cri_2, attr_3, operator = "and")

ep3 <- episodes(event_dt, case_length = 5, case_sub_criteria = sub_cri_1)
ep4 <- episodes(event_dt, case_length = 5, case_sub_criteria = sub_cri_2)
ep5 <- episodes(event_dt, case_length = 5, case_sub_criteria = sub_cri_3)
ep6 <- episodes(event_dt, case_length = 5, case_sub_criteria = sub_cri_4)

## ----warning=FALSE, include=FALSE---------------------------------------------
# plt1 <- schema(ep1, seed = 2,
#                show_label = FALSE,
#                custom_label = paste0(attr_1, " in ", attr_3, " at ", attr_2), theme = "light")
# plt2 <- schema(ep2, seed = 2,
#                show_label = FALSE,
#                custom_label = paste0(attr_1, " in ", attr_3, " at ", attr_2), theme = "light")
# plt3 <- schema(ep3, seed = 2,
#                show_label = FALSE,
#                custom_label = paste0(attr_1, " in ", attr_3, " at ", attr_2), theme = "light")
# plt4 <- schema(ep4, seed = 2,
#                show_label = FALSE,
#                custom_label = paste0(attr_1, " in ", attr_3, " at ", attr_2), theme = "light")
# 
# f <- plot_grid(plt1 + theme(plot.background = element_rect(color = "black")),
#                plt2 + theme(plot.background = element_rect(color = "black")),
#                plt3 + theme(plot.background = element_rect(color = "black")),
#                plt4 + theme(plot.background = element_rect(color = "black")),
#                labels = c("ep3", "ep4", "ep5", "ep6"),
#                label_colour = "black",
#                label_size = 12,
#                label_x = c(0, 0))
# ggsave(dpi = 100, plot = f, filename = "fig_e3.png", width = 15, height = 9, units = "in")

## ----warning=FALSE------------------------------------------------------------
# record id
rd_id <- 1:length(attr_1)

# Condition 1 - Each episode must include BSI events
cri_funx_1 <- function(x, y){
  splts <- split(x$attr, y$rd_id)
  splts_lgk <- lapply(splts, function(x){
    "RTI" %in% x
  })
  splts_lgk <- unlist(splts_lgk)
  splts_lgk[match(y$rd_id, names(splts))]
}

# Condition 2 - Each episode must include >=3 different sources of infection
cri_funx_2 <- function(x, y){
  splts <- split(x$attr, y$rd_id)
  splts_lgk <- lapply(splts, function(x){
    length(x[!duplicated(x)]) >= 3
  })
  splts_lgk <- unlist(splts_lgk)
  splts_lgk[match(y$rd_id, names(splts))]
}

# Equivalence - Logical test for matching attributes
eqv_funx <- function(x, y){
  x$rd_id == y$rd_id
}

# Sub-criteria 
sub_cri_5 <- sub_criteria(list(attr = attr_1, rd_id= rd_id), match_funcs = cri_funx_1, 
                          equal_funcs = eqv_funx)

sub_cri_6 <- sub_criteria(list(attr = attr_1, rd_id= rd_id), match_funcs = cri_funx_2,
                          equal_funcs = eqv_funx)

ep7 <- episodes(event_dt, case_length = 2, episode_type = "fixed", 
case_sub_criteria = sub_cri_5)

ep8 <- episodes(event_dt, case_length = 2, episode_type = "fixed",
                case_sub_criteria = sub_cri_6)

## ----warning=FALSE, include=FALSE---------------------------------------------
# plt1 <- schema(ep7, seed = 2,
#                show_label = "sn",
#                custom_label = attr_1, theme = "light")
# plt2 <- schema(ep8, seed = 2,
#                show_label = "sn",
#                custom_label = attr_1, theme = "light")
# 
# f <- plot_grid(plt1 + theme(plot.background = element_rect(color = "black")),
#                plt2 + theme(plot.background = element_rect(color = "black")),
#                labels = c("ep7", "ep8"),
#                label_colour = "black",
#                label_size = 12,
#                label_x = c(0, 0))
# ggsave(dpi = 100, plot = f, filename = "fig_e4.png", width = 15, height = 4.5, units = "in")

## ----warning=FALSE------------------------------------------------------------
# Group events into 2 equal parts over the strata's duration
pn2 <- partitions(event_dt, length.out = 2, separate = TRUE)

# Group events into 3-day sequences over the strata's duration
pn3 <- partitions(event_dt, by = 3, separate = TRUE)

# Group events that occured in a specified period of time 
pn4 <- partitions(event_dt, window = number_line(event_dt[4], event_dt[7]))

# Group events from separate periods into one pane
pn5 <- partitions(event_dt, length.out = 2, separate = FALSE)

## ----warning=FALSE, include=FALSE---------------------------------------------
# plt1 <- schema(pn2, seed = 2, show_label = c("window_label", "case_nm"), theme = "light")
# plt2 <- schema(pn3, seed = 2, show_label = c("window_label", "case_nm"), theme = "light")
# plt3 <- schema(pn4, seed = 2, show_label = c("window_label", "case_nm"), theme = "light")
# plt4 <- schema(pn5, seed = 2, show_label = c("window_label", "case_nm"), theme = "light")
# 
# f <- plot_grid(plt1 + theme(plot.background = element_rect(color = "black")),
#                plt2 + theme(plot.background = element_rect(color = "black")),
#                plt3 + theme(plot.background = element_rect(color = "black")),
#                plt4 + theme(plot.background = element_rect(color = "black")),
#                labels = c("pn2", "pn3", "pn4", "pn5"),
#                label_colour = "black",
#                label_y = c(0.07,.07,0.07,0.07))
# 
# ggsave(dpi = 100, plot = f, filename = "fig_e5.png", width = 15, height = 9, units = "in")

## ----warning=FALSE------------------------------------------------------------
# Preference for selecting index events
c_sort <- c(rep(2, 5), 1, rep(2, 5))
# Episodes are 6 days (5-day difference) after the earliest event 
ep9 <- episodes(event_dt, case_length = 5, episodes_max = 1)
# Episodes are 6 days (5-day difference) before the most recent event 
ep10 <- episodes(event_dt, case_length = 5, episodes_max = 1, from_last = TRUE)
# Episodes are 6 days (5-day difference) after the 6th event 
ep11 <- episodes(event_dt, case_length = 5, custom_sort = c_sort, episodes_max = 1)
# Episodes are 6 days (5-day difference) before or after the 6th event 
ep12 <- episodes(event_dt, case_length = number_line(-5, 5), custom_sort = c_sort, episodes_max = 1)

## ----warning=FALSE, include=FALSE---------------------------------------------
# plt1 <- schema(ep9, seed = 2, show_label = c("length_label", "length_arrow", "case_nm"), theme = "light")
# plt2 <- schema(ep10, seed = 2, show_label = c("length_label", "length_arrow", "case_nm"), theme = "light")
# plt3 <- schema(ep11, seed = 2, show_label = c("length_label", "length_arrow", "case_nm"), theme = "light")
# plt4 <- schema(ep12, seed = 2, show_label = c("length_label", "length_arrow", "case_nm"), theme = "light")
# 
# f <- plot_grid(plt1 + theme(plot.background = element_rect(color = "black")),
#                plt2 + theme(plot.background = element_rect(color = "black")),
#                plt3 + theme(plot.background = element_rect(color = "black")),
#                plt4 + theme(plot.background = element_rect(color = "black")),
#                labels = c("ep9", "ep10", "ep11", "ep12"), label_colour = "black")
# ggsave(dpi = 100, plot = f, filename = "fig_e6.png", width = 15, height = 9, units = "in")

## ----warning=FALSE------------------------------------------------------------
# Episodes are 4 days (3-day difference) after the earliest event with
# repeat occurrence within 4 days of the last event considered recurrences not duplicates
ep13 <- episodes(event_dt, case_length = 3, episode_type = "rolling")
# Episodes are 4 days (3-day difference) after the earliest event with
# repeat occurrence within 7 days of the last event considered recurrences not duplicates
ep14 <- episodes(event_dt, case_length = 3,  recurrence_length = 6, episode_type = "rolling")
# Episodes are 3 days (2-day difference) after the earliest event with
# repeat occurrence within 6 days of the first event considered recurrences not duplicates
ep15 <- episodes(event_dt, case_length = 2,  recurrence_length = 5, 
                episode_type = "rolling", reference_event = "first_record")
# Episodes are 2 days (1-day difference) after the earliest event with
# repeat occurrence within 4 days of the last event considered recurrences not duplicates and
# the possibility of each repeat occurrence spawning a new occurrence as if it was the initial case
ep16 <- episodes(event_dt, case_length = 1,  recurrence_length = 3, 
                episode_type = "rolling", case_for_recurrence = TRUE)
# Episodes are 2 days (1-day difference) after the earliest event with
# repeat occurrence within 4 days of the last event considered recurrences not duplicates and
# can't recur more than twice
ep17 <- episodes(event_dt, case_length = 1,  recurrence_length = 3, 
                episode_type = "rolling", rolls_max = 2)
# Episodes are 2 days (1-day difference) after the earliest event with
# repeat occurrence within 4 days of the last event considered recurrences not duplicates and
# can't recur more than once times and the selection of index events is recursive
ep18 <- episodes(event_dt, case_length = 1,  recurrence_length = 3, 
                episode_type = "recursive", rolls_max = 1)

## ----warning=FALSE, include=FALSE---------------------------------------------
# plt1 <- schema(ep13, seed = 2, show_label = c("length_label", "length_arrow"), theme = "light")
# plt2 <- schema(ep14, seed = 2, show_label = c("length_label", "length_arrow"), theme = "light")
# plt3 <- schema(ep15, seed = 2, show_label = c("length_label", "length_arrow"), theme = "light")
# plt4 <- schema(ep16, seed = 2, show_label = c("length_label", "length_arrow"), theme = "light")
# plt5 <- schema(ep17, seed = 2, show_label = c("length_label", "length_arrow"), theme = "light")
# plt6 <- schema(ep18, seed = 2, show_label = c("length_label", "length_arrow"), theme = "light")
# 
# f <- plot_grid(plt1 + theme(plot.background = element_rect(color = "black")),
#                plt2 + theme(plot.background = element_rect(color = "black")),
#                plt3 + theme(plot.background = element_rect(color = "black")),
#                plt4 + theme(plot.background = element_rect(color = "black")),
#                plt5 + theme(plot.background = element_rect(color = "black")),
#                plt6 + theme(plot.background = element_rect(color = "black")),
#                labels = c("ep13", "ep14", "ep15", "ep16", "ep17", "ep18"),
#                label_colour = "black")
# ggsave(dpi = 100, plot = f, filename = "fig_e7.png", width = 15, height = 9, units = "in")

## ----warning=FALSE------------------------------------------------------------
# Each episodes requires at least 5 temporal links
ep19 <- episodes(event_dt,
                case_length = list(number_line(2, 2),
                                   number_line(5, 7),
                                   number_line(9, 20)),
                episode_type = "fixed", case_length_total = 5, skip_if_b4_lengths = TRUE)

# Each episodes requires at least 2 temporal links
ep20 <- episodes(event_dt,
                case_length = list(number_line(2, 2),
                                   number_line(5, 7),
                                   number_line(9, 20)),
                episode_type = "fixed", case_length_total = 2, skip_if_b4_lengths = TRUE)


## ----warning=FALSE, include=FALSE---------------------------------------------
# plt1 <- schema(ep19, seed = 2,
#                show_label = c("length_label", "length_arrow", "case_nm"),
#                theme = "light")
# plt2 <- schema(ep20, seed = 2,
#                show_label = c("length_label", "length_arrow", "case_nm"),
#                theme = "light")
# 
# f <- plot_grid(plt1 + theme(plot.background = element_rect(color = "black")),
#                plt2 + theme(plot.background = element_rect(color = "black")),
#                labels = c("ep19", "ep20"),
#                label_colour = "black",
#                label_size = 12,
#                label_x = c(0, 0))
# ggsave(dpi = 100, plot = f, filename = "fig_e8.png", width = 15, height = 4.5, units = "in")

## ----warning=FALSE------------------------------------------------------------
# Dummy data of hospital stays
dfr <- diyar::hospital_admissions[c("admin_dt", "discharge_dt")]
dfr$admin_period <- number_line(dfr$admin_dt, dfr$discharge_dt)
                                  
# Group overlapping hospital stays
dfr$ep_len1 <- index_window(dfr$admin_period)
dfr$ep21 <- episodes(date = dfr$admin_period, case_length = dfr$ep_len1)

# Group overlapping hospital stays and those within 21 days of the end point of an index hospital stay 
dfr$ep_len2 <- expand_number_line(index_window(dfr$admin_period), 20, "right")
dfr$ep22 <- episodes(date = dfr$admin_period, case_length =  dfr$ep_len2)

dfr[c("admin_period", "ep_len1", "ep_len2", "ep21", "ep22")]

## ----warning=FALSE, include=FALSE---------------------------------------------
# plt1 <- schema(dfr$ep21, seed = 2,
#                show_label = c("length_label", "length_arrow"), theme = "light")
# plt2 <- schema(dfr$ep22, seed = 2,
#                show_label = c("length_label", "length_arrow"), theme = "light")
# 
# f <- plot_grid(plt1 + theme(plot.background = element_rect(color = "black")),
#                plt2 + theme(plot.background = element_rect(color = "black")),
#                labels = c("ep21", "ep22"),
#                label_colour = "black",
#                label_size = 12,
#                label_x = c(0, 0))
# ggsave(dpi = 100, plot = f, filename = "fig_e9.png", width = 15, height = 4.5, units = "in")

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

epids$exact

## ----warning=FALSE, include=FALSE---------------------------------------------
# epids_schema <- lapply(epids, function(x){
#   schema(x, seed = 2, show_label = FALSE, theme = "light")
# })
# 
# f <- plot_grid(epids_schema[[1]] + theme(plot.background = element_rect(color = "black")),
#           epids_schema[[2]] + theme(plot.background = element_rect(color = "black")),
#           epids_schema[[3]] + theme(plot.background = element_rect(color = "black")),
#           epids_schema[[4]] + theme(plot.background = element_rect(color = "black")),
#           epids_schema[[5]] + theme(plot.background = element_rect(color = "black")),
#           epids_schema[[6]] + theme(plot.background = element_rect(color = "black")),
#           epids_schema[[7]] + theme(plot.background = element_rect(color = "black")),
#           epids_schema[[8]] + theme(plot.background = element_rect(color = "black")),
#           epids_schema[[9]] + theme(plot.background = element_rect(color = "black")),
#           labels = methods, label_colour = "black", ncol = 3,
#           label_x = c(.8,.75,.62,.65,.8,.65,.5,.32,.25))
# ggsave(dpi = 100, plot = f, filename = "fig_e10.png", width = 15, height = 9, units = "in")

## ----warning=FALSE------------------------------------------------------------
summary(ep4)

## ----warning=FALSE------------------------------------------------------------
ep4b <- episodes(event_dt, case_length = 5, strata = paste0(attr_1, " ", attr_2))

summary(ep4b)

# Identical identifiers 
all(ep4 == ep4b)

## ----warning=FALSE------------------------------------------------------------
dup_events <- rep(event_dt, 5)
# Fast
system.time(
  ep23a <- episodes(dup_events, case_length = 5, episode_type = "recursive")  
)
# Faster
system.time(
  ep23b <- episodes(dup_events, case_length = 5, episode_type = "rolling", 
                  reference_event = "last_event")  
)
# Fastest
system.time(
  ep23c <- episodes(dup_events, case_length = 5, episode_type = "rolling", 
                  reference_event = "last_record")  
)

## ----warning=FALSE, include=FALSE---------------------------------------------
# plt1 <- schema(ep23a, seed = 2, show_label = c("length_label", "length_arrow"), theme = "light")
# plt2 <- schema(ep23b, seed = 2, show_label = c("length_label", "length_arrow"), theme = "light")
# plt3 <- schema(ep23c, seed = 2, show_label = c("length_label", "length_arrow"), theme = "light")
# f <- plot_grid(plt1 + theme(plot.background = element_rect(color = "black")),
#                plt2 + theme(plot.background = element_rect(color = "black")),
#                plt3 + theme(plot.background = element_rect(color = "black")),
#                labels = c("Fast (ep23a)",
#                           "Faster (ep23b)",
#                           "Fastest (ep23c)"),
#                label_colour = "black",
#                label_size = 12,
#                label_x = c(0.6, 0.6, 0.6),
#                label_y = c(0.07, 0.07, 0.07),
#                nrow = 1)
# ggsave(dpi = 100, plot = f, filename = "fig_e11.png", width = 15, height = 4.5, units = "in")

## ----warning=FALSE------------------------------------------------------------
# 10,000 duplicate events from the same `strata` e.g. patient or location
dup_events2 <- rep(event_dt, 10000)
# Fast
system.time(
  ep24a <- episodes(dup_events2, case_length = 5, episode_type = "fixed")  
)
# Faster
system.time(
  {
    # Duplicate events not included in analysis 
    ep24b <- episodes(event_dt, case_length = 5, episode_type = "fixed")
    # Results recycled for the duplicate events
    ep24b <- ep24b[match(dup_events2, event_dt)]
    }
)

# Same outcomes - identical identifiers
all(ep24a == ep24b)

# and same number of iterations
max(ep24a@iteration); max(ep24b@iteration)

## ----warning=FALSE------------------------------------------------------------
# Attribute 4 - Occurrence
attr_4 <- c(rep("Common", 5), "Rare", rep("Common", 12), "Rare", rep("Common", 3))
attr_4 <- factor(attr_4, levels = c("Rare", "Common"))

dup_events_3 <- rep(event_dt, 2)
ep25a <- episodes(dup_events_3, case_length = 0, custom_sort = attr_4, data_source = attr_4)
summary(ep25a)

## ----warning=FALSE------------------------------------------------------------
ep25b <- episodes(dup_events_3, case_length = 0, custom_sort = attr_4, skip_order = 1, 
                  data_source = attr_4)

summary(ep25b)

## ----warning=FALSE, include=FALSE---------------------------------------------
# plt1 <- schema(ep25a, seed = 2,
#                show_label = FALSE,
#                custom_label = attr_4, theme = "light")
# plt2 <- schema(ep25b, seed = 2,
#                show_label = FALSE,
#                custom_label = attr_4, theme = "light")
# 
# f <- plot_grid(plt1 + theme(plot.background = element_rect(color = "black")),
#                plt2 + theme(plot.background = element_rect(color = "black")),
#                labels = c("ep24a", "ep24b"),
#                label_colour = "black",
#                label_size = 12,
#                label_x = c(0, 0))
# ggsave(dpi = 100, plot = f, filename = "fig_e12.png", width = 15, height = 4.5, units = "in")

## ----warning=FALSE, message=FALSE---------------------------------------------
dbs <- diyar::hourly_data

# Each unit is relative to a predefined number of seconds. 
diyar::episode_unit

# 1-day fixed episodes
episodes(date = dbs$datetime,  case_length = 1, episode_unit = "days", group_stats = TRUE)

# 5-hr fixed episodes
episodes(date = dbs$datetime, case_length = 5, episode_unit = "hours", group_stats = TRUE)

## ----warning=FALSE, message=FALSE---------------------------------------------
dbs <- diyar::infections[c("date", "infection")]; dbs

# Familiar unique record ids use for the identifiers - optional
dbs$rd_id <- c(640, 17, 58, 21, 130, 79, 45, 300, 40, 13, 31)

# `strata` based on matching sources of infection
dbs$pids <- links(sn = dbs$rd_id, criteria = dbs$infection)

dbs$epids <- episodes(sn = dbs$rd_id, date = dbs$date, strata = dbs$pids, case_length = 10)

dbs

## ----warning=FALSE, message=FALSE---------------------------------------------
vals <- c(8.1, 6, 12, 8.5, 12, 3, 8, 15, 5, 7)
vals

episodes(date = vals, case_length = .5, group_stats = TRUE)

episodes(date = vals, case_length = 5, group_stats = TRUE)

episodes(date = vals, case_length = 100, group_stats = TRUE)

## ----warning=FALSE, message=FALSE---------------------------------------------
vals <- 1:10
episodes(date = vals, case_length = Inf)

episodes(date = vals, case_length = NA_real_)

