## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include=FALSE------------------------------------------------------------
# library(cowplot)
library(ggplot2)
# `schema` theme
theme <- "light" # dark vs light
bd_line <- ifelse(theme == "dark", "white", "black")

## ----warning = FALSE----------------------------------------------------------
library(diyar)
data(missing_staff_id)
dfr_stages <- missing_staff_id[c("age", "hair_colour", "branch_office", "source_1")]
# Matching `hair_colour` before matching `branch_office`
dfr_stages$p1 <- links(as.list(dfr_stages[c("hair_colour", "branch_office")]))
# Matching `branch_office` before matching `hair_colour`
dfr_stages$p2 <- links(as.list(dfr_stages[c("branch_office", "hair_colour")]))
dfr_stages

## ----warning = FALSE----------------------------------------------------------
# A function to extract the last word in a string
last_word_wf <- function(x) tolower(gsub("^.* ", "", x))
# A logical test using `last_word_wf`.
last_word_cmp <- function(x, y) last_word_wf(x) == last_word_wf(y)
# sub_critria` objects
s_cri_a <- sub_criteria(dfr_stages$hair_colour, dfr_stages$branch_office, 
                        match_funcs = c(last_word_cmp, last_word_cmp), operator = "or")
s_cri_b <- sub_criteria(dfr_stages$hair_colour, dfr_stages$branch_office, 
                        match_funcs = c(last_word_cmp, last_word_cmp), operator = "and")

dfr_stages$p3 <- links("place_holder", sub_criteria = list("cr1" = s_cri_a), recursive = TRUE)
dfr_stages$p4 <- links("place_holder", sub_criteria = list("cr1" = s_cri_b), recursive = TRUE)

## ----warning = FALSE----------------------------------------------------------
s_cri_nested_1 <- sub_criteria(s_cri_a, s_cri_b, operator = "or")
s_cri_nested_2 <- sub_criteria(dfr_stages$source_1, s_cri_b, operator = "or")
dfr_stages$p5 <- links(list("place_holder", dfr_stages$age), 
                   sub_criteria = list("cr1" = s_cri_nested_1,
                                       "cr2" = s_cri_nested_2))

## ----warning = FALSE----------------------------------------------------------
# Results
dfr_stages

## ----warning=FALSE------------------------------------------------------------
dfr_5 <- missing_staff_id[c(2, 4, 5, 6)]

# Using string comparators
# For example, matching last word in `hair_colour` and `branch_office`
last_word_wf <- function(x) tolower(gsub("^.* ", "", x))
last_word_cmp <- function(x, y) last_word_wf(x) == last_word_wf(y)

p6a <- link_records(dfr_5, attr_threshold = 1,
                           cmp_func = c(exact_match, exact_match,
                                        last_word_cmp, last_word_cmp),
                           score_threshold = -4)
# Group identifiers
p6a$pid

# Weights
subset(p6a$pid_weights, record.match)

## ----warning=FALSE------------------------------------------------------------
p6b <- links_wf_probabilistic(dfr_5, attr_threshold = 1,
                              cmp_func = c(exact_match, exact_match,
                                           last_word_cmp, last_word_cmp),
                              score_threshold = -4,
                              recursive = TRUE)
# Group identifiers
p6b$pid

# Weights
subset(p6b$pid_weights, record.match)

## ----warning = FALSE----------------------------------------------------------
dfr_2 <- c(1:5, 10:15, 20:25)
dfr_2 <- data.frame(date = as.Date("2020-01-01") + dfr_2)
dfr_2$ep1 <- episodes(dfr_2$date, case_length = 5, episode_type = "fixed")
dfr_2$ep2 <- episodes(dfr_2$date, case_length = 5, episode_type = "rolling")
dfr_2$ep3 <- episodes(dfr_2$date, case_length = 5, episode_type = "recursive")
dfr_2

## ----warning=FALSE, include=FALSE---------------------------------------------
# plt1 <- schema(dfr_2$ep1, seed = 2, show_label = c("length_arrow", "length_label"), theme = theme)
# plt2 <- schema(dfr_2$ep2, seed = 2, show_label = c("length_arrow", "length_label"), theme = theme)
# plt3 <- schema(dfr_2$ep3, seed = 2, show_label = FALSE, theme = theme)
# 
# f <- plot_grid(plt1 + theme(plot.background = element_rect(color = bd_line)), 
#                plt2 + theme(plot.background = element_rect(color = bd_line)),
#                plt3 + theme(plot.background = element_rect(color = bd_line)),
#                labels = c("Fixed episodes (ep1)", 
#                           "Rolling episodes (ep2)", 
#                           "Recursive episodes (ep3)"), 
#                label_colour = bd_line,
#                label_size = 11,
#                ncol = 1,
#                label_x = c(-.05, -.05, -.05),
#                label_y = c(.15, .15, .15))
# 
# ggsave(dpi = 100, plot = f, filename = "fig_o1.png", width = 12, height = 8, units = "in")

## ----warning=FALSE------------------------------------------------------------
event_dt <- seq(from = as.Date("2021-01-01"), to = as.Date("2021-01-11"), by = 1)
dfr_3 <- data.frame(date = event_dt)

# Group events into 2 equal parts per `strata`.
dfr_3$pn2 <- partitions(event_dt, length.out = 2, separate = TRUE)
# Group events into 3-day sequences per `strata`.
dfr_3$pn3 <- partitions(event_dt, by = 3, separate = TRUE)
# Group events into a specified period of time in each `strata`.
dfr_3$pn4 <- partitions(event_dt, window = number_line(event_dt[4], event_dt[7]))
# Group events from separate periods into one pane.
dfr_3$pn5 <- partitions(event_dt, length.out = 2, separate = FALSE)

## ----warning=FALSE, include=FALSE---------------------------------------------
# plt1 <- schema(dfr_3$pn2, seed = 2, show_label = "window_label", theme = theme)
# plt2 <- schema(dfr_3$pn3, seed = 2, show_label = "window_label", theme = theme)
# plt3 <- schema(dfr_3$pn4, seed = 2, show_label = "window_label", theme = theme)
# plt4 <- schema(dfr_3$pn5, seed = 2, show_label = "window_label", theme = theme)
# 
# f <- plot_grid(plt1 + theme(plot.background = element_rect(color = bd_line)), 
#                plt2 + theme(plot.background = element_rect(color = bd_line)),
#                plt3 + theme(plot.background = element_rect(color = bd_line)),
#                plt4 + theme(plot.background = element_rect(color = bd_line)),
#                labels = c("pn2", "pn3", "pn4", "pn5"), 
#                label_colour = bd_line,
#                label_size = 16,
#                label_y = c(0.1,.1,0.1,0.1))
# 
# ggsave(dpi = 100, plot = f, filename = "fig_o2.png", width = 15, height = 8, units = "in")

## ----warning=FALSE------------------------------------------------------------
data(hospital_admissions)
dfr_4 <- hospital_admissions[c("admin_dt", "discharge_dt")]
dfr_4$admin_period <- number_line(dfr_4$admin_dt, dfr_4$discharge_dt)
                                  
# Group overlapping hospital stays
dfr_4$nl1 <- index_window(dfr_4$admin_period)
dfr_4$ep4 <- episodes(date = dfr_4$admin_period, case_length = dfr_4$nl1)

# Group overlapping hospital stays and those within 21 days of the end point of an index hospital stay 
dfr_4$nl2 <- expand_number_line(index_window(dfr_4$admin_period), 20, "right")
dfr_4$ep5 <- episodes(date = dfr_4$admin_period, case_length =  dfr_4$nl2)

## ----warning=FALSE------------------------------------------------------------
s_cri_c <- sub_criteria(dfr_4$admin_period, match_funcs = overlaps)
dfr_4$pd5 <- links("place_holder", sub_criteria = list("cr1" = s_cri_c), recursive = TRUE)
# Results
dfr_4[c("admin_period", "nl1", "nl2", "ep4", "ep5", "pd5")]

## ----warning=FALSE------------------------------------------------------------
prs_1 <- make_pairs(LETTERS[1:4], repeats_allowed = FALSE, permutations_allowed = FALSE)
prs_2 <- make_pairs(LETTERS[1:4], repeats_allowed = TRUE, permutations_allowed = TRUE)
prs_3 <- make_pairs(1:5000, repeats_allowed = TRUE, permutations_allowed = TRUE)

str(prs_1)
str(prs_2)
str(prs_3)

## ----warning=FALSE------------------------------------------------------------
cmbi_dfr <- dfr_stages[c(1, 2)]
cmbi_dfr$combi_nm <- paste(cmbi_dfr[[1]], cmbi_dfr[[2]])
cmbi_dfr$combi_cd <- combi(as.list(cmbi_dfr))
cmbi_dfr

## ----warning=FALSE------------------------------------------------------------
summary(dfr_4$ep4)

summary(dfr_4$pd5)

## ----warning=FALSE------------------------------------------------------------
dfr_stages$p4b <- links(criteria = combi(last_word_wf(dfr_stages$hair_colour),
                                         last_word_wf(dfr_stages$branch_office)))

summary(dfr_stages$p4b)

all(dfr_stages$p4 == dfr_stages$p4b)

