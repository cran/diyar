## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include = FALSE---------------------------------------------------------
library(diyar)
# library(stringdist)
# library(cowplot)
# library(ggplot2)

stringdist <- function(...){
if (requireNamespace("stringdist", quietly = TRUE)) {
      stringdist::stringdist(...)
   } else {
     warning("`stringdist` package not found\nSimilarity scores were not calculated.\n`exact_match()` was used instead")
     k <- list(...)
      as.numeric(!exact_match(k[[1]], k[[2]]))
   }  
}



## ----warning = FALSE----------------------------------------------------------
## 3-stage linkage
# Attributes to be compared at each stage
attr_1 <- c(1, 1, 1, NA, NA, NA, NA, NA)
attr_2 <- c(NA, NA, 2, 2, 2, NA, NA, NA)
attr_3 <- c(NA, NA, NA, NA, 3, 3, 3, 3)
stages <- list(attr_1 = attr_1, attr_2 = attr_2, attr_3 = attr_3)
# Source of each record 
data_sets <- c("A", "A", "A", "A", "B", "B", "B", "B")
# Dataset
dfr <- cbind(as.data.frame(stages), data_sets)
# Linkage
dfr$pd1 <- links(stages, data_source = data_sets)
# Identifiers
dfr

## ----warning = FALSE----------------------------------------------------------
# Components of a `pid` identifier
as.data.frame(dfr$pd1)

## ----warning = FALSE----------------------------------------------------------
# Attribute - Names
attr_4 <- c(NA, NA, "James", "James", "Tobi", "Tobi", "Ope")
# Date of birth 
attr_5 <- c("12/04/1957", "12/04/1957", "22/06/1973", "20/01/1980",
            "12/04/1957", "12/04/1957", "12/04/1957")
# Dataset
stages_2 <- list(attr_4 = attr_4, attr_5 = attr_5)

dfr_2 <- as.data.frame(stages_2)
dfr_2$pd2 <- links(stages_2)

dfr_2

## ----warning = FALSE----------------------------------------------------------
attr_4c <- attr_4b <- c(attr_4, "Ope")
attr_4c[attr_4c == "Tobi"] <- NA
attr_5b <- c(attr_5, "13/02/1991")
# Attribute - Sex
attr_4.5 <- c(NA, NA, NA, NA, "M", "M", NA, NA)

stages_2bi <- list(attr_4b = attr_4b, attr_5b = attr_5b)
stages_2bii <- list(attr_4c = attr_4c, attr_4.5 = attr_4.5, attr_5b = attr_5b)
# Dataset 1
dfr_2b <- as.data.frame(stages_2bi)
# Linkage option 1
dfr_2b$pd2bi <- links(stages_2bi)

# Dataset 2
dfr_2c <- as.data.frame(stages_2bii)
# Linkage option 2
dfr_2c$pd2bii <- links(stages_2bii)

# Results for option 1
dfr_2b

# Results for option 2
dfr_2c

## ----warning = FALSE, include = FALSE-----------------------------------------
# plt1 <- schema(dfr_2b$pd2bi, seed = 2,
#                show_label = TRUE,
#                theme = "dark")
# plt2 <- schema(dfr_2c$pd2bii, seed = 2,
#                show_label = TRUE,
#                theme = "dark")
# 
# f <- plot_grid(plt1 + theme(plot.background = element_rect(color = "white")),
#                plt2 + theme(plot.background = element_rect(color = "white")),
#                labels = c("pd2bi", "pd2bii"),
#                label_colour = "white",
#                label_size = 12,
#                label_x = c(0, 0))
# ggsave(dpi = 100, plot = f, filename = "fig_p1.png", width = 15, height = 4.5, units = "in")

## ----warning = FALSE----------------------------------------------------------
dfr$pd1b <- links(stages, expand = FALSE)
dfr

## ----warning = FALSE, include = FALSE-----------------------------------------
# plt1 <- schema(dfr$pd1, seed = 2, theme = "dark")
# plt2 <- schema(dfr$pd1b, seed = 2, theme = "dark")
# f <- plot_grid(plt1 + theme(plot.background = element_rect(color = "white")),
#                plt2 + theme(plot.background = element_rect(color = "white")),
#                labels = c("pd1", "pd1b"),
#                label_colour = "white",
#                label_size = 12)
# ggsave(dpi = 100, plot = f, filename = "fig_p2.png", width = 15, height = 4.5, units = "in")

## ----warning = FALSE----------------------------------------------------------
# Attributes to be compared at each stage
attr_6 <- c(1, 1, 1, 1, 2, 2, 2, 2)
attr_7 <- c(NA, NA, 2, 2, NA, NA, NA, NA)
attr_8 <- c("3c", "3c", "3c", "3c", "3a", "3a", "4a", "4b")
stages_3 <- list(attr_6 = attr_6, attr_7 = attr_7, attr_8 = attr_8)

dfr_3 <- cbind(as.data.frame(stages_3))
dfr_3$pd3a <- links(stages_3, expand = FALSE)
dfr_3$pd3b <- links(stages_3, shrink = TRUE)

dfr_3

## ----warning = FALSE----------------------------------------------------------
data(Opes); Opes

# `criteria` AND matching department
sub_cri_1 <- sub_criteria(Opes$department)
# `criteria` AND matching (department OR hair_colour)
sub_cri_2 <- sub_criteria(Opes$department, Opes$hair_colour)
# `criteria` AND matching (department AND hair_colour AND date_of_birth)
sub_cri_3 <- sub_criteria(Opes$department, Opes$hair_colour, Opes$date_of_birth, operator = "and")
# `criteria` AND matching ((department OR hair_colour) AND date_of_birth)
sub_cri_4 <- sub_criteria(sub_cri_2, Opes$date_of_birth, operator = "and")
# `criteria` AND matching (any two parts of the date of birth)
sub_cri_5 <- sub_criteria(Opes$db_pt1, Opes$db_pt2, Opes$db_pt3)

Opes$pd4a <- links(criteria = Opes$name, sub_criteria = list(cr1 = sub_cri_1))
Opes$pd4b <- links(criteria = Opes$name, sub_criteria = list(cr1 = sub_cri_2))
Opes$pd4c <- links(criteria = Opes$name, sub_criteria = list(cr1 = sub_cri_3))
Opes$pd4d <- links(criteria = Opes$name, sub_criteria = list(cr1 = sub_cri_4))
Opes$pd4e <- links(criteria = list(Opes$name, Opes$name),
                     sub_criteria = list(cr1 = sub_cri_4,
                                         cr2 = sub_cri_5))

Opes[c("name", "department", "hair_colour", "date_of_birth", 
       "pd4a", "pd4b", "pd4c", "pd4d", "pd4e")]


## ----warning = FALSE----------------------------------------------------------
data(staff_records); 
dfr_4 <- staff_records[c("forename")]
dfr_4$forename

# Logical test 1 - Similarity score of 70% or more 
jw_func <- function(x, y){
  score <- 1 - stringdist(x, y, "jw")
  score > .7
}

# Logical test 2 - Matching Soundex
soundex_func <- function(x, y){
  score <- 1 - stringdist(x, y, "soundex")
  as.logical(score)
}

sub_cri_6 <- sub_criteria(dfr_4$forename, dfr_4$forename, 
                          match_funcs = c(jw_func, soundex_func),
                          operator = "or")
dfr_4$pd5 <- links(criteria = "place_holder", sub_criteria = list(cr1 = sub_cri_6))
dfr_4

## ----warning = FALSE----------------------------------------------------------
data(missing_staff_id); 
dfr_5 <- missing_staff_id[c("staff_id",  "initials", "hair_colour", "branch_office")]
dfr_5
score_range <- prob_score_range(attribute = as.list(dfr_5))
score_range

# Logical test - Matching last word in `hair_colour` and `branch_office`
last_word_wf <- function(x) tolower(gsub("^.* ", "", x))
last_word_cmp <- function(x, y) last_word_wf(x) == last_word_wf(y)
prob_pids2 <- links_wf_probabilistic(attribute = as.list(dfr_5),
                                     cmp_func = c(diyar::exact_match,
                                                  diyar::exact_match,
                                                  last_word_cmp,
                                                  last_word_cmp),
                                     score_threshold = score_range$mid_scorce)
prob_pids2

# Results for specific record pairs
prob_pids3 <- links_wf_probabilistic(attribute = as.list(dfr_5),
                                     cmp_func = c(diyar::exact_match,
                                                  diyar::exact_match,
                                                  last_word_cmp,
                                                  last_word_cmp),
                                     score_threshold = score_range$mid_scorce,
                                     id_1 = c(1, 1, 1),
                                     id_2 = c(6, 7, 4))
prob_pids3

## ----warning = FALSE----------------------------------------------------------
summary(Opes$pd4c)

## ----warning = FALSE----------------------------------------------------------
new_cri <- paste0(Opes$department, Opes$hair_colour, Opes$date_of_birth, sep = "-")
new_cri <- paste0(Opes$name, " ", new_cri)
Opes$pd4c2 <- links(criteria = new_cri)

max(Opes$pd4c2@iteration)

# Same outcome - identical identifiers 
Opes$pd4c; Opes$pd4c2

## ----warning = FALSE----------------------------------------------------------
# Logical test - Similarity score of 70% or more OR matching Soundex
jw_soundex_func <- function(x, y) jw_func(x, y) | soundex_func(x, y)

sub_cri_6b <- sub_criteria(dfr_4$forename, match_funcs = jw_soundex_func)
dfr_4$pd5b <- links(criteria = "place_holder", sub_criteria = list(cr1 = sub_cri_6b))

# Same outcome - identical identifiers
all(dfr_4$pd5 == dfr_4$pd5b)

## ----warning = FALSE----------------------------------------------------------
# Dataset 
dfr_6 <- data.frame(attr_1 = rep(1, 10))
# Logical test - Absolute difference between values is > 0
rng_1p_func <- function(x, y) abs(y - x) > 0
sub_cri_7 <- sub_criteria(dfr_6$attr_1, match_funcs = rng_1p_func)

dfr_6$pd_7a <- links("place_holder", sub_criteria = list("cr1" = sub_cri_7),
                     check_duplicates  = TRUE)
dfr_6$pd_7b <- links("place_holder", sub_criteria = list("cr1" = sub_cri_7),
                     check_duplicates  = FALSE)

# Same outcome - identical identifiers
all(dfr_6$pd_7a == dfr_6$pd_7b)

# but different number of iterations
max(dfr_6$pd_7a@iteration); max(dfr_6$pd_7b@iteration)

## ----warning = FALSE----------------------------------------------------------
# Attribute 2
dfr_6$attr_2 <- c(rep(1, 3), rep(2, 3), rep(3, 4))
# logical test - Difference between values is > 0
rng_1p_func <- function(x, y) y - x > 0

sub_cri_8 <- sub_criteria(dfr_6$attr_1, dfr_6$attr_2, match_funcs = rng_1p_func,
                          equal_funcs = diyar::exact_match)

dfr_6$pd_7c <- links("place_holder", sub_criteria = list("cr1" = sub_cri_8),
                     check_duplicates  = TRUE)
dfr_6$pd_7d <- links("place_holder", sub_criteria = list("cr1" = sub_cri_8),
                     check_duplicates  = FALSE)

# Same outcome - identical identifiers
all(dfr_6$pd_7c == dfr_6$pd_7d)

# but different number of iterations
max(dfr_6$pd_7c@iteration); max(dfr_6$pd_7d@iteration)

## ----warning = FALSE----------------------------------------------------------
# Dataset 
dfr_7 <- data.frame(attr_1 = 1:10, stringsAsFactors = FALSE)
# Logical test - Absolute difference of less than 4
rng_l4_func <- function(x, y) abs(y - x) < 4
sub_cri_9 <- sub_criteria(dfr_7$attr_1, match_funcs = rng_l4_func)

dfr_7$pd_8a <- links("place_holder", sub_criteria = list("cr1" = sub_cri_9), recursive  = TRUE)
dfr_7$pd_8b <- links("place_holder", sub_criteria = list("cr1" = sub_cri_9), recursive  = FALSE)

# Different outcomes - different identifiers
dfr_7

# and different number of iterations
max(dfr_7$pd_8a@iteration); max(dfr_7$pd_8b@iteration)

## ----warning = FALSE----------------------------------------------------------
# dataset 
attr_10 <- rep(LETTERS, 10000)
sub_cri_10 <- sub_criteria(attr_10)

# Fast
system.time(
  pd_8c <- links("place_holder", sub_criteria = list("cr1" = sub_cri_10),
                 recursive = TRUE)
)

# Faster
system.time(
  pd_8d <- links("place_holder", sub_criteria = list("cr1" = sub_cri_10),
                 recursive = FALSE)
)

# Fastest
system.time(
  pd_8e <- match(attr_10, attr_10)
)

# Same outcomes - identical identifiers
all(pd_8c == pd_8d) & all(pd_8c == pd_8e)

# and same number of iterations
max(pd_8c@iteration); max(pd_8d@iteration)

