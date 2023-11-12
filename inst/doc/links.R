## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include = FALSE---------------------------------------------------------
library(diyar)

## ----warning = FALSE----------------------------------------------------------
data(missing_staff_id)
missing_staff_id

## ----warning = FALSE----------------------------------------------------------
missing_staff_id$p1 <- links(criteria = missing_staff_id$initials)
missing_staff_id$p2 <- links(criteria = missing_staff_id$hair_colour)
missing_staff_id[c("initials", "hair_colour", "p1", "p2")]

## ----warning = FALSE----------------------------------------------------------
missing_staff_id$p3 <- links(
  criteria = as.list(missing_staff_id[c("initials", "hair_colour")])
  )
missing_staff_id[c("initials", "hair_colour", "p1", "p2", "p3")]

## ----warning = FALSE----------------------------------------------------------
scri_1 <- sub_criteria(
  missing_staff_id$hair_colour, 
  missing_staff_id$branch_office, 
  operator = "or"
  )
scri_2 <- sub_criteria(
  missing_staff_id$hair_colour, 
  missing_staff_id$branch_office, 
  operator = "and"
  )
missing_staff_id$p4 <- links(
  criteria = "place_holder", 
  sub_criteria = list(cr1 = scri_1), 
  recursive = TRUE
  )
missing_staff_id$p5 <- links(
  criteria = "place_holder", 
  sub_criteria = list(cr1 = scri_2), 
  recursive = TRUE
  )
missing_staff_id[c("hair_colour", "branch_office", "p4", "p5")]

## ----warning = FALSE----------------------------------------------------------
scri_3 <- sub_criteria(
  scri_1, 
  sub_criteria(
    missing_staff_id$initials, 
    missing_staff_id$branch_office,
    operator = "or"),
  operator = "and"
  )
missing_staff_id$p6 <- links(
  criteria = "place_holder", 
  sub_criteria = list(cr1 = scri_3), 
  recursive = TRUE
  )
missing_staff_id[c("hair_colour", "branch_office", "p4", "p5", "p6")]

## ----warning = FALSE----------------------------------------------------------
# A function to extract the last word in a string
last_word_wf <- function(x) tolower(gsub("^.* ", "", x))
# A logical test using `last_word_wf`.
last_word_cmp <- function(x, y) last_word_wf(x) == last_word_wf(y)

scri_4 <- sub_criteria(
  missing_staff_id$hair_colour, 
  missing_staff_id$branch_office,
  match_funcs = c(last_word_cmp, last_word_cmp),
  operator = "or"
  )
missing_staff_id$p7 <- links(
  criteria = "place_holder", 
  sub_criteria = list(cr1 = scri_4), 
  recursive = TRUE
  )
missing_staff_id[c("hair_colour", "branch_office", "p4", "p5", "p6", "p7")]

## ----warning = FALSE----------------------------------------------------------
dfr <- data.frame(x = 1:5)
roll_window_funx <- function(x, y){
  match <- abs(x - y) <= 2
  print(data.frame(y, x, match))
  cat("\n")
  return(match)
  }
roll_window_scri <- sub_criteria(
  dfr$x,
  match_funcs = roll_window_funx
  )

## ----warning = FALSE----------------------------------------------------------
dfr$b.p1 <- links(
  criteria = "place_holder",
  sub_criteria = list(cr1 = roll_window_scri),
  batched = "yes",
  recursive = TRUE
  )

## ----warning = FALSE----------------------------------------------------------
dfr$b.p2 <- links(
  criteria = "place_holder",
  sub_criteria = list(cr1 = roll_window_scri),
  batched = "no"
  )

## ----warning = FALSE----------------------------------------------------------
dfr$b.p3 <- links(
  criteria = "place_holder",
  sub_criteria = list(cr1 = roll_window_scri),
  batched = "semi",
  recursive = TRUE
  )
dfr

## ----warning = FALSE----------------------------------------------------------
missing_staff_id$p9 <- links_wf_probabilistic(
  attribute = list(missing_staff_id$hair_colour, 
                   missing_staff_id$branch_office), 
  cmp_func = c(last_word_cmp, last_word_cmp), 
  probabilistic = TRUE
  )
missing_staff_id[c("hair_colour", "branch_office", "p7", "p9")]

