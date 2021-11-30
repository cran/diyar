## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include=FALSE------------------------------------------------------------
library(diyar)

## ----warning=FALSE------------------------------------------------------------
homes <- data.frame(member = c("son_1", "son_2", "daughter_1", 
                               "father", "mother", "grand_father", "grand_mother"), 
                    age = c(4, 6, 17, 43, 40, 74, 69))
homes

## ----warning=FALSE, fig.width=7, message=FALSE--------------------------------
age_bands <- seq(0, 69, by =17)
age_bands <- number_line(age_bands, age_bands + 16)
age_bands

homes$grp_1 <- partitions(homes$age, window = age_bands, separate = TRUE)
homes

schema(homes$grp_1, seed = 4,
       custom_label = paste0(homes$member, " \n(", homes$age, " yrs)"))

## ----warning=FALSE, fig.width=7-----------------------------------------------
homes$grp_2 <- partitions(homes$age, by = 16, separate = TRUE)
schema(homes$grp_2, seed = 4,
       custom_label = paste0(homes$member, " \n(", homes$age, " yrs)"))

## ----warning=FALSE, fig.width=7-----------------------------------------------
homes$grp_3 <- partitions(homes$age, by = 16, 
                          separate = FALSE,
                          windows_total = number_line(3, 4))
homes

schema(homes$grp_3, seed = 4,
       custom_label = paste0(homes$member, " \n(", homes$age, " yrs)"))

## ----warning=FALSE, fig.width=7-----------------------------------------------
homes$alt_age <- homes$age
lgk <- homes$member %in% c("father", "mother")
homes$alt_age[lgk] <- homes$alt_age[lgk] - 5
homes$grp_4 <- partitions(homes$alt_age, by = 16, 
                          separate = TRUE,
                          windows_total = number_line(3, 4))
homes

schema(homes$grp_4, seed = 4,
       custom_label = paste0(homes$member, " \n(", homes$alt_age, " yrs)"))

## ----warning=FALSE, fig.width=7-----------------------------------------------
homes$grp_5 <- partitions(homes$alt_age, by = 16, 
                          separate = FALSE,
                          windows_total = number_line(3, 3))

homes
schema(homes$grp_5, seed = 4,
       custom_label = paste0(homes$member, " \n(", homes$alt_age, " yrs)"))

## ----warning=FALSE, fig.width=7-----------------------------------------------
homes$grp_6 <- episodes(homes$alt_age, case_length = 16)
homes

schema(homes$grp_6, seed = 4,
       show_labels = c("length_arrow", "length_label"),
       custom_label = paste0(homes$member, " \n(", homes$alt_age, " yrs)"))

## ----warning=FALSE, fig.width=7-----------------------------------------------
as.data.frame(homes$grp_6)

homes$t3_home <- length(unique(homes$grp_6@wind_id[[1]])) == 3
homes

## ----warning=FALSE, fig.width=7-----------------------------------------------
duplicate <- rbind(homes[1:2], homes[1:2])
duplicate$house_hold <- c(rep("london", 7), rep("hull", 7))

duplicate$grp_1 <- partitions(duplicate$age, by = 16, 
                               separate = FALSE,
                               windows_total = number_line(3, 4), 
                               strata = duplicate$house_hold)
duplicate$grp_2 <- episodes(duplicate$age, 
                             case_length = 16, 
                             strata = duplicate$house_hold)

## ----warning=FALSE, fig.width=7, fig.height=8---------------------------------
duplicate
schema(duplicate$grp_1, seed = 5,
       custom_label = paste0(duplicate$member, " (", duplicate$age, " yrs) in \n", duplicate$house_hold))

## ----warning=FALSE, fig.width=7, fig.height=7---------------------------------
schema(duplicate$grp_2, seed = 4,
       show_labels = c("length_arrow", "length_label"),
       custom_label = paste0(duplicate$member, " (", duplicate$age, " yrs) in \n", duplicate$house_hold))

