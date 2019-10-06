## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----warning=FALSE, message=FALSE----------------------------------------
library(diyar)
library(dplyr)
data(patient_list); patient_list

# Matching forename only
cbind(patient_list, record_group(patient_list, rd_id, forename))

# Matching forename and surname
patient_list <- mutate(patient_list, cri_1 = paste(forename, surname,sep="-") )
cbind(patient_list, record_group(patient_list, rd_id, cri_1, display = FALSE))

# Note that exact matching is case sensitive. See range matching.

## ----warning=FALSE, message=FALSE----------------------------------------
cbind(patient_list, record_group(patient_list, rd_id, c(forename, surname), display = FALSE))

## ----message=FALSE, warning=FALSE----------------------------------------
patient_list <- mutate(patient_list, cri_2 = paste(surname, sex,sep="-") )
cbind(patient_list, record_group(patient_list, rd_id, c(forename, cri_2), display = FALSE))

## ----message=FALSE, warning=FALSE----------------------------------------
library(tidyr)
data(Opes); Opes

# 1 stage linkage
  # stage 1 - name, and either department, hair colour or date of birth
cbind(
  Opes,
  record_group(Opes, rd_id, name, list("s1a"=c("department","hair_colour","date_of_birth")),  display = FALSE)
  ) %>% select(-starts_with("db_"), -sn)
  
# 1 stage linkage 
  # stage 1 - name, and either department or hair colour, and date of birth 
cbind(
  Opes,
  record_group(Opes, rd_id, c(name), 
               list("s1a"=c("department","hair_colour"), 
                    "s1b"=c("date_of_birth")),  display = FALSE)
  ) %>% select(-starts_with("db_"), -sn)

# 1 stage linkage 
  # stage 1 - name, and either department or hair colour, and either day and month of birth, day and year of birth or month and year of birth date of birth
cbind(
  Opes,
  record_group(Opes, rd_id, c(name), 
               list("s1a"=c("department","hair_colour"), 
                    "s1b"=c("db_pt1","db_pt2","db_pt3")),  display = FALSE)
  ) %>% select(-date_of_birth, -sn)


# 1 stage linkage 
  # stage 1 - name, and department, and hair colour, and either day and month of birth, day and year of birth or month and year of birth date of birth 
cbind(
  Opes,
  record_group(Opes, rd_id, c(name), 
               list("s1a"=c("department"),
                    "s1c"=c("hair_colour"),
                    "s1b"=c("db_pt1","db_pt2","db_pt3")),  display = FALSE)
  ) %>% select(-starts_with("db_"), -sn)


## ---- warning=FALSE, message=FALSE---------------------------------------
# 1 stage linkage 
  # stage 1 - name, and date of birth, and department and hair colour 
cbind(
  Opes,
  record_group(Opes, rd_id, name, 
               list("s1a"=c("department"),
                    "s1b"=c("hair_colour"),
                    "s1c"=c("date_of_birth")),  display = TRUE)
  ) %>% select(-starts_with("db_"))

# 1 stage linkage 
  # stage 1 - name, and date of birth, and department and hair colour
Opes_b <- unite(Opes, cri, c(name, date_of_birth, department, hair_colour))
cbind(
  Opes_b,
  record_group(Opes_b, rd_id, c(cri),  display = TRUE)
  ) %>% select(-starts_with("db_"))

## ----message=FALSE, warning=FALSE----------------------------------------
library(lubridate)
Opes_c <- select(Opes, date_of_birth)
Opes_c$dummy_cri <- 1
Opes_c

# Match record within 3 months before or after a date
Opes_c$range <- expand_number_line(as.number_line(dmy(Opes_c$date_of_birth)), period(2, "years"), "end")
Opes_c$range@gid <- as.numeric(dmy(Opes_c$date_of_birth))
bind_cols(Opes_c,
  record_group(Opes_c, criteria = dummy_cri, sub_criteria = list(s1="range")))

# Match record within 5 years younger or older than an age
Opes_c$age <- as.numeric(round((Sys.Date() - dmy(Opes_c$date_of_birth))/365.5)) # approximate age
Opes_c$range <- as.number_line(Opes_c$age)
Opes_c$range@gid <- Opes_c$age
Opes_c$range <- expand_number_line(Opes_c$range, 5, "end")
bind_cols(Opes_c,
  record_group(Opes_c, criteria = dummy_cri, sub_criteria = list(s1="range")))

## ----warning=FALSE, message=FALSE----------------------------------------
data(patient_list_2); patient_list_2

cbind(
  patient_list,
  record_group(patient_list, rd_id, c(forename, surname, sex))
)

## ----warning=FALSE, message=FALSE----------------------------------------
patient_list_b <- patient_list_2
patient_list_b <- mutate(patient_list_b, forename = 
                           ifelse(rd_id %in% 1:3, "Nil", forename))  

# 2 stage linkage
    # Stage 1 - forename
    # Stage 2 - Surname
cbind(
  patient_list_b,
  record_group(patient_list_b, rd_id, c(forename, surname), display = FALSE)
)

# 2 stage linkage
    # Stage 1 - forename
    # Stage 2 - Surname and sex
patient_list_b <- mutate(patient_list_b, cri_2 = paste(surname,sex,sep=""))

cbind(
  patient_list_b,
  record_group(patient_list_b, rd_id, c(forename, cri_2), display = FALSE)
)


## ----warning=FALSE, message=FALSE----------------------------------------
# Using NA as the proxy for missing value
patient_list_b <- mutate(patient_list_b,forename = ifelse(forename=="Nil",NA,forename))

cbind(
  patient_list_b,
  record_group(patient_list_b, rd_id, c(forename, surname), display = FALSE)
)

# Using "" as the proxy for missing value
patient_list_b <- mutate(patient_list_b,forename = ifelse(is.na(forename),"",forename))  

cbind(
  patient_list_b,
  record_group(patient_list_b, rd_id, c(forename, surname), display = FALSE)
)


