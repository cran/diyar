## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----warning=FALSE, message=FALSE----------------------------------------
library(diyar); library(dplyr)
data(patient_list); 
dbs <- patient_list[c("forename","surname","sex")]; dbs

# 1 stage <- Matching surname only
dbs$pids_a <-record_group(dbs, criteria = surname, to_s4 = TRUE)

# 2 stage - Matching surname, then matching sex
dbs$pids_b <- record_group(dbs, criteria = c(surname, sex), display = FALSE, to_s4 = TRUE)

dbs
# Note that exact matching is case sensitive. See range matching.

## ----warning=FALSE, message=FALSE----------------------------------------
dbs$pids_c <- record_group(dbs, criteria =  c(sex, surname), display = FALSE, to_s4 = TRUE)

dbs

## ----message=FALSE, warning=FALSE----------------------------------------
dbs_2 <- patient_list; dbs_2

dbs_2$cri_2 <- paste(dbs_2$surname, dbs_2$sex,sep="-")
dbs_2$pid_d <- record_group(dbs_2, rd_id, c(forename, cri_2), display = FALSE, to_s4 = TRUE)

dbs_2

## ----message=FALSE, warning=FALSE----------------------------------------
library(tidyr)
data(Opes); Opes

# 1 stage linkage
  # stage 1 - name AND (department OR hair_colour OR date_of_birth)
Opes$pids_a <- record_group(Opes, criteria = name, 
                            sub_criteria = list(
                              "s1a"=c("department","hair_colour","date_of_birth")),
                            display = FALSE, to_s4 = TRUE)

Opes[c("name","department","hair_colour","date_of_birth","pids_a")]
  
# 1 stage linkage 
  # stage 1 - name AND ((department OR hair_colour) AND (date_of_birth)) 
Opes$pids_b <- record_group(Opes, criteria = name, 
                            sub_criteria = list(
                              "s1a"=c("department","hair_colour"),
                              "s1b"=c("date_of_birth")),
                            display = FALSE, to_s4 = TRUE)

Opes[c("name","department","hair_colour","date_of_birth","pids_b")]

# 1 stage linkage 
  # stage 1 - name AND ((department OR hair_colour) AND (dd-mm OR dd-yyyy OR mm-yyyy))
Opes$pids_c <- record_group(Opes, criteria = name, 
                            sub_criteria = list(
                              "s1a"=c("department","hair_colour"),
                              "s1b"=c("db_pt1","db_pt2","db_pt3")),
                            display = FALSE, to_s4 =TRUE)

Opes[c("name","department","hair_colour","date_of_birth","pids_c")]

# 1 stage linkage 
  # stage 1 - name AND ((department)  AND (hair_colour) AND (dd-mm OR dd-yyyy OR mm-yyyy))
Opes$pids_d <- record_group(Opes, criteria =name, 
               sub_criteria = list(
                 "s1a"=c("department"),
                 "s1c"=c("hair_colour"),
                 "s1b"=c("db_pt1","db_pt2","db_pt3")),  
               display = FALSE, to_s4 = TRUE)

Opes[c("name","department","hair_colour","date_of_birth","pids_d")]

## ---- warning=FALSE, message=FALSE---------------------------------------
# 1 stage linkage 
  # stage 1 - name AND ((department)  AND (hair_colour) AND (date_of_birth))
Opes$pids_e <- record_group(Opes, criteri = name, 
                            sub_criteria = list(
                              "s1a"=c("department"), 
                              "s1b"=c("hair_colour"), 
                              "s1c"=c("date_of_birth")),
                            display = TRUE, to_s4 = TRUE)

Opes$cri <- paste(Opes$name, Opes$date_of_birth, Opes$department, Opes$hair_colour, sep="-")

# 1 stage linkage 
  # stage 1 - name AND department AND hair_colour AND date_of_birth
Opes$pids_f <- record_group(Opes, criteria = cri,  display = TRUE, to_s4 =TRUE)

Opes[c("name","department","hair_colour","date_of_birth","pids_e","pids_f")]

## ----message=FALSE, warning=FALSE----------------------------------------
library(lubridate)
Opes_c <- Opes["date_of_birth"]
Opes_c

# Match on date of birth + 2 years
Opes_c$date_of_birth <- dmy(Opes_c$date_of_birth)
Opes_c$range_a <- expand_number_line(as.number_line(Opes_c$date_of_birth), period(2, "years"), "end")
Opes_c$range_a@gid <- as.numeric(Opes_c$date_of_birth)

Opes_c$pids_a <- record_group(Opes_c, criteria = range_a, to_s4 =TRUE)

Opes_c[c("date_of_birth","range_a","pids_a")]

# Match on age +/- 5 years
Opes_c$age <- as.numeric(round((Sys.Date() - Opes_c$date_of_birth)/365.5)) # approximate age
Opes_c$range_b <- expand_number_line(as.number_line(Opes_c$age), 5, "both")
Opes_c$range_b@gid <- Opes_c$age

Opes_c$pids_b <- record_group(Opes_c, criteria = range_b, to_s4 =TRUE)

Opes_c[c("age","range_b","pids_b")]

## ----warning=FALSE, message=FALSE----------------------------------------
data(patient_list_2); patient_list_2

patient_list_2$pids_a <- record_group(patient_list_2, rd_id, c(forename, surname, sex), to_s4 = TRUE)

patient_list_2

## ----warning=FALSE, message=FALSE----------------------------------------
patient_list_2$forename <- ifelse(patient_list_2$rd_id %in% 1:3, "Nil", patient_list_2$forename)
# 2 stage linkage
    # Stage 1 - forename
    # Stage 2 - surname

patient_list_2$pids_b <- record_group(patient_list_2, criteria = c(forename, surname), 
                                      display = FALSE, to_s4 =TRUE)

patient_list_2[c("forename","surname","pids_b")]

## ----warning=FALSE, message=FALSE----------------------------------------
# Using NA as the proxy for missing value
patient_list_2 <- mutate(patient_list_2,forename = ifelse(forename=="Nil",NA,forename))

patient_list_2$pids_d <- record_group(patient_list_2, rd_id, c(forename, surname), 
                                      display = FALSE, to_s4 = TRUE)

# Using "" as the proxy for missing value
patient_list_2 <- mutate(patient_list_2,forename = ifelse(is.na(forename),"",forename))  

patient_list_2$pids_e <- record_group(patient_list_2, rd_id, c(forename, surname), 
                                      display = FALSE, to_s4 = TRUE)

patient_list_2[c("forename","surname","pids_d","pids_e")]

## ----warning=FALSE, message=FALSE----------------------------------------
library(phonics)

patient_list_2$soundex <- soundex(patient_list_2$surname)

patient_list_2$pids_e <- record_group(patient_list_2, rd_id, c(forename, soundex), 
                                      display = FALSE, to_s4 = TRUE)

patient_list_2[c("forename","surname","soundex","pids_d","pids_e")]

