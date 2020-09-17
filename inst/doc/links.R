## ----setup, include = F-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = T,
  comment = "#>"
)

## ----include=F----------------------------------------------------------------
plot_pid <- diyar:::plot_pid

## ----warning=F, message=F-----------------------------------------------------
library(diyar);
data(patient_list); 
dbs <- patient_list[c("forename","surname","sex")]; dbs

# 1 stage <- Matching surname only
dbs$pids_a <- links(criteria = dbs$surname, display = "none", group_stats = T)

# 2 stage - Matching surname, then matching sex
dbs$pids_b <- links(criteria = list(dbs$surname, dbs$sex), display = "none")

dbs

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
cat("to_df(`pid`)")
to_df(dbs$pids_a)

## ----warning=F, message=F-----------------------------------------------------
dbs2 <- patient_list[c("forename","surname","sex")]; dbs2

# Matching `sex`, and `forename` initials
lgl_1 <- function(x, y) substr(x, 1, 1) == substr(y, 1, 1) 
dbs2$pids_1 <- links(criteria =  list(dbs2$sex), 
                     sub_criteria = list(
                       cr1 = sub_criteria(dbs2$forename, funcs = lgl_1)),
                     display = "none")

# Matching `sex`, and `forename` with character length of 5
lgl_2 <- function(x, y) nchar(x) == nchar(y) & nchar(x) == 5
dbs2$pids_2 <- links(criteria =  list(dbs2$sex), 
                    sub_criteria = list(
                      cr1 = sub_criteria(dbs2$forename, funcs = lgl_2)),
                    display = "none")

# Matching `sex`, and `forename` that ends with "y"
lgl_3 <- function(x, y) substr(x, length(x) - 1, length(x)) == "y"
dbs2$pids_3 <- links(criteria =  list(dbs2$sex), 
                    sub_criteria = list(
                      cr1 = sub_criteria(dbs2$forename, funcs = lgl_3)),
                    display = "none")

# Matching `sex` and at least one `forename` that ends with "y"
lgl_4 <- function(x, y) substr(y, length(y) - 1, length(y)) == "y"
dbs2$pids_4 <- links(criteria =  list(dbs2$sex), 
                    sub_criteria = list(
                      cr1 = sub_criteria(dbs2$forename, funcs = lgl_4)),
                    display = "none")

## ----message=F, warning=F-----------------------------------------------------
Opes_c <- Opes[c("date_of_birth", "name", "hair_colour")]
Opes_c <- Opes["date_of_birth"]
# approximate age
Opes_c$age <- as.numeric(round((Sys.Date() - as.Date(Opes_c$date_of_birth, "%d/%m/%Y"))/365.5))

# Match individuals between ages 35 and 55 
rng1 <- function(x, y) x %in% 35:55
Opes_c$pids_a <- links(criteria = "place_holder",
                       sub_criteria = list(cr1 = sub_criteria(Opes_c$age, funcs = rng1)),
                       display ="none")

# Match individuals with a 5-year age gap between themselves
rng2 <- function(x, y) abs(y - x) %in% 0:5
Opes_c$pids_b <- links(criteria = "place_holder",
                       sub_criteria = list(cr1 = sub_criteria(Opes_c$age, funcs = rng2)),
                       display ="none")

# Match individuals with more than a 5-year age gap between each other 
rng3 <- function(x, y) (y - x) > 5
Opes_c$pids_c <- links(criteria = "place_holder",
                       sub_criteria = list(cr1 = sub_criteria(Opes_c$age, funcs = rng3)),
                       display ="none")

Opes_c[c("age","pids_a", "pids_b", "pids_c")]

## ----warning=F, message=F-----------------------------------------------------
dbs$pids_c <- links(criteria =  list(dbs$sex, dbs$surname), display = "none")

dbs

## ----message=F, warning=F-----------------------------------------------------
dbs_2 <- patient_list; dbs_2

dbs_2$cri_2 <- paste(dbs_2$surname, dbs_2$sex,sep="-")
dbs_2$pid_d <- links(sn = dbs_2$rd_id, list(dbs_2$forename, dbs_2$cri_2), display = "none")

dbs_2

## ----echo=F, fig.height=4, fig.width=8.5, message=F, warning=F----------------
plot_pid(pid = dbs_2$pid_d)

## ----message=F, warning=F-----------------------------------------------------
data(Opes); Opes

# 1 stage linkage
  # stage 1 - name AND (department OR hair_colour OR date_of_birth)
Opes$pids_a <- links(criteria = Opes$name,
                     sub_criteria = list(cr1 = sub_criteria(Opes$department, 
                                                            Opes$hair_colour, 
                                                            Opes$date_of_birth)),
                     display = "none"
                     )

Opes[c("name","department","hair_colour","date_of_birth","pids_a")]
  
# 1 stage linkage 
  # stage 1 - name AND ((department OR hair_colour) AND (date_of_birth)) 
Opes$pids_b <- links(criteria = Opes$name, 
                     sub_criteria = list(cr1 = sub_criteria(Opes$department, 
                                                            Opes$hair_colour),
                                         cr1 = sub_criteria(Opes$date_of_birth)),
                     display = "none")

Opes[c("name","department","hair_colour","date_of_birth","pids_b")]

# 1 stage linkage 
  # stage 1 - name AND ((department OR hair_colour) AND (dd-mm OR dd-yyyy OR mm-yyyy))
Opes$pids_c <- links(criteria = Opes$name, 
                     sub_criteria = list(cr1 = sub_criteria(Opes$department, 
                                                            Opes$hair_colour),
                                         cr1 = sub_criteria(Opes$db_pt1, Opes$db_pt2, Opes$db_pt3)),
                            display = "none")

Opes[c("name","department","hair_colour","date_of_birth","pids_c")]

# 1 stage linkage 
  # stage 1 - name AND ((department)  AND (hair_colour) AND (dd-mm OR dd-yyyy OR mm-yyyy))
Opes$pids_d <- links(criteria = Opes$name, 
                     sub_criteria = list(cr1 = sub_criteria(Opes$department),
                                         cr1 = sub_criteria(Opes$hair_colour),
                                         cr1 = sub_criteria(Opes$db_pt1, Opes$db_pt2, Opes$db_pt3)),
                            display = "none")

Opes[c("name","department","hair_colour","date_of_birth","pids_d")]

## ----message=F, warning=F-----------------------------------------------------
data(Opes); Opes

# 2 stage linkage
  # stage 1 - link "Opes" in departments that start with the letter "P" THEN 
  # stage 2 - bridge these to "Opes" whose hair colour starts with the letter "B"
dept_func <- function(x, y) substr(x, 1, 1) == substr(y, 1, 1) & substr(y, 1, 1) == "P" 
hair_func <- function(x, y) substr(x, 1, 1) == substr(y, 1, 1) & substr(y, 1, 1) == "B"

Opes$p_exp_a <- links(criteria = list(Opes$name, Opes$name),
                     sub_criteria = list(
                       cr1 = sub_criteria(Opes$department, funcs = dept_func),
                       cr2 = sub_criteria(Opes$hair_colour, funcs = hair_func)),
                     display = "none")

# 2 stage linkage
  # stage 1 - link "Opes" in departments that start with the letter "P" THEN 
  # stage 2 - link "Opes" whose hair colour starts with the letter "B"
Opes$p_nexp_a <- links(criteria = list(Opes$name, Opes$name),
                     sub_criteria = list(
                       cr1 = sub_criteria(Opes$department, funcs = dept_func),
                       cr2 = sub_criteria(Opes$hair_colour, funcs = hair_func)),
                     expand = F,
                     display = "none")

Opes[c("name","department","hair_colour", "p_exp_a", "p_nexp_a")]

## ----message=F, warning=F-----------------------------------------------------
# The same as `p_exp_a` 
Opes$p_exp_b <- links(criteria = list(Opes$name, Opes$name),
                     sub_criteria = list(
                       cr2 = sub_criteria(Opes$department, funcs = dept_func),
                       cr1 = sub_criteria(Opes$hair_colour, funcs = hair_func)),
                     display = "none")

# Not the same as `p_nexp_a`
Opes$p_nexp_b <- links(criteria = list(Opes$name, Opes$name),
                     sub_criteria = list(
                       cr2 = sub_criteria(Opes$department, funcs = dept_func),
                       cr1 = sub_criteria(Opes$hair_colour, funcs = hair_func)),
                     expand = F,
                     display = "none")

Opes[c("name","department","hair_colour", "p_exp_a", "p_exp_b", "p_nexp_a", "p_nexp_b")]

## ----warning=F----------------------------------------------------------------
data(patient_list_2); patient_list_2

patient_list_2$pids_a <- links(
  sn = patient_list_2$rd_id,
  criteria = list(patient_list_2$forename, 
                  patient_list_2$surname,
                  patient_list_2$sex), 
  display = "none")

patient_list_2

## ----echo=F, fig.height=4, fig.width=8.5, message=F, warning=F----------------
plot_pid(pid = patient_list_2$pids_a)

## ----warning=F----------------------------------------------------------------
df <- data.frame(
  forename = c("John", "John", "Jon", 
               NA_character_, "Mary", "Mary",
               "Mariam", "John", "Henry",
               "Obinna", "Tomi"),
  surname = c("Swan", "Swanley", "Swanley",
              NA_character_, "Jane", "Jan",
              "Janet", "Swan", "Henderson", 
              "Nelson", "Abdulkareem"), 
  age = c(12, 11, 10, 10, 5, 6, 6, 12, 30, 31, 2500)
)

df$pids_a <- links(criteria = list(df$forename, df$surname, df$age), display = "none")
 
df

## ----echo=F, fig.height=4, fig.width=8.5, message=F, warning=F----------------
plot_pid(df$pids_a)

## ----message=F, warning=F-----------------------------------------------------
data(Opes); Opes

# 1 stage linkage
  # stage 1 - link "Opes" that are in the same department AND have the same day and month of birth 
Opes$cri <- paste(Opes$name, Opes$department, Opes$db_pt1, sep = " ")
Opes$p_shk_c <- links(criteria = list(Opes$cri),
                     display = "none")

Opes[c("name","department","db_pt3", "p_shk_c")]

## ----message=F, warning=F-----------------------------------------------------
# 3 stage linkage
  # stage 1 - link "Opes" that have the same name, THEN WITHIN THESE
  # stage 2 - link "Opes" that are in the same department, THEN WITHIN THESE
  # stage 3 - link "Opes" have the same day and month of birth 
Opes$p_shk_d <- links(criteria = list(Opes$name, Opes$department, Opes$db_pt1),
                      sub_criteria = list(
                        cr1 = sub_criteria("place_holder", funcs = diyar::exact_match),
                        cr2 = sub_criteria("place_holder", funcs = diyar::exact_match),
                        cr3 = sub_criteria("place_holder", funcs = diyar::exact_match)
                      ),
                      shrink = T,
                     display = "none")

Opes[c("name","department","db_pt3", "p_shk_c", "p_shk_d")]

## ----message=F, warning=F-----------------------------------------------------
# 1 stage linkage
  # stage 1 - link "Opes" that have the same name
Opes$p_shk_e1 <- links(criteria = list(Opes$name),
                      sub_criteria = list(
                        cr1 = sub_criteria("place_holder", funcs = diyar::exact_match)),
                      shrink = T,
                     display = "none")

# Another attempt is made always made at the next for records with no links 
Opes$e1 <- ifelse(Opes$p_shk_e1@pid_cri == 0, "No Hits", as.character(Opes$p_shk_e1))

# 1 stage linkage
  # stage 1 - link "Opes" that are in the same department
Opes$p_shk_e2 <- links(criteria = list(Opes$department),
                      strata = Opes$e1,
                       sub_criteria = list(
                        cr1 = sub_criteria("place_holder", funcs = diyar::exact_match)),
                      shrink = T,
                     display = "none")

# Another attempt is made always made at the next for records with no links
Opes$e2 <- ifelse(Opes$p_shk_e1@pid_cri == 0, "No Hits", as.character(Opes$p_shk_e1))

# 1 stage linkage
  # stage 3 - link "Opes" have the same day and month of birth 
Opes$p_shk_e3 <- links(criteria = list(Opes$db_pt1),
                      strata = Opes$e2,
                       sub_criteria = list(
                        cr1 = sub_criteria("place_holder", funcs = diyar::exact_match)),
                      shrink = T,
                     display = "none")

Opes[c("name","department","db_pt3", "p_shk_e1", "p_shk_e2", "p_shk_e3", "p_shk_c", "p_shk_d")]

## ----message=F, warning=F-----------------------------------------------------
data(Opes); Opes

Opes$p_cmp1 <- links(criteria = list("place_holder", "place_holder"),
                      sub_criteria = list(
                        cr1 = sub_criteria(Opes$department, funcs = dept_func),
                        cr2 = sub_criteria(Opes$hair_colour, funcs = hair_func)),
                     expand = T,
                     shrink = F,
                     display = "none")

Opes$p_cmp2 <- links(criteria = list("place_holder", "place_holder"),
                      sub_criteria = list(
                        cr1 = sub_criteria(Opes$department, funcs = dept_func),
                        cr2 = sub_criteria(Opes$hair_colour, funcs = hair_func)),
                     expand = F,
                     shrink = T,
                     display = "none")

# `p_cmp1` is not the same as `p_cmp2`
Opes[c("name", "department", "hair_colour", "p_cmp1", "p_cmp2")]

## ----warning=F, message=F-----------------------------------------------------
patient_list_2$forename <- ifelse(patient_list_2$rd_id %in% 1:3, "Nil", patient_list_2$forename)
# 2 stage linkage
    # Stage 1 - forename
    # Stage 2 - surname

patient_list_2$pids_b <- links(criteria = list(patient_list_2$forename, 
                                               patient_list_2$surname), 
                               display = "none")

patient_list_2[c("forename","surname","pids_b")]

## ----warning=F, message=F-----------------------------------------------------
# `NA` as the proxy for missing value
patient_list_2$forename <- ifelse(patient_list_2$forename == "Nil", NA, patient_list_2$forename)

patient_list_2$pids_d <- links(sn = patient_list_2$rd_id, 
                               criteria = list(patient_list_2$forename, 
                                               patient_list_2$surname), 
                               display = "none")

patient_list_2[c("forename","surname", "pids_b", "pids_d")]

## ---- warning=F, message=F----------------------------------------------------
# Matching `sex`, and `forename` initials
dbs2
dbs2$initials <- substr(dbs2$forename, 1, 1)
dbs2$pids_1b <- links(criteria = dbs2$initials,
                     display = "none")

dbs2[c("forename", "initials", "pids_1", "pids_1b")]

## ---- warning=F, message=F----------------------------------------------------
# 1 stage linkage 
  # stage 1 - name AND ((department)  AND (hair_colour) AND (year_of_birth))
Opes$month_of_birth <- substr(Opes$date_of_birth, 4, 5)
Opes$pids_e <- links(criteria = Opes$name, 
                     sub_criteria = list(cr1 = sub_criteria(Opes$department),
                                         cr1 = sub_criteria(Opes$hair_colour),
                                         cr1 = sub_criteria(Opes$month_of_birth)),
                            display = "none")

Opes$cri <- paste(Opes$name, Opes$month_of_birth, Opes$department, Opes$hair_colour, sep="-")

# 1 stage linkage 
  # stage 1 - name AND department AND hair_colour AND date_of_birth
Opes$pids_f <- links(criteria = Opes$cri,  display = "none")

Opes[c("name","department","hair_colour","month_of_birth","pids_e","pids_f")]

## ---- warning=F, message=F----------------------------------------------------
Opes[c("department")]

Opes$links_a <- links(Opes$department, display = "none")

Opes$links_b <- match(Opes$department, Opes$department[!duplicated(Opes$department)])

Opes[c("department", "links_a", "links_b")]


