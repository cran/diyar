## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include=FALSE-------------------------------------------------------
plot_epid <- diyar:::plot_epid
date <- function(x) as.Date(x, "%d/%m/%Y")
dttm <- function(x) as.POSIXct(x, "UTC",format="%d/%m/%Y %H:%M:%S")

## ----message=FALSE, warning=FALSE, include=FALSE-------------------------
library(diyar)
ds <- c("01/04/2019", "06/04/2019", "10/04/2019", "11/04/2019")
ds <- as.Date(ds, "%d/%m/%Y")
ep <- rolling_episodes(ds, case_length = 6, group_stats = T, data_source = c("DS1","DS1","DS3", "DS1"))

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
df <- data.frame(events =ds, data_source = c("DS1","DS1","DS3", "DS1"), epid= ep)
df

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
to_df(df$ep)

## ----message=FALSE, warning=FALSE----------------------------------------
library(diyar)

# Events
ds <- c("01/04/2019", "03/04/2019", "13/04/2019","16/04/2019", "18/04/2019")
ds <- data.frame(date= as.Date(ds, "%d/%m/%Y"))
ds$date

## ----warning=FALSE-------------------------------------------------------
# 6-day (5-day difference) episodes - fixed episodes
ds$f1 <- fixed_episodes(ds$date, case_length = 5, display = F)

ds$f1

## ----echo=FALSE, fig.height=4, fig.width=8.5, message=FALSE, warning=FALSE----
plot_epid(epid = ds$f1, date= ds$date, case_length = 5)

## ----warning=FALSE-------------------------------------------------------
rng <- number_line(12, 16); rng
ds$f2 <- fixed_episodes(ds$date, case_length = rng, display = F)

ds$f2

## ----echo=FALSE, fig.height=4, fig.width=8.5, message=FALSE, warning=FALSE----
plot_epid(epid = ds$f2, date= ds$date, case_length = rng)

## ----warning=FALSE-------------------------------------------------------
ds$f3 <- fixed_episodes(ds$date, case_length = rng, display = F, skip_if_b4_lengths = F)

ds$f3

## ----echo=FALSE, fig.height=4, fig.width=8.5, message=FALSE, warning=FALSE----
plot_epid(epid = ds$f3, date= ds$date, case_length = rng)

## ----warning=FALSE-------------------------------------------------------
ds$f4 <- fixed_episodes(ds$date, case_length = 5, from_last = T, display = F)

ds$f4

## ----echo=FALSE, fig.height=4, fig.width=8.5, message=FALSE, warning=FALSE----
plot_epid(epid = ds$f4, date= ds$date, case_length = 5, from_last = T)

## ----warning=FALSE-------------------------------------------------------
ds2 <- data.frame(date=  as.Date(c("13/03/2020", "01/04/2020","15/08/2020",
                                   "11/12/2020", "31/12/2020"), "%d/%m/%Y"),
                  diag = c("HBV","HIV","MyCt","HBV", "MyCt"), stringsAsFactors = F)


# First make the HIV diagnosis the index event 
ds2$user_ord <- ifelse(ds2$diag=="HIV", 1,2)

# Then track concurrent infections up to 6 months AFTER the HIV diagnosis
ds2$ep1 <- fixed_episodes(date = ds2$date, custom_sort = ds2$user_ord, data_source = ds2$diag,
                          case_length = 6, episode_unit = "months", display = F)

# Using `data_source` populates the `epid_dataset` slot of the `epid` object. 
# This is useful when you're working with different datasets
ds2$ep1_ds <- ds2$ep1@epid_dataset

# Track preceeding infections up to 6 months BEFORE the HIV diagnosis
ds2$ep2 <- fixed_episodes(date = ds2$date, custom_sort = ds2$user_ord, data_source = ds2$diag,
                          case_length = -6, episode_unit = "months", display = F)

ds2$ep2_ds <- ds2$ep2@epid_dataset

ds2

## ----warning=FALSE-------------------------------------------------------
# Track preceeding and concurrent infections up to 6 months BEFORE OR AFTER the HIV diagnosis

# Method 1a
# Track diagnoses 6 months AFTER the HIV diagnosis.
  # `bi_direction` then tells the function to also check 6 months BEFORE the HIV diagnosis
ds2$ep3a <- fixed_episodes(date = ds2$date, custom_sort = ds2$user_ord, case_length = 6, 
                           bi_direction = T, episode_unit = "months", display = F)

# Method 1b
# Track diagnoses 6 months BEFORE the HIV diagnosis
  # bi_direction` then tells the function to also check 6 months AFTER the HIV diagnosis
ds2$ep3b <- fixed_episodes(date = ds2$date, custom_sort = ds2$user_ord, case_length = -6,
                           bi_direction = T, episode_unit = "months", display = F, 
                           data_source = ds2$diag)

ds2$ep3_ds <- ds2$ep3b@epid_dataset

# Method 2a
# Track diagnoses 6 months BEFORE or AFTER the HIV diag with an applicable. 
  # There's no need for `bi_direction`.
rng <- number_line(-6, 6)
ds2$ep4 <- fixed_episodes(date = ds2$date, custom_sort = ds2$user_ord, case_length = rng, episode_unit = "months",
                          display = F, data_source = ds2$diag)

ds2$ep4_ds <- ds2$ep4@epid_dataset

# Method 2b
# Track diagnoses between 2 and 6 months AFTER the HIV diagnosis. 
  #` bi_direction` then tells the function to also check between 2 and 6 months BEFORE the HIV diagnosis
# Diagnoses before 2 months on either side are skipped as mentioned in the "cut-offs" section
rng2 <- number_line(2, 6)
ds2$ep5 <- fixed_episodes(date = ds2$date, custom_sort = ds2$user_ord, case_length = rng2, 
                          bi_direction =T, episode_unit = "months", display = F)

ds2

## ----warning=FALSE, message=FALSE----------------------------------------
dbf <- infections[c("date", "infection")]

# 3 levels: "UTI" > "BSI" > "RTI"
dbf$c_sort1 <- ifelse(dbf$infection == "UTI", 1, ifelse(dbf$infection == "BSI", 2, 3))
# 2 levels: "UTI" > ("BSI" OR "RTI")
dbf$c_sort2 <- ifelse(dbf$infection == "UTI", 1, 2)
# 2 levels: "BSI" > ("UTI" OR "RTI")
dbf$c_sort3 <- ifelse(dbf$infection == "BSI", 1, 2)

dbf$ep_1 <- fixed_episodes(dbf$date, case_length = 15, data_source = dbf$infection, 
                           custom_sort = dbf$c_sort1, display = F)

dbf$ep_2 <- fixed_episodes(dbf$date, case_length = 15, data_source = dbf$infection, 
                           custom_sort = dbf$c_sort2, display = F)

dbf$ep_3 <- fixed_episodes(dbf$date, case_length = 15, data_source = dbf$infection, 
                           custom_sort = dbf$c_sort3, display = F)

dbf

## ----warning=FALSE-------------------------------------------------------
# 6-day (5-day difference) episodes with 11-days recurrence periods - rolling episodes
ds$r1 <- rolling_episodes(ds$date, case_length = 5, recurrence_length = 10, display = F)

ds$r1

## ----echo=FALSE, fig.height=4, fig.width=8.5, message=FALSE, warning=FALSE----
plot_epid(epid = ds$r1, date= ds$date, case_length = 5, recurrence_length = 10)

## ----warning=FALSE-------------------------------------------------------
rng <- number_line(13, 15); rng
ds$r2 <- rolling_episodes(ds$date, case_length = 5, recurrence_length = rng, display = F)

ds$r2

## ----echo=FALSE, fig.height=4, fig.width=8.5, message=FALSE, warning=FALSE----
plot_epid(epid = ds$r2, date= ds$date, case_length = 5, recurrence_length = rng)

## ----warning=FALSE-------------------------------------------------------
rng <- number_line(13, 15); rng
ds$r3 <- rolling_episodes(ds$date, case_length = 5, recurrence_length = rng, 
                          skip_if_b4_lengths = F, display = F)

ds$r3

## ----echo=FALSE, fig.height=4, fig.width=8.5, message=FALSE, warning=FALSE----
plot_epid(epid = ds$r3, date= ds$date, case_length = 5, recurrence_length = rng)

## ----message=FALSE, warning=FALSE----------------------------------------
df <- c("01/04/2019", "11/04/2019", "16/04/2019","21/04/2019", "07/05/2019")
df <- data.frame(date= as.Date(df, "%d/%m/%Y"))

# 15-day fixed episodes
df$f1 <- fixed_episodes(df$date, case_length = 14, display = F)

# 15-day rolling episodes with 15-day recurrence periods
  # Since the first event is the reference event for the next recurrence window and,
  # the recurrence_length is the same the case_length (14-day difference), 
  # the result will be the same as the 15-day fixed-episode (above) 
df$r1 <- rolling_episodes(df$date, case_length = 14, recurrence_from_last = F, 
                          to_s4 = T, display = F)

df

# Identical
all(df$f1==df$r1)

## ----message=FALSE, warning=FALSE----------------------------------------
df$r2 <- rolling_episodes(df$date, case_length = 14, recurrence_length = 16, display = F)

df$r2

## ----echo=FALSE, fig.height=5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_epid(epid = df$r2, date = df$date, case_length =14, recurrence_length = 16)

## ----message=FALSE, warning=FALSE----------------------------------------
df$r3 <- rolling_episodes(df$date, case_length = 14, recurrence_length = 16, 
                          recurrence_from_last = F, display = F)

df$r3

## ----echo=FALSE, fig.height=5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_epid(epid = df$r3, date = df$date, case_length =14, recurrence_length = 16)

## ----message=FALSE, warning=FALSE----------------------------------------
df <- c("01/04/2019", "11/04/2019", "16/04/2019","21/04/2019","25/04/2019", "07/05/2019")
df <- data.frame(date= as.Date(df, "%d/%m/%Y"))

## ----message=FALSE, warning=FALSE----------------------------------------
df$r4 <- rolling_episodes(df$date, case_length = 10, display = F)

df$r4

## ----echo=FALSE, fig.height=5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_epid(epid = df$r4, date = df$date, case_length =10, recurrence_length = 10)

## ----message=FALSE, warning=FALSE----------------------------------------
df$r5 <- rolling_episodes(df$date, case_length = 10, case_for_recurrence = T, 
                          to_s4 = T, display = F)

df$r5

## ----echo=FALSE, fig.height=5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_epid(epid = df$r5, date = df$date, case_length =10, recurrence_length = 10)

## ----message=FALSE, warning=FALSE----------------------------------------
date <-  seq.Date(as.Date("01/04/2019", "%d/%m/%Y"), 
                  as.Date("21/04/2019","%d/%m/%Y"), 
                  by="3 day")
# Example 2
df2 <- data.frame(date = date, sn = 1:length(date))
# dates
df2$date

## ----message=FALSE, warning=FALSE----------------------------------------
df2$r1 <- rolling_episodes(df2$date, case_length = 6, recurrence_length = 4, display = F, sn=df2$sn)

df2$r1

## ----echo=FALSE, fig.height=5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_epid(epid = df2$r1, date = df2$date, case_length =6, recurrence_length = 4)

## ----message=FALSE, warning=FALSE----------------------------------------
df2$r2 <- rolling_episodes(df2$date, case_length = 6, recurrence_length = 4, 
                           case_for_recurrence = T, display = F, sn=df2$sn)

df2$r1

## ----echo=FALSE, fig.height=5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_epid(epid = df2$r2, date = df2$date, case_length =6, recurrence_length = 4)

## ----warning=FALSE, message=FALSE----------------------------------------
data("infections_3");
dbs <- infections_3[c("pid","date")]; 
# dates
dbs$date

## ----message=FALSE, warning=FALSE----------------------------------------
dbs$eps_1 <- fixed_episodes(strata = dbs$pid, date = dbs$date, case_length = 3, display = F, 
                            episodes_max = 1)

dbs$eps_1

## ----echo=FALSE, fig.height=5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_epid(epid = dbs$eps_1, date = dbs$date, case_length =3)

## ----message=FALSE, warning=FALSE----------------------------------------
dbs$eps_2 <- fixed_episodes(strata = dbs$pid, date = dbs$date, case_length = 3, display = FALSE, episodes_max = 2)

dbs$eps_2

## ----echo=FALSE, fig.height=5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_epid(epid = dbs$eps_2, date = dbs$date, case_length =3)

## ----message=FALSE, warning=FALSE----------------------------------------
dbs$eps_4 <- rolling_episodes(strata = dbs$pid, date =dbs$date, case_length = 2,display = F)

dbs$eps_4

## ----echo=FALSE, fig.height=5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_epid(epid = dbs$eps_4, date = dbs$date, case_length =2, recurrence_length = 2)

## ----message=FALSE, warning=FALSE----------------------------------------
dbs$eps_6 <- rolling_episodes(strata = dbs$pid, date =dbs$date, case_length = 2,display = F, 
                              rolls_max = 1)

dbs$eps_6

## ----echo=FALSE, fig.height=5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_epid(epid = dbs$eps_6, date = dbs$date, case_length =2, recurrence_length = 2)

## ----message=FALSE, warning=FALSE----------------------------------------
dbs$eps_7 <- rolling_episodes(strata = dbs$pid, date =dbs$date, case_length = 2,display = F, 
                              rolls_max = 1, case_for_recurrence = T)

dbs$eps_7

## ----echo=FALSE, fig.height=5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_epid(epid = dbs$eps_7, date = dbs$date, case_length =2, recurrence_length = 2)

## ----message=FALSE, warning=FALSE----------------------------------------
# Dates
dates <- seq(as.Date("01/04/2019", "%d/%M/%Y"), as.Date("20/04/2019", "%d/%M/%Y"), "4 days")
case_len <- 4

# Periods 
periods <- number_line(dates, dates + 4)

dates
periods

# Track fixed episodes from events with a 5 day cut-off 
mth1 <- fixed_episodes(date = dates, case_length = 4, display = F)

# Track fixed episode from periods that are 5 days long 
mth2 <- fixed_episodes(date = periods, case_length =0, display = F)

# Same results
mth1; mth2

# Track rolling episodes from events using a 5 day cut-off 
mth3 <- rolling_episodes(date = dates, case_length = 4, display = F)

# Track rolling episode from periods that are 5 days long 
mth4 <- rolling_episodes(date = periods, case_length =0, display = F)

# Same results
mth3; mth4

## ----message=FALSE, warning=FALSE----------------------------------------
hos_admin <- diyar::hospital_admissions
hos_admin$admin_period <- number_line(hos_admin$admin_dt, hos_admin$discharge_dt)

# Hospital stay
hos_admin$admin_period

## ----message=FALSE, warning=FALSE----------------------------------------
hs_epids_a <- fixed_episodes(date=hos_admin$admin_period, case_length = 0, 
                display=F, group_stats=T)

## ----echo=FALSE, fig.height=5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_epid(epid = hs_epids_a, date = hos_admin$admin_period, case_length = 0)

## ----message=FALSE, warning=FALSE----------------------------------------
hs_epids_b <- fixed_episodes(date=hos_admin$admin_period, case_length = 20, 
                display=F, group_stats=T)

## ----echo=FALSE, fig.height=5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_epid(epid = hs_epids_b, date = hos_admin$admin_period, case_length = 20)

## ----message=FALSE, warning=FALSE----------------------------------------
hs_epids_c <- rolling_episodes(date=hos_admin$admin_period, case_length = 0, 
                recurrence_length=15, display=F, group_stats=T)

## ----echo=FALSE, fig.height=5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_epid(epid = hs_epids_c, date = hos_admin$admin_period, case_length = 0, recurrence_length=15)

## ----warning=FALSE-------------------------------------------------------
# wrapper function to use with lapply
epi_grp_func <- function(x){
  epids <- fixed_episodes(date=hos_admin$admin_period, 
                 sn=hos_admin$rd_id, 
                 case_length = 0, 
                 overlap_methods = x, 
                 display = FALSE, 
                 to_s4 = TRUE, 
                 group_stats = TRUE)
  
  # for the prurpose of this demonstration, limit to overlapped periods    
  epids[epids@epid_total>1]
}

# Methods 
methods <- list(
  # Identical intervals
  exact = "exact",
  # Overlapping intervals
  across= "across",
  # Intervals with aligned start points
  aligns_start = "aligns_start",
  # Intervals with aligned end points
  aligns_end = "aligns_end",
  # Chained intervals
  chain = "chain",
  # Intervals occurring completely within others
  inbetween = "inbetween",
  # Chained intervals and those occurring completely within others
  cb1 = "chain|inbetween",
  # Chained intervals, identical intervals and those occurring completely within others
  cb2 = "exact|chain|inbetween",
  # Chained intervals, overlapping intervals and those with aligned end point
  cb3 = "across|chain|aligns_end"
)

epids <- lapply(methods, epi_grp_func)
names(epids) <- names(methods)

# Results
epids["chain"]

epids["cb2"]

## ----echo=FALSE, fig.height=5, fig.width=8.5, message=FALSE, warning=FALSE----
eps <- epids[["exact"]]
plot_epid(epid = eps, date = hos_admin$admin_period[eps@sn] , case_length = 0)

## ----echo=FALSE, fig.height=5, fig.width=8.5, message=FALSE, warning=FALSE----
eps <- epids[["chain"]]
plot_epid(epid = eps, date = hos_admin$admin_period[eps@sn] , case_length = 0)

## ----echo=FALSE, fig.height=5, fig.width=8.5, message=FALSE, warning=FALSE----
eps <- epids[["cb1"]]
plot_epid(epid = eps, date = hos_admin$admin_period[eps@sn] , case_length = 0)

## ----warning=FALSE-------------------------------------------------------
pa <- c("28/03/2019", "01/04/2019", "03/04/2019", "07/04/2019","12/04/2019")
pz <- c("31/03/2019", "10/04/2019", "05/04/2019", "09/04/2019","13/04/2019")

pa <- as.Date(pa, "%d/%m/%Y")
pz <- as.Date(pz, "%d/%m/%Y")

periods <- number_line(pa, pz)
periods


## ----message=FALSE, warning=FALSE----------------------------------------
eps_a <- fixed_episodes(periods, case_length =6, group_stats = T, display = F)

eps_a

## ----echo=FALSE, fig.height=5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_epid(epid = eps_a, date = periods, case_length = 6)

## ----message=FALSE, warning=FALSE----------------------------------------
eps_b <- fixed_episodes(periods, case_length =-2, group_stats = T, display = F)

eps_b

## ----echo=FALSE, fig.height=5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_epid(epid = eps_b, date = periods, case_length = -2)

## ----message=FALSE, warning=FALSE----------------------------------------
eps_c <- fixed_episodes(periods, case_length =-2, group_stats = T, display = F,
                        include_index_period = F)

eps_c

## ----echo=FALSE, fig.height=5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_epid(epid = eps_c, date = periods, case_length = -2)

## ----message=FALSE, warning=FALSE----------------------------------------
eps_d <- fixed_episodes(periods, case_length =-6, group_stats = T, display = F)

eps_d

## ----echo=FALSE, fig.height=5, fig.width=8.5, message=FALSE, warning=FALSE----
plot_epid(epid = eps_d, date = periods, case_length = -6)

## ----warning=FALSE-------------------------------------------------------
dbs <- diyar::infections[c("date","infection")]
dbs <- dbs[dbs$infection%in% c("UTI","BSI"),]
dbs$epi <- ifelse(dbs$infection=="UTI", 7, 14)
dbs$recur <- ifelse(dbs$infection=="UTI", 30, 0)

dbs$epids <- rolling_episodes(date=dbs$date, case_length =dbs$epi, to_s4 =TRUE, 
                              strata = dbs$infection, recurrence_length = dbs$recur, 
                              display = FALSE, group_stats = TRUE)

dbs

## ----warning=FALSE-------------------------------------------------------
dbs <- diyar::infections_4

dbs$epids <- episode_group(dbs, sn=rid, strata=c(pid, organism, source), date=date, 
                case_length =epi, episode_type = "rolling", recurrence_length = recur,
                display = FALSE)

dbs

## ----warning=FALSE, message=FALSE----------------------------------------
dbs <- head(hourly_data[c("datetime","category")], 10)
dbs$subset <- ifelse(dbs$category!="GP3", NA, "group")

# To skip OR
dbs$epids_a <- rolling_episodes(strata= dbs$subset, date = dbs$datetime, case_length = 5, 
                                episode_unit = "hours", recurrence_length = 9, display = TRUE)

#   OR

# Not to skip
dbs$epids_b <- rolling_episodes(strata= dbs$category, date = dbs$datetime, case_length = 5, 
                                episode_unit = "hours", recurrence_length = 9, display = TRUE)

dbs

## ----warning=FALSE, message=FALSE----------------------------------------
dbf <- dbf[c("date", "infection")]
dbf$c_sort <- ifelse(dbf$infection == "UTI", 1, ifelse(dbf$infection == "BSI", 2, 3))

# Skip episodes that would begin with BSI/UTI records
dbf$ep_a <- fixed_episodes(dbf$date, case_length = 15, data_source = dbf$infection, 
                           custom_sort = dbf$c_sort, skip_order = 1)
dbf$ep_a_d <- dbf$ep_a@epid_dataset

# Don't skip any episodes
dbf$ep_b <- fixed_episodes(dbf$date, case_length = 15, data_source = dbf$infection, 
                           custom_sort = dbf$c_sort)
dbf$ep_b_d <- dbf$ep_b@epid_dataset

dbf

## ----warning=FALSE, message=FALSE----------------------------------------
dttm <- function(x) as.POSIXct(x, "UTC",format="%d/%m/%Y %H:%M:%S")
dbg <- data.frame(date = seq.POSIXt(dttm("01/04/2018 00:00:00"), dttm("31/05/2018 00:00:00"),
                                    by="3 days"))
dbg <- head(dbg, 11)
dbg$recurrence <- 2
dbg$case_len <- 6
dbg$dataset <- paste("DS",c(1:3, rep(c(1:2),2), rep(3,4)), sep="")

# Don't skip any
dbg$ep_a <- episode_group(dbg, date = date, case_length = case_len, episode_type ="rolling", 
                          recurrence_length = recurrence, data_source = dataset, to_s4 = T,
                          display = F)

# Breakup episodes without events from the `DS3` `data_source` 
  # The `DS3` event MUST BE linked to events from other `data_sources`  
dbg$ep_b <- episode_group(dbg, date = date, case_length = case_len, episode_type ="rolling", 
                          recurrence_length = recurrence, data_source = dataset, to_s4 = T,
                          data_links = "DS3", display = F)

# Breakup episodes without events from the `DS3` `data_source` 
  # The `DS3` event DOESN'T HAVE to be linked to events from other `data_sources`   
dbg$ep_c <- episode_group(dbg, date = date, case_length = case_len, episode_type ="rolling", 
                          recurrence_length = recurrence, data_source = dataset, to_s4 = T,
                          data_links = list(g="DS3"), display = F)

# Breakup episodes without events from the `DS3` `data_source` 
  # The `DS3` event MUST BE linkned to events from the `DS1` `data_source`
dbg$ep_d <- episode_group(dbg, date = date, case_length = case_len, episode_type ="rolling", 
                          recurrence_length = recurrence, data_source = dataset, to_s4 = T,
                          data_links = list(l=c("DS3","DS1")), display = F)

dbg[c("date", "dataset", "ep_a", "ep_b", "ep_c", "ep_d")]

## ----warning=FALSE, message=FALSE----------------------------------------
dbs <- diyar::hourly_data

# Each unit is relative to a set number of seconds. 
diyar::episode_unit

# 1-day fixed episodes
fixed_episodes(date = dbs$datetime, case_length = 1, episode_unit = "days", 
               group_stats = TRUE, display = FALSE)

# 5-hr fixed episodes
fixed_episodes(date = dbs$datetime, case_length = 5, episode_unit = "hours", 
               group_stats = TRUE, display = FALSE)

# 5-hr rolling episodes
rolling_episodes(date = dbs$datetime, case_length = 5, episode_unit = "hours", 
                 group_stats = TRUE, display = FALSE)

## ----warning=FALSE, message=FALSE----------------------------------------
dbs <- diyar::infections[c("date","infection")]; dbs

# familiar unique record ids for reference - optional
dbs$rd_id <- c(640,17,58,21,130,79,45,300,40,13,31)

# strata based on matching sources of infection
dbs$pids <- record_group(dbs, sn = rd_id,  criteria = infection, display = FALSE)

# stratified grouping 
dbs$epids <- fixed_episodes(sn = dbs$rd_id, date = dbs$date, strata = dbs$pids, 
                             to_s4 = TRUE, display = FALSE, case_length = 10)

dbs

## ----warning=FALSE, message=FALSE----------------------------------------
vals <- c(8.1,6,12,8.5,12,3,8,15,5,7)

vals

fixed_episodes(vals, case_length = .5, group_stats = T, display = F)

fixed_episodes(vals, case_length = 5, group_stats = T, display = F)

fixed_episodes(vals, case_length = 100, group_stats = T, display = F)

