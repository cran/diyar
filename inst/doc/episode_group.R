## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE, warning=FALSE----------------------------------------
library(dplyr); library(lubridate); library(diyar)

data("infections_2")
dates <- infections_2$date

# Fixed episodes
f <- fixed_episodes(date = dates, case_length=14, group_stats = TRUE, to_s4 =TRUE)

# Rolling episodes
r <- rolling_episodes(date = dates, case_length=14, group_stats = TRUE, display = FALSE, to_s4 =TRUE)

dates # dates

f # fixed episode identifiers

r # rolling episode identifiers

## ----echo=FALSE, fig.height=4, fig.width=7, message=FALSE, warning=FALSE----
library(tidyr); library(ggplot2)

r_epids <- f_epids <- infections_2[c("rid","pid","date")]

f_epids$epids <- f
r_epids$epids <- r

r_epids$dt_a <- start_point(r_epids$epids@epid_interval)
r_epids$dt_z <- end_point(r_epids$epids@epid_interval)
r_epids$case_nm <- r_epids$epids@case_nm
r_epids$epid <- r_epids$epids@.Data

f_epids$dt_a <- start_point(f_epids$epids@epid_interval)
f_epids$dt_z <- end_point(f_epids$epids@epid_interval)
f_epids$case_nm <- f_epids$epids@case_nm
f_epids$epid <- f_epids$epids@.Data

plot <- bind_rows(
  mutate(r_epids, method="rolling episodes"),
  mutate(f_epids, method="fixed episodes")) %>% 
  mutate(epi=14, recur=14) %>% 
  select(method, rid, epid, date, dt_a, dt_z, epi, recur, case_nm) %>% 
  mutate(epid = paste("EP",epid,sep="")) %>% 
  mutate_at(vars(dt_a, dt_z), funs(dmy(format(.,"%d/%m/%Y")))) 

plot <- plot %>% 
  group_by(method, epid) %>% 
  arrange(date) %>% 
  mutate(
    last_record = ifelse(row_number()==n(),1,0),
    recur_dup = ifelse(case_nm=="Duplicate" & !is.na(case_nm[row_number()+1]) & case_nm[row_number()+1] == "Recurrent", 1,0)
    ) %>% 
  ungroup() %>% 
  mutate(
    dash_range = case_when(
      case_nm=="Case" ~ epi, 
      (case_nm =="Recurrent" & last_record !=1) | (recur_dup ==1) ~ recur,
      TRUE ~ 0
    ), 
    dash_num = ifelse(dash_range==0,"",paste(dash_range, "days")),
    dash_range = date + duration(dash_range,"days")
  )

g_area <- unique(select(plot, method, epid, date=dt_a, dt_z)) 
g_area$rid <- max(plot$rid)

ggplot(plot, aes(x=date, y=rid, group=rid, colour=epid)) +
  geom_point() +
  geom_segment(aes(x=date,xend=dash_range,y=rid,yend=rid), linetype=2) +
  geom_text(aes(hjust="right", label=case_nm), nudge_x = -2, nudge_y = .05, show.legend = FALSE) +
  geom_text(aes(hjust="left", label=dash_num), nudge_x = 2, nudge_y = .18, show.legend = FALSE, size =2.5) +
  scale_y_continuous("Record ID", breaks = 1:6) +
  scale_x_date("Specimen date", limits = c(dmy("15/03/2019"), dmy("01/07/2019") ), date_breaks = "25 days", date_labels = "%d %b") +
  facet_wrap(~method, scales = "free_y") +
  guides(col = guide_legend(nrow = 1)) +
  theme(
    strip.background = element_rect(fill="transparent"),
    axis.line = element_line(colour = "black"),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )

## ----warning=FALSE, message=FALSE----------------------------------------
data("infections_3");
dbs <- infections_3[c("pid","date")]; dbs

# Maximum of one fixed episode grouping per strata
dbs$eps_1 <- fixed_episodes(strata = dbs$pid, date = dbs$date, case_length = 3, display = FALSE, episodes_max = 1, to_s4 = TRUE)

# Maximum of one rolling episode grouping per strata
dbs$eps_2 <- rolling_episodes(strata = dbs$pid, date = dbs$date, case_length = 3, display = FALSE, episodes_max = 1, to_s4 = TRUE)

# Maximum of two fixed episode grouping per strata
dbs$eps_3 <- fixed_episodes(strata = dbs$pid, date = dbs$date, case_length = 3, display = FALSE, episodes_max = 2, to_s4 = TRUE)

dbs


## ----warning=FALSE, message=FALSE----------------------------------------
# Infinite recurrence periods per episode (Default)
dbs$eps_4 <- rolling_episodes(strata = dbs$pid, date =dbs$date, case_length = 2,display = FALSE, to_s4 = TRUE)

# Maximum of one recurrence period per episode
dbs$eps_5 <- rolling_episodes(strata = dbs$pid, date =dbs$date, case_length = 2,display = FALSE, 
                              rolls_max = 1, to_s4 = TRUE)

# Maximum of two recurrence periods per episode
dbs$eps_6 <- rolling_episodes(strata = dbs$pid, date =dbs$date, case_length = 2,display = FALSE, 
                              rolls_max = 2, to_s4 = TRUE)

dbs

## ----message=FALSE, warning=FALSE----------------------------------------
dbs <- infections_2[c("date")]; dbs

# Episode grouping in chronological order
dbs$forward <- fixed_episodes(date=dbs$date, case_length= 14,
                              group_stats = TRUE, display = FALSE, to_s4=TRUE)

# Episode grouping in reverse chronological order
dbs$backward <- fixed_episodes(date=dbs$date, case_length= 14, group_stats = TRUE, display = FALSE,
                               from_last=TRUE, to_s4=TRUE)

dbs[c("forward","backward")]

## ----message=FALSE, warning=FALSE----------------------------------------
dates <- c("01/04/2019", "05/04/2019", "07/04/2019")
dates <- as.Date(dates,"%d/%m/%Y")
user_sort <- c(2,1,2)

# preference determined by from_last 
fixed_episodes(date=dates, case_length=6, to_s4=TRUE, display=FALSE, group_stats = TRUE)

# user defined preference via custom sort is prioritised before from_last 
fixed_episodes(date=dates, case_length=6, to_s4=TRUE, custom_sort = user_sort, display=FALSE, group_stats = TRUE)

# user defined preference via custom sort is prioritised before from_last. 
# Duplicates are flagged from both directions
fixed_episodes(date=dates, case_length=6, to_s4=TRUE, custom_sort = user_sort, display=FALSE, 
               bi_direction = TRUE, group_stats = TRUE)


## ----message=FALSE, warning=FALSE----------------------------------------
dbs <- infections_2[c("date","infx")]; dbs
dbs$infx <- gsub("E. coli ","",dbs$infx)
dbs$infx[c(2,5)] <- "UTI"

dbs$epids_1 <- fixed_episodes(date=dbs$date, case_length=14, 
                custom_sort = dbs$infx, display = FALSE, to_s4 = TRUE)

dbs$infx_f <- factor(dbs$infx, levels = c("UTI","BSI"))

dbs$epids_2 <- fixed_episodes(date=dbs$date, case_length=14, 
                custom_sort = dbs$infx_f, display = FALSE, to_s4 = TRUE)

dbs$epids_3 <- fixed_episodes(date=dbs$date, case_length=14, 
                custom_sort = dbs$infx_f, display = FALSE, to_s4 = TRUE, bi_direction = TRUE)

dbs

## ----message=FALSE, warning=FALSE----------------------------------------
dbs <- tibble(date=c("01/04/2019", "05/04/2019"))

dbs$date <- as.Date(dbs$date, "%d/%M/%Y")

# 10-day periods beginning with the `date`
dbs$period <- number_line(dbs$date, dbs$date + 10)

dbs

# Grouping events
fixed_episodes(date=dbs$date, case_length=30, to_s4=TRUE, display=FALSE, group_stat=TRUE)

# Grouping the 10-day periods
fixed_episodes(date=dbs$period, case_length=30, to_s4=TRUE, display=FALSE, group_stat=TRUE)

## ----message=FALSE, warning=FALSE----------------------------------------
hos_admin <- diyar::hospital_admissions; hos_admin

hos_admin$admin_period <- number_line(hos_admin$admin_dt, hos_admin$discharge_dt)

# Grouping the actual admissions into episodes
fixed_episodes(date=hos_admin$admin_dt, sn=hos_admin$rd_id, case_length=0, 
                display=FALSE, to_s4=TRUE, group_stats=TRUE)

# Grouping the periods of stay (admission -> discharge)
fixed_episodes(date=hos_admin$admin_period, sn=hos_admin$rd_id, case_length = 0, 
                display=FALSE, to_s4=TRUE, group_stats=TRUE)


## ----warning=FALSE-------------------------------------------------------
# Overlapping intervals
across <- fixed_episodes(date=hos_admin$admin_period, sn=hos_admin$rd_id, case_length = 0, 
               overlap_method = "across", display = FALSE, to_s4 = TRUE, group_stats = TRUE)

across

# Chained intervals
chain <- fixed_episodes(date=hos_admin$admin_period, sn=hos_admin$rd_id, case_length = 0, 
               overlap_method = "chain", display = FALSE, to_s4 = TRUE, group_stats = TRUE)

chain

# Intervals with aligned end points
aligns_end <- fixed_episodes(date=hos_admin$admin_period, sn=hos_admin$rd_id, case_length = 0, 
               overlap_method = "aligns_end", display = FALSE, to_s4 = TRUE, group_stats = TRUE)

aligns_end

# Intervals with aligned start points
aligns_start <- fixed_episodes(date=hos_admin$admin_period, sn=hos_admin$rd_id, case_length=0, 
               overlap_method = "aligns_start", display = FALSE, to_s4 = TRUE, group_stats = TRUE)

aligns_start

# Intervals occurring completely within others
inbetween <- fixed_episodes(date=hos_admin$admin_period, sn=hos_admin$rd_id, case_length = 0, 
               overlap_method = "inbetween", display = FALSE, to_s4 = TRUE, group_stats = TRUE)

inbetween

# Chained intervals and those occurring completely within others
chain_inbetween <- fixed_episodes(date=hos_admin$admin_period, sn=hos_admin$rd_id, case_length = 0, 
               overlap_method = c("chain","inbetween"), display = FALSE, to_s4 = TRUE, group_stats = TRUE)

chain_inbetween


## ----echo=FALSE, fig.height=5, fig.width=8, message=FALSE, warning=FALSE----
across <- cbind(hos_admin, to_df(across))
chain <- cbind(hos_admin, to_df(chain))
aligns_start <- cbind(hos_admin, to_df(aligns_start))
chain_inbetween <- cbind(hos_admin, to_df(chain_inbetween))
inbetween <- cbind(hos_admin, to_df(inbetween))
aligns_end <- cbind(hos_admin, to_df(aligns_end))

plot <- rbind(
  mutate(across, method="across"),
  mutate(chain, method="chain"),
  mutate(aligns_start, method="aligns_start"),
  mutate(aligns_end, method="aligns_end"),
  mutate(inbetween, method="inbetween"), 
  mutate(chain_inbetween, method="chain and inbetween")
  )

plot <- mutate(plot,
  mrk=paste(epid, method,sep="_"),
  admin_dt = dmy(format(left_point(admin_period), "%d/%m/%Y")),
  discharge_dt = dmy(format(right_point(admin_period), "%d/%m/%Y"))
  ) %>% select(-admin_period)

plot <- dplyr::arrange(plot, .data$mrk)
plot$N <- rep(rle(plot$mrk)$lengths, rle(plot$mrk)$lengths)

plot <- plot %>%
  mutate(epid_2 = paste(method, ": epid ",epid, sep="" )) %>% 
  select(rd_id, epid, epid_2, admin_dt, discharge_dt, method, N, epid, case_nm) %>%
  filter(N>1) %>%
  select(rd_id, admin_dt, discharge_dt, method, epid, epid_2, case_nm) %>%
  gather(var, val, -c(rd_id, method, epid, epid_2, case_nm)) %>%
  mutate(
    rd_id = as.character(rd_id), 
    lab =ifelse(case_nm=="Case" & var=="admin_dt", case_nm, ""), 
    method = factor(method, levels = c("across","aligns_start","aligns_end","chain", "inbetween", "chain and inbetween"))
    )

ggplot(plot, aes(x=val, y=rd_id, group=rd_id, label=lab, colour=epid_2)) +
  geom_line(aes(colour=epid_2)) +
  geom_point() +
  geom_text(aes(hjust="right"), nudge_x = -2, nudge_y = .05, show.legend = FALSE) +
  scale_y_discrete("Record ID") +
  scale_x_date("Admitted period", limits = c(dmy("15/12/2018"), dmy("10/02/2019")), date_breaks = "20 days", date_labels = "%d %b") +
  guides(col = guide_legend(nrow = 2)) +
  facet_wrap(~method, scales = "free_y") +
  theme(
    strip.background = element_rect(fill="transparent"),
    axis.line = element_line(colour = "black"),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )


## ----warning=FALSE, include=FALSE----------------------------------------

# Overlapping intervals
ha2 <- hos_admin
ha2$epi_len <- 30
ha2$shift <- ifelse(ha2$rd_id %in% c(2,8,5,7), 0, 30)
ha2$admin_period <- shift_number_line(ha2$admin_period, duration(ha2$shift, "days"))

# Overlapping intervals
across <- fixed_episodes(date=ha2$admin_period, sn=ha2$rd_id, case_length = 30, 
               overlap_method = "across", display = FALSE, to_s4 = TRUE, group_stats = TRUE)

# Chained intervals
chain <- fixed_episodes(date=ha2$admin_period, sn=ha2$rd_id, case_length = 30, 
               overlap_method = "chain", display = FALSE, to_s4 = TRUE, group_stats = TRUE)

# Intervals with aligned end points
aligns_end <- fixed_episodes(date=ha2$admin_period, sn=ha2$rd_id, case_length = 30, 
               overlap_method = "aligns_end", display = FALSE, to_s4 = TRUE, group_stats = TRUE)

# Intervals with aligned start points
aligns_start <- fixed_episodes(date=ha2$admin_period, sn=ha2$rd_id, case_length = 30, 
               overlap_method = "aligns_start", display = FALSE, to_s4 = TRUE, group_stats = TRUE)

# Intervals occurring completely within others
inbetween <- fixed_episodes(date=ha2$admin_period, sn=ha2$rd_id, case_length = 30, 
               overlap_method = "inbetween", display = FALSE, to_s4 = TRUE, group_stats = TRUE)

# Chained intervals and those occurring completely within others
chain_inbetween <- fixed_episodes(date=ha2$admin_period, sn=ha2$rd_id, case_length = 30, 
               overlap_method = c("chain","inbetween"), display = FALSE, to_s4 = TRUE, group_stats = TRUE)

# Chained intervals and those with aligned end points
chain_aligns_end <- fixed_episodes(date=ha2$admin_period, sn=ha2$rd_id, case_length = 30, 
               overlap_method = c("chain","aligns_end"), display = FALSE, to_s4 = TRUE, group_stats = TRUE)


## ----echo=FALSE, fig.height=5.5, fig.width=8, message=FALSE, warning=FALSE----
across <- cbind(ha2, to_df(across))
chain <- cbind(ha2, to_df(chain))
aligns_start <- cbind(ha2, to_df(aligns_start))
chain_inbetween <- cbind(ha2, to_df(chain_inbetween))
chain_aligns_end <- cbind(ha2, to_df(chain_aligns_end))
inbetween <- cbind(ha2, to_df(inbetween))
aligns_end <- cbind(ha2, to_df(aligns_end))

plot <- rbind(
  mutate(across, method="across"),
  mutate(chain, method="chain"),
  mutate(aligns_start, method="aligns_start"),
  mutate(aligns_end, method="aligns_end"),
  mutate(inbetween, method="inbetween"), 
  mutate(chain_inbetween, method="chain and inbetween"), 
  mutate(chain_aligns_end, method="chain and aligns_end")
  )

plot <- mutate(plot,
  mrk=paste(epid, method,sep="_"),
  admin_dt = dmy(format(left_point(admin_period), "%d/%m/%Y")),
  discharge_dt = dmy(format(right_point(admin_period), "%d/%m/%Y"))
  ) %>% select(-admin_period)

plot <- dplyr::arrange(plot, .data$mrk)
plot$N <- rep(rle(plot$mrk)$lengths, rle(plot$mrk)$lengths)

plot <- plot %>%
  mutate(epid_2 = paste(method, ": epid ",epid, sep="" )) %>% 
  select(rd_id, epid, epid_2, admin_dt, discharge_dt, method, N, epid, case_nm, epi_len) %>%
  filter(N>1) %>%
  select(rd_id, admin_dt, discharge_dt, method, epid, epid_2, case_nm, epi_len) %>%
  gather(var, val, -c(rd_id, method, epid, epid_2, case_nm, epi_len)) %>%
  mutate(
    rd_id = as.character(rd_id), 
    lab =ifelse(case_nm=="Case" & var=="admin_dt", case_nm, ""), 
    method = factor(method, levels = c("across","aligns_start","aligns_end","chain", "inbetween",
                                       "chain and inbetween", "chain and aligns_end","aligns_start and aligns_end"))
    )

ep_plot <- filter(plot, var=="discharge_dt" & case_nm == "Case") %>% 
  mutate(var="start")

#ep_plot$k <- ifelse(ep_plot$case_nm=="Case", ep_plot$epi_len, ep_plot$recur_len)

ep_plot <- mutate(ep_plot, val= val + duration(epi_len,"days"), var="end") %>% 
  bind_rows(ep_plot)

ep_plot$lab <- ifelse(ep_plot$var=="start", paste(ep_plot$epi_len, "days"),"")
                      
ggplot(plot, aes(x=val, y=rd_id, group=rd_id, label=lab, colour=epid_2)) +
  geom_line() +
  geom_point() +
  geom_text(aes(hjust="right"), nudge_x = -2, nudge_y = .05, show.legend = FALSE) +
  geom_line(data=ep_plot, aes(x=val, y=rd_id, group=rd_id, colour=epid_2), linetype=2) +
  #geom_text(data=ep_plot, aes(hjust="left", label=lab), nudge_x = 2, nudge_y = .18, show.legend = FALSE, size =2.5) +
  scale_y_discrete("Record ID") +
  scale_x_date("Admitted period", limits = c(dmy("15/12/2018"), dmy("01/04/2019")), date_breaks = "35 days", date_labels = "%d %b") +
  guides(col = guide_legend(nrow = 4)) +
  facet_wrap(~method, scales = "free_y") +
  theme(
    strip.background = element_rect(fill="transparent"),
    axis.line = element_line(colour = "black"),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )


## ----warning=FALSE-------------------------------------------------------
data(infections)
dbs <- infections[c("date","infection")]
dbs <- dbs[dbs$infection%in% c("UTI","BSI"),]
dbs$epi <- ifelse(dbs$infection=="UTI", 7, 14)
dbs$recur <- ifelse(dbs$infection=="UTI", 30, 0)

dbs$epids <- rolling_episodes(date=dbs$date, case_length =dbs$epi, to_s4 =TRUE,
                            recurrence_length = dbs$recur, display = FALSE, group_stats = TRUE)

dbs

## ----warning=FALSE-------------------------------------------------------
data("infections_4"); 
dbs <- infections_4

dbs$epids <- episode_group(infections_4, sn=rid, strata=c(pid, organism, source), date=date, 
                case_length =epi, episode_type = "rolling", recurrence_length = recur,
                display = FALSE, to_s4 = TRUE)

dbs

## ----warning=FALSE, message=FALSE----------------------------------------
data("hourly_data"); dbs <- hourly_data

dbs[c("datetime", "category")]

rolling_episodes(strata = dbs$category, date = dbs$datetime, case_length = 5,
                 episode_unit = "hours", recurrence_length = 9, group_stats = TRUE, to_s4 = TRUE, display = FALSE)

## ----warning=FALSE, message=FALSE----------------------------------------
dbs <- head(hourly_data[c("datetime","category")], 10)
dbs$subset <- ifelse(dbs$category!="GP3", NA, "group")

dbs$epids <- rolling_episodes(strata= dbs$subset, date = dbs$datetime, case_length = 5, episode_unit = "hours", 
                        recurrence_length = 9, display = TRUE, group_stats = TRUE, to_s4 = TRUE)

dbs

## ----warning=FALSE, message=FALSE----------------------------------------
data(infections) 

dbs <- infections[c("date","infection")]; dbs

# familiar unique record ids for reference
rd_id <- c(640,17,58,21,130,79,45,300,40,13,31)

# strata based on matching sources of infection
dbs$pids <- record_group(dbs, criteria = infection, to_s4 = TRUE, display = FALSE)

# stratified grouping 
dbs$epids <- fixed_episodes(sn = rd_id, date = dbs$date, strata = dbs$pids, 
                             to_s4 = TRUE, display = FALSE, group_stats = TRUE, case_length = 10)

dbs

## ----warning=FALSE, message=FALSE----------------------------------------
vals <- c(8.1,6,12,8.5,12,3,8,15,5,7)

vals

fixed_episodes(vals, case_length = .5, group_stats = T, to_s4 = T, display = F)

fixed_episodes(vals, case_length = 5, group_stats = T, to_s4 = T, display = F)

fixed_episodes(vals, case_length = 100, group_stats = T, to_s4 = T, display = F)

