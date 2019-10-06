## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE, warning=FALSE----------------------------------------
library(dplyr)
library(lubridate)
library(diyar)

data("infections_2"); infections_2

# Fixed episodes
fixed_episodes <- cbind(
  infections_2,
  episode_group(infections_2, rid, pid, date, epi, 
                group_stats = TRUE)
  )

fixed_episodes


## ----warning=FALSE, message=FALSE----------------------------------------
# Rolling episodes
# Progress messages can be turned off with the display argument
rolling_episodes <- cbind(
  infections_2,
  episode_group(infections_2, sn = rid, strata = pid, date =date, 
                case_length = epi, episode_type = "rolling",
                recurrence_length = recur, 
                display = FALSE, group_stats = TRUE)
  )

rolling_episodes

## ----echo=FALSE, fig.height=4, fig.width=7, message=FALSE, warning=FALSE----
library(tidyr)
library(ggplot2)
plot <- bind_rows(
  mutate(rolling_episodes, method="rolling episodes"),
  mutate(fixed_episodes, method="fixed episodes")
  ) %>% 
  mutate(mrk=paste(epid,method,sep = "_"),
         mrk_2=paste(epid,method,case_nm, sep = "_"))

plot <- dplyr::arrange(plot,mrk)
plot$n <- sequence(rle(plot$mrk)$lengths)
plot$N <- rep(rle(plot$mrk)$lengths, rle(plot$mrk)$lengths)

plot <- dplyr::arrange(plot, mrk_2)
plot$n2 <- sequence(rle(plot$mrk_2)$lengths)
plot$N2 <- rep(rle(plot$mrk_2)$lengths, rle(plot$mrk_2)$lengths)

plot <- plot %>%
  mutate(epid_2 = paste(method, ": epid ",epid, sep="" )) %>% 
  select(rid, epid, date, method, epid_2, case_nm, N, n, N2, n2, epi, recur) %>%
  mutate(cd="A", rid=as.character(rid))

lens_plot <- filter(plot, (case_nm=="Duplicate" & n!=N & n2==N2) | case_nm !="Duplicate")

lens_plot <- mutate(lens_plot,
                    end_l= ifelse(case_nm=="Case", epi, recur),
                    end = date + duration(end_l, "days"))

lens_plot <- select(lens_plot, rid, date, end,
                    epid, epid_2, case_nm, method)

lens_plot <- gather(lens_plot, var, val, -c(rid, epid, epid_2, case_nm, method)) 

ggplot(plot, aes(x=date, y=rid, group=rid, label=case_nm, colour=epid_2)) +
  geom_line(aes(colour=epid_2)) +
  geom_point() +
  geom_text(aes(hjust="right"), nudge_x = -2, nudge_y = .05, show.legend = FALSE) +
  scale_y_discrete("Record ID") +
  scale_x_date("Specimen date", limits = c(dmy("15/03/2019"), dmy("01/07/2019") ), date_breaks = "25 days", date_labels = "%d %b") +
  geom_line(data=lens_plot, aes(x=val, y=rid, group=rid, colour=epid_2), linetype=2) +
  facet_wrap(~method, scales = "free_y") +
  guides(col = guide_legend(nrow = 2)) +
  theme(
    strip.background = element_rect(fill="transparent"),
    axis.line = element_line(colour = "black"),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )

## ----warning=FALSE, message=FALSE----------------------------------------
data("infections_3"); infections_3

# Infinite recurrence periods per episode (Default)
cbind(
  infections_3,
  episode_group(infections_3, sn = rid, strata = pid, date =date, case_length = epi, 
                episode_type = "rolling", recurrence_length = recur, display = FALSE)
  )

# Maximum of one recurrence period per episode
cbind(
  infections_3,
  episode_group(infections_3, sn = rid, strata = pid, date =date, case_length = epi, 
                episode_type = "rolling", recurrence_length = recur, rolls_max = 1, 
                display = FALSE)
  )

# Maximum of two recurrence periods per episode
cbind(
  infections_3,
  episode_group(infections_3, sn = rid, strata = pid, date =date, case_length = epi, 
                episode_type = "rolling", recurrence_length = recur, rolls_max = 2, 
                display = FALSE)
  )

## ----warning=FALSE, message=FALSE----------------------------------------
# Maximum of one episode with one recurrence period
cbind(
  infections_3,
  episode_group(infections_3, sn = rid, strata = pid, date =date,  case_length = epi, 
                episode_type = "rolling", recurrence_length = recur, 
                rolls_max = 1, episodes_max = 1, display = FALSE)
  )

# Maximum of two episodes with one recurrence period
cbind(
  infections_3,
  episode_group(infections_3, sn = rid, strata = pid, date =date,  case_length = epi, 
                episode_type = "rolling", recurrence_length = recur, 
                rolls_max = 1, episodes_max = 2, display = FALSE)
  )


## ----message=FALSE, warning=FALSE----------------------------------------
# Episode grouping in chronological order
cbind(
  infections_2,
  episode_group(infections_2, rid, pid, date, epi, display = FALSE)
  )

# Episode grouping in reverse chronological order
cbind(
  infections_2,
  episode_group(infections_2, rid, pid, date, epi, from_last = TRUE, display = FALSE)
  )


## ----message=FALSE, warning=FALSE----------------------------------------
infections_2a <- mutate(infections_2, infx=ifelse(row_number() %in% c(2,5), "E. coli UTI", infx))
cbind(
  infections_2a,
  episode_group(infections_2a, sn=rid, strata=pid, date=date, case_length =epi, 
                custom_sort = infx, display = FALSE)
  )

infections_2a <- mutate(infections_2a,infx = factor(infx, levels = c("E. coli UTI","E. coli BSI")))
cbind(
  infections_2a,
  episode_group(infections_2a, sn=rid, strata=pid, date=date, case_length =epi, custom_sort = infx, display = FALSE)
  )


## ----warning=FALSE-------------------------------------------------------
cbind(
  infections_2a,
  episode_group(infections_2a, sn=rid, strata=pid, date=date, 
                case_length =epi, custom_sort = infx, bi_direction = TRUE, display = FALSE)
  )


## ----message=FALSE, warning=FALSE----------------------------------------
cbind(
  infections_2a,
  episode_group(infections_2a, sn=rid, strata=pid, date=date, case_length =epi, 
                display = FALSE)
  ) %>%
  group_by(epid) %>% 
    mutate(
    case_nm = case_when(
      infx=="E. coli UTI" & n()>1 & case_nm == "Duplicate" ~ "Case",
      infx=="E. coli BSI" & n()>1 & case_nm == "Case" ~ "Duplicate",
      TRUE ~ case_nm
    )
  ) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  select(-starts_with("dup"))


## ----message=FALSE, warning=FALSE----------------------------------------
data("hospital_admissions"); hospital_admissions

hospital_admissions$admin_period <- number_line(hospital_admissions$admin_dt, hospital_admissions$discharge_dt)

# Grouping the actual admissions into episodes
cbind(
  hospital_admissions,
  episode_group(hospital_admissions, date=admin_dt, sn=rd_id, case_length = epi_len, 
                display = FALSE)
  ) %>% select(-admin_period)

# Grouping the periods admissions (admission -> discharge)
cbind(
  hospital_admissions,
  episode_group(hospital_admissions, date=admin_period, sn=rd_id, case_length = epi_len,
                display = FALSE)
  ) %>% select(-admin_dt, -discharge_dt)

## ----warning=FALSE-------------------------------------------------------
# Overlapping intervals
across <- cbind(
  hospital_admissions,
  episode_group(hospital_admissions, date=admin_period, sn=rd_id, case_length = epi_len, 
                overlap_method = "across", display = FALSE)
  ) %>% select(-admin_dt, -discharge_dt)

across

# Chained intervals
chain <- cbind(
  hospital_admissions,
  episode_group(hospital_admissions, date=admin_period, sn=rd_id, case_length = epi_len, 
                overlap_method = "chain", display = FALSE)
  ) %>% select(-admin_dt, -discharge_dt)

chain

# Intervals with aligned end points
aligns_end <- cbind(
  hospital_admissions,
  episode_group(hospital_admissions, date=admin_period, sn=rd_id, case_length = epi_len, 
                overlap_method = "aligns_end", display = FALSE)
  ) %>% select(-admin_dt, -discharge_dt)

aligns_end

# Intervals with aligned start points
aligns_start <- cbind(
  hospital_admissions,
  episode_group(hospital_admissions, date=admin_period, sn=rd_id, case_length = epi_len, 
                overlap_method = "aligns_start", display = FALSE)
  ) %>% select(-admin_dt, -discharge_dt) 

aligns_start

# Intervals occurring completely within others
inbetween <- cbind(
  hospital_admissions,
  episode_group(hospital_admissions, date=admin_period, sn=rd_id, case_length = epi_len, 
                overlap_method = "inbetween", display = FALSE)
  ) %>% select(-admin_dt, -discharge_dt)

inbetween

# Intervals occurring completely within others
chain_inbetween <- cbind(
  hospital_admissions,
  episode_group(hospital_admissions, date=admin_period, sn=rd_id, case_length = epi_len, 
                overlap_method = c("chain","inbetween"), display = FALSE)
  ) %>% select(-admin_dt, -discharge_dt)

chain_inbetween


## ----echo=FALSE, fig.height=5, fig.width=8, message=FALSE, warning=FALSE----
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
hospital_admissions_2 <- hospital_admissions
hospital_admissions_2$epi_len <- 30
hospital_admissions_2$shift <- ifelse(hospital_admissions_2$rd_id %in% c(2,8,5,7), 0, 30)
hospital_admissions_2$admin_period <- shift_number_line(hospital_admissions_2$admin_period, duration(hospital_admissions_2$shift, "days"))

across <- cbind(
  hospital_admissions_2,
  episode_group(hospital_admissions_2, date=admin_period, sn=rd_id, case_length = epi_len, 
                overlap_method = "across", display = FALSE)
  ) %>% select(-admin_dt, -discharge_dt)

# Chained intervals
chain <- cbind(
  hospital_admissions_2,
  episode_group(hospital_admissions_2, date=admin_period, sn=rd_id, case_length = epi_len, 
                overlap_method = "chain", display = FALSE)
  ) %>% select(-admin_dt, -discharge_dt)

# Intervals with aligned end points
aligns_end <- cbind(
  hospital_admissions_2,
  episode_group(hospital_admissions_2, date=admin_period, sn=rd_id, case_length = epi_len, 
                overlap_method = "aligns_end", display = FALSE)
  ) %>% select(-admin_dt, -discharge_dt)

# Intervals with aligned start points
aligns_start <- cbind(
  hospital_admissions_2,
  episode_group(hospital_admissions_2, date=admin_period, sn=rd_id, case_length = epi_len, 
                overlap_method = "aligns_start", display = FALSE)
  ) %>% select(-admin_dt, -discharge_dt)

# Intervals occurring completely within others
inbetween <- cbind(
  hospital_admissions_2,
  episode_group(hospital_admissions_2, date=admin_period, sn=rd_id, case_length = epi_len, 
                overlap_method = "inbetween", display = FALSE)
  ) %>% select(-admin_dt, -discharge_dt)

# Chained intervals and those occurring completely within others
chain_inbetween <- cbind(
  hospital_admissions_2,
  episode_group(hospital_admissions_2, date=admin_period, sn=rd_id, case_length = epi_len, 
                overlap_method = c("chain","inbetween"), display = FALSE)
  ) %>% select(-admin_dt, -discharge_dt)

# Chained intervals and those with aligned end points
chain_aligns_end <- cbind(
  hospital_admissions_2,
  episode_group(hospital_admissions_2, date=admin_period, sn=rd_id, case_length = epi_len, 
                overlap_method = c("chain","aligns_end"), display = FALSE)
  ) %>%
  select(-admin_dt, -discharge_dt)

# Intervals with aligned start perod or aligned end points
aligns_start_end <- cbind(
  hospital_admissions_2,
  episode_group(hospital_admissions_2, date=admin_period, sn=rd_id, case_length = epi_len, 
                overlap_method = c("aligns_start","aligns_end"), display = FALSE)
  ) %>%
  select(-admin_dt, -discharge_dt)

## ----echo=FALSE, fig.height=5.5, fig.width=8, message=FALSE, warning=FALSE----

plot <- rbind(
  mutate(across, method="across"),
  mutate(chain, method="chain"),
  mutate(aligns_start, method="aligns_start"),
  mutate(aligns_end, method="aligns_end"),
  mutate(inbetween, method="inbetween"), 
  mutate(chain_inbetween, method="chain and inbetween"), 
  mutate(chain_aligns_end, method="chain and aligns_end")
  #mutate(aligns_start_end, method="aligns_start and aligns_end")
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

ggplot(plot, aes(x=val, y=rd_id, group=rd_id, label=lab, colour=epid_2)) +
  geom_line() +
  geom_point() +
  geom_text(aes(hjust="right"), nudge_x = -2, nudge_y = .05, show.legend = FALSE) +
  geom_line(data=ep_plot, aes(x=val, y=rd_id, group=rd_id, colour=epid_2), linetype=2) +
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
fixed_intervals_a <- mutate(hospital_admissions, recur_len = 0) %>%
  cbind(.,episode_group(., date=admin_period, sn=rd_id, case_length = epi_len,
                        display = FALSE)
  ) %>% select(-admin_dt, -discharge_dt)

fixed_intervals_a

fixed_intervals_b <- mutate(hospital_admissions, recur_len = 0, epi_len=20) %>%
  cbind(.,episode_group(., date=admin_period, sn=rd_id, case_length = epi_len,
                        display = FALSE)
  ) %>% select(-admin_dt, -discharge_dt)

fixed_intervals_b

rolling_intervals_a <- mutate(hospital_admissions, recur_len = 4) %>%
  cbind(.,episode_group(., date=admin_period, sn=rd_id, case_length = epi_len, 
                        episode_type = "rolling", recurrence_length = recur_len, 
                        display = FALSE)
  ) %>% select(-admin_dt, -discharge_dt)

rolling_intervals_a

rolling_intervals_b <- mutate(hospital_admissions, recur_len = 10) %>%
  cbind(.,episode_group(., date=admin_period, sn=rd_id, case_length = epi_len, 
                        episode_type = "rolling", recurrence_length = recur_len,
                        display = FALSE)
  ) %>% select(-admin_dt, -discharge_dt)

rolling_intervals_b


## ----echo=FALSE, fig.height=5.5, fig.width=8.5, message=FALSE, warning=FALSE----

plot <- rbind(
  mutate(fixed_intervals_a, method="1. fixed episodes; case_length - 0, recurrence_length - 0"),
  mutate(fixed_intervals_b, method="3. fixed episodes; case_length - 30, recurrence_length - 0"),
  mutate(rolling_intervals_a, method="2. rolling episodes; case_length - 0, recurrence_length - 4"),
  mutate(rolling_intervals_b, method="4. rolling episodes; case_length - 0, recurrence_length - 10")
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
  select(rd_id, epid, epid_2, admin_dt, discharge_dt, method, N, epid, case_nm, epi_len, recur_len) %>%
  filter(N>1) %>%
  select(rd_id, admin_dt, discharge_dt, method, epid, epid_2, case_nm, epi_len, recur_len) %>%
  gather(var, val, -c(rd_id, method, epid, epid_2, case_nm, epi_len, recur_len)) %>%
  mutate(
    rd_id = as.character(rd_id), 
    lab =ifelse(case_nm!="Duplicate" & var=="admin_dt", case_nm, "")
    )

ep_plot <- filter(plot, var=="discharge_dt" & 
                    (case_nm %in% c("Case","Recurrent") | (case_nm == "Duplicate" & rd_id ==7))) %>% 
  mutate(var="start")

ep_plot$k <- ifelse(ep_plot$case_nm=="Case", ep_plot$epi_len, ep_plot$recur_len)

ep_plot <- mutate(ep_plot, val= val + duration(k,"days"), var="end") %>% 
  bind_rows(ep_plot)

ggplot(plot, aes(x=val, y=rd_id, group=rd_id, label=lab, colour=epid_2)) +
  geom_line() +
  geom_point() +
  geom_text(aes(hjust="right"), nudge_x = -2, nudge_y = .05, show.legend = FALSE) +
  geom_line(data=ep_plot, aes(x=val, y=rd_id, group=rd_id, colour=epid_2), linetype=2) +
  scale_y_discrete("Record ID") +
  scale_x_date("Admitted period", limits = c(dmy("26/12/2018"), dmy("10/02/2019")), date_breaks = "10 days", date_labels = "%d %b") +
  facet_wrap(~method, scales = "free_y", nrow = 2) +
  guides(colour= guide_legend(ncol = 2)) +
  theme(
    strip.background = element_rect(fill="transparent"),
    axis.line = element_line(colour = "black"),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )


## ----warning=FALSE-------------------------------------------------------
infections_2b <- mutate(infections_2, 
                       epi = ifelse(infx=="E. coli UTI", 7, 14),
                       recur = ifelse(infx=="E. coli UTI", 30, 0))
cbind(
  infections_2b,
  episode_group(infections_2b, sn=rid, strata=c(pid, infx), date=date, 
                case_length =epi, episode_type = "rolling", recurrence_length = recur,
                display = FALSE)
  )


## ----warning=FALSE-------------------------------------------------------
data("infections_4"); infections_4

cbind(
  infections_4,
  episode_group(infections_4, sn=rid, strata=c(pid, organism, source), date=date, 
                case_length =epi, episode_type = "rolling", recurrence_length = recur,
                display = FALSE)
  )


## ----warning=FALSE-------------------------------------------------------
cbind(
  infections_4,
  episode_group(infections_4, sn=rid, strata=c(pid, organism, source, treated), date=date, 
                case_length =epi, episode_type = "rolling", recurrence_length = recur,
                display = FALSE)
  )


## ----warning=FALSE, message=FALSE----------------------------------------
data("hourly_data"); hourly_data

cbind(
  select(hourly_data, datetime),
  episode_group(hourly_data, rid, category, datetime, epi, episode_type = "rolling", episode_unit = "hours", recurrence_length = recur, group_stats = TRUE)
  )

## ----warning=FALSE, message=FALSE----------------------------------------
head(hourly_data, 10) %>% 
  mutate(subset = ifelse(category!="GP3", NA, "group")) %>% 
  cbind(.,episode_group(., rid, subset, datetime, epi, episode_type = "rolling", episode_unit = "hours", 
                        recurrence_length = recur, display = TRUE, group_stats = TRUE))

## ----warning=FALSE, message=FALSE----------------------------------------
data(infections); infections
db_a <- infections

# Fixed episodes
f_epi <- fixed_episodes(x = db_a$date, case_length = db_a$epi_len, display = FALSE)
f_epi; str(f_epi)

# Rolling episodes
r_epi <- rolling_episodes(x = db_a$date, case_length = db_a$epi_len, recurrence_length = 40, display = FALSE)
f_epi; str(f_epi)

# Working with a data.frame
db_b <- mutate(db_a, epid_interval= fixed_episodes(x = date, case_length = epi_len, strata = infection, display = FALSE))

# Extract useful episode information from the number_line objects
db_b$epid <- db_b$epid_interval@gid
db_b$epid_length <- number_line_width(db_b$epid_interval)
select(db_b, rd_id, date, epid_interval, epid, epid_length)

