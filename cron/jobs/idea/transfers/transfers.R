# Load packages ####
require(dplyr)
#require(RSQLServer)
require(silounloadr)
require(lubridate)
require(purrr)
require(stringr)
require(futile.logger)


setwd("/jobs/idea/transfers")

source('lib/helpers.R')

flog.threshold(TRACE)
flog.appender(appender.tee("logs/transfers.log"))




readRenviron("/config/.Renviron")

flog.info("Load config and set other variables")

config <- as.data.frame(read.dcf("/config/config.dcf"),
                        stringsAsFactors = FALSE)

flog.info("Connect to Silo/BQ")
bigrquery::set_service_token("/config/bq/kipp-chicago-silo-2-aa786970aefd.json")


flog.info("Get students table")

students <- get_powerschool("students") %>%
  drop_fivetran_cols() %>%
  select(id,
         student_number,
         schoolid,
         lastfirst, 
         exitdate,
         exitcode
         ) %>%
  collect()


hsr_dates <- c(ymd("161004"), ymd("151001") - years(0:1))

flog.info("Get enrollments for\n\t%s", paste(hsr_dates, collapse = "\n\t"))

membs_list <- hsr_dates %>%
  map(~get_membership_on_date(.) %>%
      collect())


flog.info("Processing enrollment data.")

enrolled <- bind_rows(membs_list)

enrolled_2 <- enrolled %>%
  left_join(students, by = c("studentid"="id")) %>%
  mutate(
    date_start =  calendardate,
    date_end = date_start + years(1),
    sy = sprintf("%s-%s",
                 year(date_start),
                 year(date_end)),
    transferred = (!is.na(exitcode) &
                     exitcode != "GR" &
                     exitdate < date_end),
    exit_date = ymd(ifelse(exitdate >= date_end, NA, as.character(exitdate))),
    exit_month = month(exitdate,label = TRUE, abbr = TRUE)
    )


flog.info("\t transfers by day")
transfers_by_day <- enrolled_2 %>%
  group_by(sy, schoolid.x,  exit_date) %>%
  summarize(transfers = sum(transferred)) %>%
  group_by(sy, schoolid.x) %>%
  mutate(cum_transfers = dplyr::order_by(
    exit_date,
    cumsum(as.integer(transfers))))

flog.info("\t transfers by day by code")
transfers_by_day_by_code <- enrolled_2 %>%
  filter(transferred) %>%
  group_by(sy, schoolid.x,  exit_date, exitcode) %>%
  summarize(transfers = sum(transferred)) %>%
  group_by(sy, schoolid.x, exitcode) %>%
  mutate(cum_transfers = dplyr::order_by(
    exit_date,
    cumsum(as.integer(transfers))))

flog.info("\t transfers by month")
transfers_by_month <- transfers_by_day %>%
  mutate(exit_month = month(exit_date, label = TRUE, abbr = TRUE)) %>%
  group_by(sy, schoolid.x, exit_month) %>%
  summarize(
    date = max(exit_date),
    transfers = sum(transfers),
    cum_transfers = max(cum_transfers))

flog.info("\t transfers by month by code")
transfers_by_month_by_code <- transfers_by_day_by_code %>%
  mutate(exit_month = month(exit_date, label = TRUE, abbr = TRUE)) %>%
  group_by(sy, schoolid.x, exit_month, exitcode) %>%
  summarize(
    date = max(exit_date),
    transfers = sum(transfers),
    cum_transfers = max(cum_transfers))


transfer_order <- c("Dropped Out", "Expelled", "Behavior/Discipline", "Academics", "Avoid Retention", "Special Needs", "Other", "Don't Know", "Xfer Other KIPP", "Transport", "Moved")

transfer_reasons<-data_frame(exitcode=as.character(c(1:11)),reason=c("Dropped Out", "Moved", "Transport", "Expelled", "Behavior/Discipline", "Academics", "Avoid Retention", "Special Needs", "Other", "Don't Know", "Xfer Other KIPP"))

transfer_reasons <- transfer_reasons %>%
  mutate(reason = factor(reason, levels = transfer_order, ordered = TRUE))

month_order <- c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")

month_factor <- factor(month_order, levels = month_order, ordered = TRUE)


flog.info("Scaffold transfers tables")
scaffold <- expand.grid(sy = unique(transfers_by_day_by_code$sy),
                        schoolid.x = unique(transfers_by_day_by_code$schoolid.x),
                        exit_month = month_factor,
                        exitcode = unique(transfers_by_day_by_code$exitcode))

flog.info("Create transfers table")
transfers_by_month_2 <- scaffold %>%
  left_join(transfers_by_month_by_code, by =c("sy", "schoolid.x", "exit_month", "exitcode")) %>%
  left_join(transfer_reasons, by="exitcode")%>%
  group_by(sy, schoolid.x,  exitcode) %>%
  mutate(cum_transfers_2 = as.integer(zoo::na.locf(cum_transfers, na.rm = FALSE)),
         month = factor(exit_month, levels = month_order, ordered=  TRUE)) %>%
  ungroup() %>%
  mutate(sy = factor(sy, levels = rev(unique(sy)), ordered = TRUE),
         school_name = school_names(schoolid.x)
         ) %>%
  arrange(sy, school_name, month, desc(reason))


flog.info("Calculate transfer goals")
transfer_goals <- enrolled %>%
  group_by(schoolid, calendardate) %>%
  summarize(N = n()) %>%
  mutate(yearly_goal = round(.1 * N),
         monthly_goal = yearly_goal/12,
         sy2 = sprintf("%s-%s",
                      year(calendardate),
                      year(calendardate)+1),
         sy = factor(sy2, levels=rev(unique(sy2)), ordered = TRUE),
         schoolid.x=schoolid,
         school_name = school_names(schoolid)
  )


transfers_by_month_by_school <- transfers_by_month_2 %>%
  group_by(school_name, month, reason) %>%
  summarize(transfers = sum(transfers, na.rm = TRUE)) %>%
  group_by(school_name) %>%
  mutate(pct = transfers/sum(transfers, na.rm=TRUE))



flog.info("Save all transfers tables")
save(transfers_by_month_2,
     transfer_goals,
     transfer_reasons,
     transfers_by_month_by_code,
     transfers_by_month,
     transfers_by_day_by_code,
     transfers_by_day,
     transfers_by_month_by_school,
     file="/data/transfers.Rda")

flog.info("Telling shiny-server to restart")
system("touch /srv/shiny-server/war/restart.txt")


flog.info("Done!")