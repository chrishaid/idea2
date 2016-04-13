# Load packages ####
require(dplyr)
require(RSQLServer)
#require(readr)
require(lubridate)
require(purrr)
require(stringr)
#require(mapvizieR)


setwd("/jobs/transfers")

# Get Config data ####
config <- as.data.frame(read.dcf("../config/config.dcf"),
                        stringsAsFactors = FALSE)

# Connect to Silo ####
silo_ps_db <- src_sqlserver(server =  config$SILO_URL,
                            database = config$SILO_DBNAME_PS,
                            properties = list(user = config$SILO_USER,
                                              password = config$SILO_PWD))

# Get students ####
students <- tbl(silo_ps_db, "students")

hsr_dates <- ymd("151001") - years(0:2)


membs_list <- hsr_dates %>%
map(~get_membership_on_date(silo_ps_db, .) %>%
      collect())

enrolled <- bind_rows(membs_list)

names(enrolled) <- tolower(names(enrolled))

students <- collect(students)

names(students) <- tolower(names(students))

enrolled_2 <- enrolled %>%
  left_join(students, by = c("studentid"="id")) %>%
  mutate(
    date_start =  ymd_hms(calendardate),
    date_end = date_start + years(1),
    sy = sprintf("%s-%s",
                 year(date_start),
                 year(date_end)),
    transferred = (!is.na(exitcode) &
                     exitcode != "GR" &
                     ymd_hms(exitdate) < date_end),
    exit_date = ymd_hms(ifelse(ymd_hms(exitdate) >= date_end, NA, exitdate)),
    exit_month = month(ymd_hms(exitdate),label = TRUE, abbr = TRUE)
    ) 


transfers_by_day <- enrolled_2 %>%
  group_by(sy, schoolid.x,  exit_date) %>%
  summarize(transfers = sum(transferred)) %>%
  group_by(sy, schoolid.x) %>%
  mutate(cum_transfers = dplyr::order_by(
    exit_date, 
    cumsum(as.integer(transfers))))
  
transfers_by_day_by_code <- enrolled_2 %>%
  filter(transferred) %>%
  group_by(sy, schoolid.x,  exit_date, exitcode) %>%
  summarize(transfers = sum(transferred)) %>%
  group_by(sy, schoolid.x, exitcode) %>%
  mutate(cum_transfers = dplyr::order_by(
    exit_date, 
    cumsum(as.integer(transfers))))

transfers_by_month <- transfers_by_day %>%
  mutate(exit_month = month(exit_date, label = TRUE, abbr = TRUE)) %>%
  group_by(sy, schoolid.x, exit_month) %>%
  summarize(
    date = max(exit_date),
    transfers = sum(transfers),
    cum_transfers = max(cum_transfers))

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

transfer_scale <- c("gray", "lightgray", 
                    scales::brewer_pal("qual", palette = 3)(nrow(transfer_reasons)-2))


transfers_by_month_2 <- scaffold %>%
  left_join(transfers_by_month_by_code, by =c("sy", "schoolid.x", "exit_month", "exitcode")) %>%
  left_join(transfer_reasons, by="exitcode")%>% 
  group_by(sy, schoolid.x,  exitcode) %>% 
  mutate(cum_transfers_2 = as.integer(zoo::na.locf(cum_transfers, na.rm = FALSE)),
         month = factor(exit_month, levels = month_order, ordered=  TRUE))

todays_month <- month(today(), label = TRUE, abbr = TRUE) %>%
  factor(levels = month_order, ordered = TRUE)




ggplot(transfers_by_month_2 %>%
         filter(!(month > todays_month &
                  sy == "2015-2016")
                ), 
       aes(x = month, y=cum_transfers_2)) +
  geom_bar(aes(fill = reason, y=cum_transfers_2), 
            position = "stack",
            stat = "identity") +
  facet_grid(sy ~ schoolid.x) +
  scale_fill_manual(values = rev(transfer_scale)) +
  theme_light()

