readRenviron("/config/.Renviron")

library(silounloadr)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(janitor)
library(prophet)

setwd("/jobs/idea/ktc/")
source("lib/library.R")


# Get KTC data's from silo2 ####

#googlesheets::gs_auth(token="/config/gs_token.rds")
#system("chmod 777 .httr-oauth")


contact <- get_alumni_db("contact", collect = TRUE)
account <- get_alumni_db("account", collect = TRUE)
enrollment_c <- get_alumni_db("enrollment_c", collect = TRUE)
college_persistence <- get_alumni_db("college_persistence_c", collect = TRUE)
user <- get_alumni_db("user", collect = TRUE)
applications <- get_alumni_db("application_c", collect = TRUE)
contact_note_c <- get_alumni_db("contact_note_c", collect = TRUE)



# Prep enrollments_data data ###
class_all <- prep_class()


# Prep contancts ####

contacts_details <-
  contact_note_c %>%
  inner_join(contact %>%
               select(id,
                      last_name,
                      first_name,
                      kipp_hs_class_c),
             by = c("contact_c" = "id") ) %>%
  inner_join(user, by = c("created_by_id" = "id"))

contacts_summary <- contacts_details %>%
  group_by(kipp_hs_class_c, contact_c, status_c, name.y) %>%
  summarize(N = n())

  # Prep hs applications by school ####
  hs_applications <- contact %>%
    left_join(applications, by = c("id"="applicant_c")) %>%
    inner_join(user, by=c("owner_id" = "id"))%>%
    select(id,
       last_name = last_name.x,
       first_name = first_name.x,
       status = application_submission_status_c,
       counselor = name,
       school = currently_enrolled_school_c,
       class = kipp_hs_class_c) %>%
mutate(school = ifelse( grepl("Ascend", school),
                        "KIPP Ascend Middle School", school))

##student-level hs application status####
##2021 counselors
counselors_2021 <- c("Catrina Thomas", "China Hill", "Tiffany Harrell")

#prep data for student-level bar plot
  students_data_bar <- hs_applications %>%
  filter(grepl("KIPP", school),
         class %in% 2021,
         counselor %in% counselors_2021) %>%
    group_by(id, last_name, first_name, status, counselor, school, class) %>%
    summarize(n_applications = n()) %>%
    mutate(name = sprintf("%s %s", first_name, last_name)) %>%
    ungroup %>%
    spread(key=status, value = n_applications) %>% ##status as a column
    clean_names %>%
    mutate(submitted = ifelse(is.na(submitted), 0, submitted),
    no_status = ifelse(x_na == 1 & submitted == 0, 0, x_na),
    color_name = ifelse(submitted == 0, "red", "black")) %>% #setting name color
    gather(key=status, value = n_applications,
    -c(id, last_name, first_name, counselor, name, color_name, x_na, school, class)) %>% #gather status
    mutate(n_applications = ifelse(is.na(n_applications), 0, n_applications)) %>%
    group_by(name, status, color_name, n_applications, school, class) %>%
    summarise()

#prep data for student table
  student_table <- contact %>%
    left_join(applications, by = c("id"="applicant_c")) %>%
    inner_join(user, by=c("owner_id" = "id")) %>%
    inner_join(account, by = c("school_c" = "id")) %>%
    select(id,
           last_name = last_name.x,
           first_name = first_name.x,
           sub_status = application_submission_status_c,
           class = kipp_hs_class_c,
           counselor = name.x.x,
           current_school = currently_enrolled_school_c,
           hs_name = name.y.y,
           decision = application_status_c,
           matriculation = matriculation_decision_c)

# Transistions ####
transitions_by_student <- class_all %>%
  filter(type != "KIPP School") %>%
  group_by(id) %>%
  arrange(start_date) %>%
  mutate(transitions = row_number(id)) %>%
  filter(transitions == max(transitions))


transitions_contacts<-transitions_by_student %>%
  inner_join(contacts_summary %>%
               group_by(contact_c) %>%
               summarize(contacts = sum(N)), by = c("id" = "contact_c"))

 ##prophet ####
  dat_all <- contacts_details %>%
  filter(status_c %in% statuses[[1]]) %>%
  mutate(date_year = year(date_c),
  status = ifelse(date_c < ymd("2016-10-01") &
                  is.na(status_c), "Outreach", status_c )) %>%
  filter(!is.na(status)) %>%
  group_by_("date_year", "date_c", "status",
  "contact_c", "name.y", "kipp_hs_class_c")  %>%
  summarise() %>%
  ungroup() %>%
  group_by(date_year) %>%
  mutate(sy = silounloadr::calc_academic_year(date_c, format = "short"))

##cumulative####
df_final_csum <- dat_all %>%
  filter(status %in% "Successful") %>%
  distinct(contact_c, status, .keep_all=TRUE) %>%
  filter(!kipp_hs_class_c %in% 2021)  %>%
  group_by(date_year, date_c) %>%
  summarise(N=n()) %>%
  mutate(y = cumsum(N)) %>%
  ungroup() %>%
  select("ds" = date_c,
         y)

  #historical daily totals####
success <- dat_all %>%
  filter(status == "Successful") %>%
  group_by(sy, date_c) %>%
  summarize(y=n()) %>%
  ungroup() %>%
  select(ds = date_c, y)

outreach <- dat_all %>%
  filter(status == "Outreach") %>%
  group_by(sy, date_c) %>%
  summarize(y=n()) %>%
  ungroup() %>%
  select(ds = date_c, y)


both <- dat_all %>%
  group_by(sy, date_c) %>%
  summarize(y=n()) %>%
  ungroup() %>%
  select(ds = date_c, y)


  ##holidays####
  labor_day <- data_frame(
    holiday = "labor day",
    ds = as.Date(c("2010-09-06", "2011-09-05", "2012-09-03", "2013-09-02",
                   "2014-09-01", "2015-09-07", "2016-09-05"))
  )

  columbus_day <- data_frame(
    holiday = "columbus day",
    ds = as.Date(c("2010-10-11", "2011-10-10", "2013-10-14", "2014-10-13",
                   "2015-10-12", "2016-10-10"))
  )

  veterans_day <- data_frame(
    holiday = "veterans day",
    ds = as.Date(c("2010-11-11", "2011-11-11", "2013-11-11", "2014-11-11",
                   "2015-11-11", "2016-11-11"))
  )

  thanksgiving <-  data_frame(
    holiday = "thanksgiving",
    ds = as.Date(c("2010-11-25", "2011-11-24", "2012-11-21", "2013-11-27",
                   "2014-11-26", "2015-11-25", "2016-11-23")),
    lower_window = 0,
    upper_window = c(3,3,4,4,4,4,4)
  )

  winter_break <- data_frame(
    holiday = "winter break",
    ds = as.Date(c("2010-12-20", "2011-12-23", "2012-12-24", "2013-12-23",
                   "2014-12-22", "2015-12-21", "2016-12-23")),
    lower_window = 0,
    upper_window = c(14, 16, 14, 13, 13, 13, 16)
  )

  mlk <- data_frame(
    holiday = "mlk day",
    ds = as.Date(c("2011-01-17", "2012-01-16", "2013-01-21", "2014-01-20",
                   "2015-01-19", "2016-01-18", "2017-01-16"))
  )

  lincoln <- data_frame(
    holiday = "lincoln's birthday",
    ds = as.Date(c("2010-02-15","2011-02-21", "2012-02-20", "2013-02-18",
                   "2014-02-12", "2015-02-16", "2016-02-15", "2017-02-17", "2017-02-20"))
  )

  teach_apprec <- data_frame(
    holiday = "teacher appreciation",
    ds = as.Date("2015-03-20", "2016-03-25", "2017-03-17")
  )
  pulaski <- data_frame(
    holiday = "pulaski birthday",
    ds = as.Date(c("2010-03-01", "2011-03-07",  "2012-03-05"))
  )

  spring_break <- data_frame(
    holiday = "spring break",
    ds = as.Date(c("2011-04-18", "2012-04-02", "2013-04-01", "2014-04-14",
                   "2015-04-06", "2016-04-18", "2017-04-10")),
    lower_window = 0,
    upper_window = 4
  )

  memorial <- data_frame(
    holiday = "memorial day",
    ds = as.Date(c("2010-05-31","2011-05-30", "2012-05-28", "2013-05-27",
                   "2014-26-05", "2015-05-25", "2016-05-30", "2017-05-29"))
  )

  school_start <- data_frame(
    holiday = "school start",
    ds = as.Date(c("2010-08-23", "2011-08-22", "2012-08-13", "2013-08-19",
                   "2014-08-18", "2015-08-19", "2016-08-22"))
  )

  school_end <- data_frame(
    holiday = "school end",
    ds = as.Date(c("2010-06-18", "2011-06-17", "2012-06-15", "2013-06-14",
                   "2014-06-18", "2015-06-17", "2016-06-17", "2017-06-15"))
  )

  holidays <- bind_rows(labor_day, columbus_day, veterans_day, thanksgiving,
                        winter_break, mlk, lincoln, teach_apprec, pulaski,
                        spring_break, memorial, school_start, school_end)

##prophet model_unique####

  prophet_model <- prophet::prophet(df_final_csum, holidays = holidays, mcmc.samples = 500)
  future <- prophet::make_future_dataframe(prophet_model, periods = 6, freq = "weeks")
  forecast <- predict(prophet_model, future)
  prophet_dat <- prophet:::df_for_plotting(prophet_model, forecast)

## prophet models - success, outreach, both####

m_success <- prophet::prophet(success, holidays = holidays, mcmc.samples = 500)
future_success <- prophet::make_future_dataframe(m_success, periods = 6, freq = "weeks")
forecast_success <- predict(m_success, future_success)
prophetdf_success <- prophet:::df_for_plotting(m_success, forecast_success)

holiday_comps <- unique(m_success$holidays$holiday) %>% as.character()

m_outreach <- prophet::prophet(outreach, holidays = holidays, mcmc.samples = 500)
future_outreach <- prophet::make_future_dataframe(m_outreach, periods = 6, freq = "weeks")
forecast_outreach <- predict(m_outreach, future_outreach)
prophetdf_outreach <- prophet:::df_for_plotting(m_outreach, forecast_outreach)

m_both <- prophet::prophet(both, holidays = holidays, mcmc.samples = 500)
future_both <- prophet::make_future_dataframe(m_both, periods = 6, freq = "weeks")
forecast_both <- predict(m_both, future_both)
prophetdf_both <- prophet:::df_for_plotting(m_both, forecast_both)

prophetdf_success <- prophetdf_success %>%
  mutate(type = "successful")

prophetdf_outreach <- prophetdf_outreach %>%
  mutate(type = "outreach")

prophetdf_both <- prophetdf_both %>%
  mutate(type = "both")

prophet_all <- bind_rows(prophetdf_success, prophetdf_outreach, prophetdf_both)

##quad model and goal table ####

goal_tbl <- as.tbl(data.frame(goal_n = seq(0,740, by = 7.7),
                              ds = as.POSIXct(ymd("2017-01-01") + days(0:96))))

success_csum <- df_final_csum %>%
  filter(ds >= start)

q_mod <- lm(y ~ I(as.numeric(ds)) + I(as.numeric(ds)^2), data = success_csum)

goal_tbl$qmod <- predict(q_mod, goal_tbl[,"ds"])


# Save data ####
save(
     class_all,
     contacts_details,
     contacts_summary,
     hs_applications,
     students_data_bar,
     student_table,
     transitions_by_student,
     contacts_details,
     transitions_contacts,
     success_csum,
     prophet_dat,
     prophetdf_success,
     holiday_comps,
     prophetdf_outreach,
     prophetdf_both,
     prophet_all,
     goal_tbl,
     file = "/data/ktc.Rda"
     )

system("touch /srv/shiny-server/ktc/restart.txt")
