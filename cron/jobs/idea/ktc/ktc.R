readRenviron("/config/.Renviron")

library(silounloadr)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(janitor)

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
     file = "/data/ktc.Rda"
     )

system("touch /srv/shiny-server/ktc/restart.txt")
