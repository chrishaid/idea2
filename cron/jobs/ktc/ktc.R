readRenviron("/config/.Renviron")

library(silounloadr)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

setwd("/jobs/ktc/")
source("lib/library.R")


# Get KTC data's from silo2 ####

contact <- get_alumni_db("contact", collect = TRUE)
account <- get_alumni_db("account", collect = TRUE)
enrollment_c <- get_alumni_db("enrollment_c", collect = TRUE)
college_persistence <- get_alumni_db("college_persistence_c", collect = TRUE)
user <- get_alumni_db("user", collect = TRUE)

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
     transitions_by_student,
     contacts_details,
     transitions_contacts,
     file = "/data/ktc.Rda"
     )

system("touch /srv/shiny-server/ktc/restart.txt")
