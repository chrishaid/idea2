setwd("/jobs/silo/teachboost")

library(futile.logger)

# set up logging

flog.threshold(TRACE)
flog.appender(appender.tee("logs/teachboost.log"))

flog.info("Setting environmental vars")
readRenviron("/config/.Renviron")
Sys.setenv("GCS_AUTH_FILE" = "/config/gcs/kipp-chicago-silo-2-3789ce3e3415.json")

flog.info("Loading packages")
library(silounloadr)
library(tidyr)
library(httr)
library(teachboostr)
library(purrr)
library(googleCloudStorageR)
library(janitor)
library(stringr)
library(lubridate)
library(dplyr)

#function to use with object_function in gcs_upload call
f_json <- function(input, output) jsonlite::write_json(input, path = output, pretty = T, dataframe = "rows")
f_csv <- function(input, output) readr::write_csv(input, path = output)


flog.info("Logging into GCS and setting global bucket on GCS to teachboost")
gcs_global_bucket("teachboost")


flog.info("Connecting to and pulling teachboost forms data")
forms_resp <- ftry(get_tb("forms"))

flog.info("Unpacking forms data")
forms_df <- forms_resp %>% 
  unpack_tb()

flog.info("Uploading forms data to GCS")
gcs_upload(forms_df, 
           name = "forms/files/forms.csv",
           object_function = f_csv)


flog.info("Connecting to and pulling teachboost users data")
users_resp <- ftry(get_tb("users"))

flog.info("Unpacking users data")
users_df <- users_resp %>% 
  unpack_tb() 

flog.info("Stripping group_access data from users")

group_access_df <- users_df %>%
  dplyr::select(user_id = id , dplyr::contains("group_access.")) %>%
  gather(variable, value, dplyr::contains("group_access")) %>%
  dplyr::filter(!is.na(value)) %>%
  tidyr::separate(col = variable, 
                  into = c("toss", "group_id"), 
                  remove = TRUE, 
                  sep = "\\.") %>%
  select(-toss) %>%
  mutate(user_id = as.integer(user_id),
         group_id = as.integer(group_id))

users_df <- users_df %>% select(-dplyr::contains("group_access."))

flog.info("Stripping roles data from users")

roles_df <- users_df %>%
  dplyr::select(user_id = id , dplyr::contains("roles")) %>%
  gather(variable, value, dplyr::contains("roles")) %>%
  dplyr::filter(!is.na(value)) %>%
  tidyr::separate(col = variable, 
                  into = c("toss", "group_id", "school_year"), 
                  remove = TRUE, 
                  sep = "\\.") %>%
  select(-toss) %>%
  mutate(user_id = as.integer(user_id),
         group_id = as.integer(group_id),
         school_year = as.integer(school_year))

users_df <- users_df %>% select(-dplyr::contains("roles.")) %>%
  mutate(id = as.integer(id))
  


flog.info("Uploading users data to GCS")
gcs_upload(users_df, 
           name = "users/files/users.json", 
           object_function = f_json,
           type = 'application/json')

flog.info("Uploading roles data to GCS")
gcs_upload(roles_df, 
           name = "roles/files/roles.csv", 
           object_function = f_csv)

flog.info("Uploading group_access data to GCS")
gcs_upload(group_access_df, 
           name = "group_access/files/group_access.csv", 
           object_function = f_csv)


flog.info("Connecting to and pulling teachboost groups data")
groups_resp <- ftry(get_tb("groups"))

flog.info("Unpacking groups data")
groups_df <- groups_resp %>% 
  unpack_tb() %>%
  mutate(id = as.integer(id),
         parent_id = as.integer(parent_id))
  


flog.info("Uploading groups data to GCS")
gcs_upload(groups_df, 
           name = "groups/files/groups.csv", 
           object_function = f_csv)


flog.info("Connecting to and pulling teachboost templates data")
templates_resp <- ftry(get_tb("templates"))

flog.info("Unpacking groups data")
templates_df <- templates_resp %>% 
  unpack_tb() %>%
  mutate(id = as.integer(id))

flog.info("Uploading groups data to GCS")
gcs_upload(templates_df, 
           name = "templates/files/templates.json", 
           object_function = f_json)

flog.info("TeachBoost uploads to GCS Complete")

