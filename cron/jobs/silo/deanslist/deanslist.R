setwd("/jobs/silo/deanslist")

library(silounloadr)
library(dplry)
library(httr)
library(deanslistr)

susp_list <- get_suspensions(domain = "kippchicago")

safe_unnest <- safely(unnest)

susp_2 <- susp_list %>%
  purrr::map(~safe_unnest(.x, Penalties)) %>%
  purrr::transpose()


ok <- susp_2$error %>% map_lgl(is_null)

ok

suspensions <- susp_2$result %>% keep(ok) %>% bind_rows()

suspensions_2 <- suspensions %>%
  mutate(school = str_extract(school_name, "K.{3,4}$"),
         StartDate = ymd(StartDate),
         Month = month(StartDate, label = TRUE, abbr = TRUE),
         NumDays = as.integer(NumDays)) %>%
  filter(str_detect(PenaltyName, "Suspension")) %>% 
  select(SuspensionID, 
         school,
         StartDate, 
         NumDays, 
         Month, 
         StudentID, 
         Month, 
         PenaltyName, 
         Category) 