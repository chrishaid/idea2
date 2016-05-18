library(lubridate)
library(stringr)
library(googlesheets)
library(tidyr)
library(dplyr)

setwd("/jobs/recruiting")

# from hadley, stops stripping of date and factor classes from ifelse
safe.ifelse <- function(cond, yes, no) structure(ifelse(cond, yes, no), class = class(yes))

gs_auth(token="/config/gs_token.rds")
system("chmod 777 .httr-oauth")

#sheetkey <- read.dcf('/config//recruit.dcf', fields='SHEET_KEY')[1]

sheet_name <- '16-17 Student Enrollment - Dashboard'

sheet <- gs_title(sheet_name)
#sheet_names<-gs_ws_ls(sheet)

# Registrations ####
regs <- gs_read(sheet, "Registrations")

#glimpse(regs)

# munge regs names

fix_names <- function(x) {
  x %>% str_replace_all("#", "n") %>%
    str_replace_all("%", "pct") %>%
    str_replace_all("\\s", "_") %>%
    str_replace_all("\\(|\\)", "") %>%
    str_replace_all("\\+/-", "deviation_from") %>%
    tolower()
}

regs_names <- names(regs)

regs_names_new <- regs_names %>% fix_names()

  

names(regs) <- regs_names_new

#glimpse(regs)

regs2 <- regs %>% 
  select(-starts_with("x"):-pct_seats_filled) %>%
  mutate(gr = ifelse(school == "Region", "All", gr),
        grade = factor(gr, levels=c("K", "5", "6", "All"), ordered = TRUE))

regs_tidy <- regs2 %>% 
  select(school, grade, overall_reg_goal,  starts_with("n_reg_as")) %>%
  tidyr::gather(variable, value, starts_with("n_reg_")) %>%
  mutate(date = str_extract(variable, "\\d/\\d{1,2}"),
         date = mdy(sprintf("%s/%s", date, year(today()))),
         date_eow = ceiling_date(date, unit = "week") - days(2)) %>%
  rename(actual=value) %>%
  group_by(school, grade, date_eow) %>%
  filter(date == max(date)) %>%
  ungroup()

regs_tidy %>% as.data.frame()

# let's get goals

reg_goals <- gs_read(sheet, "Goals (Updated)", range = "A12:R21")

#glimpse(reg_goals)

# munge names
reg_goals_names <- names(reg_goals) %>% fix_names()

names(reg_goals) <- reg_goals_names

# tidy goals
regs_goals_tidy<-reg_goals %>% 
  mutate(gr = ifelse(school == "Region", "All", gr),
         grade = factor(gr, levels=c("K", "5", "6", "All"), ordered = TRUE)) %>%
  select(school, grade, starts_with("reg_goal")) %>%
  gather(variable, goal, starts_with("reg_goal")) %>%
  mutate(date = str_extract(variable, "\\d/\\d{1,2}"),
         date = mdy(sprintf("%s/%s", date, year(today()))),
         date_eow = ceiling_date(date, unit = "week") - days(2)) %>%
  select(-variable)


# get original goals 
reg_goals_orig <- gs_read(sheet, "Goals (Original)", range = "A12:R22")

# munge names
reg_goals_orig_names <- names(reg_goals_orig) %>% fix_names()

names(reg_goals_orig) <- reg_goals_orig_names

# tidy goals
reg_goals_orig_tidy<-reg_goals_orig %>% 
  mutate(gr = ifelse(school == "Region", "All", gr),
         grade = factor(gr, levels=c("K", "5", "6", "All"), ordered = TRUE)) %>%
  select(school, grade, starts_with("reg_goal")) %>%
  gather(variable, goal, starts_with("reg_goal")) %>%
  mutate(date = str_extract(variable, "\\d/\\d{1,2}"),
         date = mdy(sprintf("%s/%s", date, year(today()))),
         date_eow = ceiling_date(date, unit = "week") - days(2)) %>%
  select(-variable) %>% 
  filter(date < min(regs_goals_tidy$date),
         goal != "---") %>%
  mutate(goal = as.integer(goal))

reg_goals_tidy_2 <- bind_rows(reg_goals_orig_tidy, regs_goals_tidy)


reg_goals_actual <- reg_goals_tidy_2 %>%
  left_join(regs_tidy, by = c("school", "grade", "date_eow")) %>%
  mutate(pct_of_goal = actual/goal,
         cols = ifelse(pct_of_goal>=1, "darkgreen", "red"),
         cols = ifelse(between(pct_of_goal,.9,0.99999), "gold", cols))

# Now for EIFs ####
eifs <- gs_read(sheet, "EIFs")

#glimpse(eifs)


eifs_names <- names(eifs)

eifs_names_new <- eifs_names %>% fix_names()

names(eifs) <- eifs_names_new

#glimpse(eifs)

eifs2 <- eifs %>% 
  select(-starts_with("x"):-pct_toward_overall_goal) %>%
  mutate(gr = ifelse(school == "Region", "All", gr),
         grade = factor(gr, levels=c("K", "5", "6", "All"), ordered = TRUE))

eifs_tidy <- eifs2 %>% 
  select(school, grade, overall_eif_goal,  starts_with("n_eifs_as")) %>%
  tidyr::gather(variable, value, starts_with("n_eifs_")) %>%
  mutate(date = str_extract(variable, "\\d/\\d{1,2}"),
         date = mdy(sprintf("%s/%s", date, year(today()))),
         date_eow = ceiling_date(date, unit = "week") - days(2)) %>%
  rename(actual=value) %>%
  group_by(school, grade, date_eow) %>%
  filter(date == max(date)) %>%
  ungroup()

eifs_tidy %>% as.data.frame()

# let's get goals

eif_goals <- gs_read(sheet, "Goals (Updated)", range = "A1:R10")

#glimpse(eif_goals)

# munge names
eif_goals_names <- names(eif_goals) %>% fix_names()

names(eif_goals) <- eif_goals_names

# tidy goals
eif_goals_tidy<-eif_goals %>% 
  mutate(gr = ifelse(school == "Region", "All", gr),
         grade = factor(gr, levels=c("K", "5", "6", "All"), ordered = TRUE)) %>%
  select(school, grade, starts_with("eif_goal")) %>%
  gather(variable, goal, starts_with("eif_goal")) %>%
  mutate(date = str_extract(variable, "\\d/\\d{1,2}"),
         date = mdy(sprintf("%s/%s", date, year(today()))),
         date_eow = ceiling_date(date, unit = "week") - days(2)) %>%
  select(-variable)


# get original goals 
eif_goals_orig <- gs_read(sheet, "Goals (Original)", range = "A1:R10")

# munge names
eif_goals_orig_names <- names(eif_goals_orig) %>% fix_names()

names(eif_goals_orig) <- eif_goals_orig_names

# tidy goals
eif_goals_orig_tidy<-eif_goals_orig %>% 
  mutate(gr = ifelse(school == "Region", "All", gr),
         grade = factor(gr, levels=c("K", "5", "6", "All"), ordered = TRUE)) %>%
  select(school, grade, starts_with("eif_goal")) %>%
  gather(variable, goal, starts_with("eif_goal")) %>%
  mutate(date = str_extract(variable, "\\d/\\d{1,2}"),
         date = mdy(sprintf("%s/%s", date, year(today()))),
         date_eow = ceiling_date(date, unit = "week") - days(2)) %>%
  select(-variable) %>% 
  filter(date < min(eif_goals_tidy$date),
         goal != "---") %>%
  mutate(goal = as.integer(goal))

eif_goals_tidy_2 <- bind_rows(eif_goals_orig_tidy, eif_goals_tidy)


eif_goals_actual <- eif_goals_tidy_2 %>%
  left_join(eifs_tidy, by = c("school", "grade", "date_eow")) %>%
  mutate(pct_of_goal = actual/goal,
         cols = ifelse(pct_of_goal>=1, "darkgreen", "red"),
         cols = ifelse(between(pct_of_goal,.9,0.99999), "gold", cols))

save(reg_goals_actual,
     eif_goals_actual,
     file="/data/recruiting.Rda")

system('touch /srv/shiny-server/recruit/restart.txt')
