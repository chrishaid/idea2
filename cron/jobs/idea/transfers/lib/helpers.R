# Ensure that a given date is a week day.  If not, then set for nearest monday.

force_weekday <- function(date, offset = 0) {

  date <- lubridate::ymd(date)

  if(lubridate::wday(date) == 1) date <- date + lubridate::days(1 + offset) # Sunday
  if(lubridate::wday(date) == 7)  date <- date + lubridate::days(2 + offset) # Saturday

  # return
  date

}

# drop fivetran columns 
drop_fivetran_cols <- . %>% select(-starts_with("_fivetran"))

# Get membership on a given date

get_membership_on_date <- function(date, offset = 0) {

  # Ensure date is a weekday and then format
  date <- force_weekday(date, offset)

  #format date to YYYY-MM-DD
  date <- lubridate::parse_date_time(date, "ymd")
  date_formatted <- format(date, "%Y-%m-%d %H:%M")
  filter_date <- sprintf("calendardate == '%s'", date_formatted)
  
  
  out <- get_powerschool("ps_membership_defaults") %>%
    select(studentid, schoolid, calendardate, starts_with("_fivetran_deleted")) %>%
    filter_("!`_fivetran_deleted`", 
            filter_date) %>%
    drop_fivetran_cols()

  # return
  collect(out)





}



# add schools' names
school_names <- function(school_number, as_factor = TRUE){

  names_lookup <- function(x) {
    switch(as.character(x),
           "78102" = "KAP",
           "7810" = "KAMS",
           "400146" = "KCCP",
           "400163" = "KBCP",
           "4001802" = "KOP",
           "400180" = "KOA")
  }
  out <- vapply(school_number, FUN = names_lookup, "character")
  if(as_factor) out <- factor(out, levels = c("KAP", "KAMS", "KCCP", "KBCP", "KOP", "KOA"))

  out

}
