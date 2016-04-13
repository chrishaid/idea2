# Ensure that a given date is a week day.  If not, then set for nearest monday.

force_weekday <- function(date, offset = 0) {
  
  date <- lubridate::ymd(date)
  
  if(lubridate::wday(date) == 1) date <- date + lubridate::days(1 + offset) # Sunday
  if(lubridate::wday(date) == 7)  date <- date + lubridate::days(2 + offset) # Saturday
  
  # return
  date
  
}

# Get membership on a given date

get_membership_on_date <- function(con, date, offset = 0) {
  
  # Ensure date is a weekday and then format
  date <- force_weekday(date, offset)
  
  #format date to YYYY-MM-DD
  date <- lubridate::parse_date_time(date, "ymd")
  date_formatted <- format(date, "%Y-%m-%d")
  
  qry <- sprintf(
          "SELECT * FROM membership WHERE CALENDARDATE = '%s'",
          date_formatted
         )
  
  # return
  tbl(con, sql(qry))
  
  
  
  
  
}



#GEt member ship on a certian day

