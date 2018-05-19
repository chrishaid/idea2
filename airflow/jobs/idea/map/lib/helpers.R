# Seperate CDF 

separate_cdf <- function(combinded_cdf, district_name = "Not provided"){
  ar_names <- names(ex_CombinedAssessmentResults) %>% tolower
  stu_names <- names(ex_CombinedStudentsBySchool) %>% tolower
  
  if (!"districtname" %in% tolower(names(combinded_cdf))) {
    combinded_cdf <- combinded_cdf %>% mutate_(districtname = ~district_name)
  }
  
  roster<-combinded_cdf %>%
    select_(.dots = stu_names) %>%
    unique
  
  cdf<-combinded_cdf %>% select(-studentlastname:-studentfirstname,
                                -studentmi:-studentgender,
                                -grade) %>%
    mutate(testid=as.character(testid))
  
  out <- list(cdf = cdf,
              roster = roster)
  
}