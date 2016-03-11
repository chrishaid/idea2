# Seperate CDF 

separate_cdf <- function(combinded_cdf, district_name = "Not provided"){
  ar_names <- names(ex_CombinedAssessmentResults)
  stu_names <- names(ex_CombinedStudentsBySchool)
  
  if (!"districtname" %in% tolower(names(combinded_cdf))) {
    combinded_cdf <- combinded_cdf %>% mutate_(DistrictName = ~district_name)
  }
  
  roster<-combinded_cdf %>%
    select_(.dots = stu_names) %>%
    unique
  
  cdf<-combinded_cdf %>% select(-StudentLastName:-StudentFirstName,
                                -StudentMI:-StudentGender,
                                -Grade) %>%
    mutate(TestID=as.character(TestID))
  
  out <- list(cdf = cdf,
              roster = roster)
  
}