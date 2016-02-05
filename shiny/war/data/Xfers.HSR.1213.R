#all Students enrolled on Oct 31 who have exit dates prior to 9/30/2013

#Xfers.HSR.1213<-subset(Enrolled.121003, ymd_hms(Enrolled.121003$EXITDATE)<ymd("13-06-14"))
#Enrolled.121003<-data.table(Enrolled.121003)
#Xfers.HSR.1213<-Enrolled.121003[!is.na(EXITCODE) & EXITCODE!="GR" & ymd_hms(EXITDATE)<=ymd("13-09-30")]
Xfers.HSR.1213 <- Enrolled.121003 %>%
  filter(!is.na(EXITCODE),
         EXITCODE!="GR",
         ymd_hms(EXITDATE)<=ymd("13-09-30"))