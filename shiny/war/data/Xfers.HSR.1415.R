#all Students enrolled on Oct 31 who have exit dates prior to 6/14/2013

#Xfers.HSR.1213<-subset(Enrolled.121003, ymd_hms(Enrolled.121003$EXITDATE)<ymd("13-06-14"))
#Enrolled.141001<-data.table(Enrolled.141001)
#Xfers.HSR.1314<-Enrolled.131001[!is.na(EXITCODE) & EXITCODE!="GR" & ymd_hms(EXITDATE)<=ymd("14-09-30")]
Xfers.HSR.1415 <- Enrolled.141001 %>%
  filter(!is.na(EXITCODE),
         EXITCODE!="GR",
         ymd_hms(EXITDATE)<=ymd("15-09-30"))