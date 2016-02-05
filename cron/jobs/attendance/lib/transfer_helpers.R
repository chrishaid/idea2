rm#extract month and date of transfers
#need cumulative summation that handles NAs as 0s for next function to work

cum.na <- function(x) { 
  # Calcualtes cumsum by treating NAs as 0s
  # Args:
  #       x: a vector over which to calculate a cumulative sum
  # Returns:
  #       x: a vector fo the cumulative sums of the argument
  x[which(is.na(x))] <- 0 
  return(cumsum(x)) 
} 

school_abbrev <- function(.data){
  
  look_up <- function(x){
    z <- switch(as.character(x), 
                      "78102"  = "KAP",
                      "7810"   = "KAMS",
                      "400146" = "KCCP",
                      "400163" = "KBCP"
                )
    z
  }
    out <- mapply(look_up, .data)
  as.character(out)
}

transferplot.data.prep <- function (transfers.data, enrolled.data) {
  # Creates a ready to ploy month cumulative sum of powerschool exit data using the list fo transferd students 
  
  #require(data.table)
  require(lubridate)
  require(reshape2)
  
  #enrolled.data<-as.data.table(enrolled.data)
  #transfers.data<-as.data.table(transfers.data)
  
  hsr_enrolled <- enrolled.data %>%
    group_by(SCHOOLID) %>%
    summarize(HSR_Enrolled=n()) %>%
    mutate(School=school_abbrev(SCHOOLID)) %>%
    select(-SCHOOLID)
  
  #hsr_enrolled<-enrolled.data[,list(HSR_Enrolled=.N),by=SCHOOLID]
  #hsr_enrolled[SCHOOLID==78102,  School:= "KAP"]
  #hsr_enrolled[SCHOOLID==7810,   School:="KAMS"]
  #hsr_enrolled[SCHOOLID==400146, School:="KCCP"]
  #hsr_enrolled[SCHOOLID==400163, School:="KBCP"]
  #hsr_enrolled[,SCHOOLID:=NULL]
  #setkey(hsr_enrolled, School)
  
  ############ Change the filter below to deal with interKIPP transfers ####
  transfers.data <- transfers.data %>%
    mutate(
           Month=month(ymd_hms(transfers.data$EXITDATE), label=TRUE),
           Week=week(ymd_hms(transfers.data$EXITDATE))
           ) %>%
    filter(EXITCODE!=11)
  #exit gracefuly if nrow(transfers.data)==0
  
  #transfers.data$Month<-lubridate::month(ymd_hms(transfers.data$EXITDATE), label=TRUE)
  #transfers.data$Week<-lubridate::week(ymd_hms(transfers.data$EXITDATE))
  
  monthly.by.school <- transfers.data %>%
    group_by(SCHOOLID, Month) %>%
    summarize(N=n()) %>%
    arrange(Month)
  
  #monthly.by.school<-transfers.data[,list(N=.N), by=list(SCHOOLID, Month)][order(Month)]
  #monthly.by.school<-arrange(ddply(transfers.data, .(SCHOOLID, Month), summarise, N=length(Month)), Month)
  
  ####Need to build data fram that contains actual YTD, as well as 10% annual ceiling for each school.  Months need to 
  # start with October
  mons<-c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")
  mons<-factor(mons, levels=mons, ordered=TRUE)
  #Schools<-unique(as.character(monthly.by.school$School))
  Schools<-factor(c("KAP", "KAMS","KCCP", "KBCP" ), 
                  levels=c("KAP", "KAMS","KCCP", "KBCP" ))
  
  
  cum.monthly<-monthly.by.school %>%
    mutate(Month=factor(as.character(Month), levels=mons)) %>%
    dplyr::arrange(SCHOOLID, Month) %>%
    group_by(SCHOOLID) %>%
    mutate(YTD=with_order(Month, cumsum, x=N)) %>%
    select(SCHOOLID, Month, N, YTD) %>%
    mutate(School=school_abbrev(SCHOOLID),
           School=factor(School, levels=c("KAP", "KAMS","KCCP", "KBCP")))
  
  #cum.monthly<-monthly.by.school[,list(Month, N, YTD=cumsum(N)), by=SCHOOLID]
  #cum.monthly<-ddply(monthly.by.school, .(SCHOOLID), transform, YTD=cumsum(N))
  
  #cum.monthly[SCHOOLID==78102,  School:= "KAP"]
  #cum.monthly[SCHOOLID==7810,   School:="KAMS"]
  #cum.monthly[SCHOOLID==400146, School:="KCCP"]
  #cum.monthly[SCHOOLID==400163, School:="KBCP"]
  

    
  
  #School<-c("KAPS", "KAMS", "KCCP

  redline<-enrolled.data %>%
    group_by(SCHOOLID) %>%
    summarize(N.Enrolled=n()) %>%
    mutate(N.10pct=N.Enrolled*.1,
           line.month=N.10pct/12,
           School=school_abbrev(SCHOOLID),
           School=factor(School, levels=c("KAP", "KAMS","KCCP", "KBCP" ))) %>%
    select(School, line.month)
  
  #redline<-ddply(enrolled.data, .(SCHOOLID), summarise, N.Enrolled=length(STUDENTID))
#   redline[,N.10pct:=N.Enrolled*.1]
#   redline[,line.month:=N.10pct/12]
#   redline[SCHOOLID==78102,  School:= "KAP"]
#   redline[SCHOOLID==7810,   School:="KAMS"]
#   redline[SCHOOLID==400146, School:="KCCP"]
#   redline[SCHOOLID==400163, School:="KBCP"]
#   redline<-redline[,list(School, line.month)]
  
xferplot.data<-expand.grid(Month=factor(as.character(mons)), 
                             School=Schools, 
                             Variable=c("Cumulative Transfers", "Ceiling")) %>%
  left_join(y=redline, 
            by="School") %>%
  dplyr::rename(Value=line.month) %>%
  mutate(Value=ifelse(Variable=="Cumulative Transfers", NA, Value))
  

  #xferplot.data$Value[xferplot.data$Variable=="Cumulative Transfers"]<-NA
  
xfer.cums<-cum.monthly %>% 
  mutate(Month=as.character(Month)) %>%
  select(School, Month, N, YTD) %>%
   mutate(Variable="Cumulative Transfers")
  
#[,list(School, Month, N)]
  #xfer.cums<-cum.monthly[,c("School", "Month", "N")]
  #xfer.cums[,Variable:="Cumulative Transfers"]
  
xferplot.merge<-left_join(xferplot.data, 
                      xfer.cums, 
                      by=c("School", "Month", "Variable"), 
                      ) %>%
  mutate(Value=ifelse(!is.na(N), N, Value),
         Month=factor(as.character(Month), levels=mons)) %>%
  select(School, Month, Variable, Value) %>%
  arrange(School, Variable, Month) 
  
  #xferplot.merge<-merge(xferplot.data, 
  #                      xfer.cums, 
  #                      by=c("School", "Month", "Variable"), 
  #                      all.x=TRUE)
  #xferplot.merge$Value[!is.na(xferplot.merge$N)]<-xferplot.merge$N[!is.na(xferplot.merge$N)]
  #xferplot.merge<-arrange(xferplot.merge[,c(1:4)], School, Variable, Month)
  
#   cum.na <- function(x) { 
#     x[which(is.na(x))] <- 0 
#     return(cumsum(x)) 
#   } 
  

xferplot <- xferplot.merge %>%
  group_by(School, Variable) %>%
  mutate(CumVal=cum.na(Value)) %>%
  arrange(School, Variable, Month) %>%
  mutate(Value=ifelse(!is.na(Value), CumVal, Value))

  #xferplot<-arrange(ddply(xferplot.merge, .(School, Variable), transform, CumVal= cum.na(Value)),School, Variable, Month)
  
  #xferplot$Value[!is.na(xferplot$Value)]<-xferplot$CumVal[!is.na(xferplot$Value)]
  
  #xferplot<-data.table(xferplot)
  
  #setkey(xferplot, School)
  
xferplot2<- left_join(hsr_enrolled, xferplot, by="School") %>%
  mutate(Attrition_Rate=round(CumVal/HSR_Enrolled,3),
         Month=factor(as.character(Month), levels=mons),
         School=factor(as.character(School), levels=c("KAP", "KAMS", "KCCP", "KBCP"))
         ) %>%
  arrange(School, Variable, Month)
  
  
#   xferplot[hsr_enrolled]
#   
#   xferplot2[,Attrition_Rate:=round(CumVal/HSR_Enrolled,3)]
#   
#   xferplot2[,Month:=factor(as.character(Month), levels=mons)]
#   xferplot2[,School:=factor(as.character(School), levels=c("KAP", "KAMS", "KCCP", "KBCP"))]
#   xferplot2[order(School, Variable, Month)]
#   as.data.frame(xferplot2)
  #x<-as.data.frame(xferplot2)
  #arrange(x, School, Variable, Month)
xferplot2
}