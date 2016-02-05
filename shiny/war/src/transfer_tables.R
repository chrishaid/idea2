#### Transfers #### 

#table of Transfers by schoool 


Xfersreasons<-data.frame(EXITCODE=as.integer(c(1:11,99)),Reason=c("Dropped Out", 
                                                                    "Moved", 
                                                                    "Transport", 
                                                                    "Expelled", 
                                                                    "Behavior/Discipline", 
                                                                    "Academics", 
                                                                    "Avoid Retention", 
                                                                    "Special Needs", 
                                                                    "Other", 
                                                                    "Don't Know", 
                                                                    "Xfer Other KIPP", 
                                                                    "DNA"))
Xfer.students.table <- Xfers.HSR.1516 %>%  
  mutate(EXITCODE=as.integer(as.character(EXITCODE))) %>%
  left_join(Xfersreasons, by="EXITCODE")

message("Summarizing Xfer.table")
Xfer.table <- Xfer.students.table %>% 
  filter(EXITCODE!=11) %>%
  mutate(School=school_abbrev(SCHOOLID)) %>%
  group_by(Reason, School) %>%
  dplyr::summarize(N=n()) 

  
message("Casting Xfer.table")
# Create summary table
Xfer.table<-cast(as.data.frame(Xfer.table), 
                 Reason~School, 
                 margins=TRUE, 
                 fun.aggregate=sum)

#Xfer.table<-cast(ddply(Xfer.students.table, .(Reason, SCHOOLID), function(df)c(N=nrow(df))), Reason~SCHOOLID, margins=TRUE, fun.aggregate=sum)
# renaming some variable
message("Changin Xfer.table column names")
names(Xfer.table)[names(Xfer.table)=="Reason"]<-"Transfer Reason"
names(Xfer.table)[names(Xfer.table)=="(all)"]<-"Region"
#names(Xfer.table)<-c("Transfer Reason", "KAMS", "KAP", "KCCP", "KBCP", "Region")

levels(Xfer.table[,1])[length(Xfer.table[,1])]<-"Total"
#Xfer.table<-Xfer.table[,c("Transfer Reason", "KAP", "KAMS", "KCCP", "KBCP", "Region")]

# Create table of each student transferred with School, Grade, reason, comment.
message("Create Xfer.students.table")
Xfer.students.table <- Xfer.students.table %>% 
  mutate(LastFirst = paste(LAST_NAME, FIRST_NAME, sep=', '),
         School=school_abbrev(SCHOOLID),
         School=factor(School, levels=c("KAP", "KAMS", "KCCP", "KBCP")),
         Transferred=format(ymd_hms(EXITDATE), format="%b %d, %Y")) %>%
  select(School, 
         LastFirst,
         Grade=GRADE_LEVEL,
         Transferred,
         Reason,
         Comment=EXITCOMMENT) %>%
  arrange(School, Transferred)

# Xfer.students.table[, LastFirst:= paste(LAST_NAME, FIRST_NAME, sep=', ')]
# Xfer.students.table[SCHOOLID==78102, School:="KAP"]
# Xfer.students.table[SCHOOLID==7810, School:="KAMS"]
# Xfer.students.table[SCHOOLID==400146, School:="KCCP"]
# Xfer.students.table[SCHOOLID==400163, School:="KBCP"]
# 
# 
# Xfer.students.table[, School:=factor(School, levels=c("KAP", "KAMS", "KCCP", "KBCP"))]
# 
# Xfer.students.table[,Transferred:=format(ymd_hms(EXITDATE), format="%b %d, %Y")]
# 
# Xfer.students.table<-copy(Xfer.students.table[,list(School, 
#                                                     Grade=GRADE_LEVEL, 
#                                                     Student=LastFirst,
#                                                     Transferred, 
#                                                     Reason, 
#                                                     Comment=EXITCOMMENT)][order(School, Transferred)]
# )

