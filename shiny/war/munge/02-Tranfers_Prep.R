xferplot.1314<-transferplot.data.prep(Xfers.HSR.1314, Enrolled.131001)
xferplot.1415<-transferplot.data.prep(Xfers.HSR.1415, Enrolled.141001)
xferplot.1516<-transferplot.data.prep(Xfers.HSR.1516, Enrolled.151001)


xferplot.1314$Year<-"SY13-14"
xferplot.1415$Year<-"SY14-15"
xferplot.1516$Year<-"SY15-16"

xferplot<-rbind(xferplot.1314, xferplot.1415, xferplot.1516)


#non movers
xferplot.1314.nonmovers<-transferplot.data.prep(filter(Xfers.HSR.1314, EXITCODE!=2), Enrolled.131001)
xferplot.1415.nonmovers<-transferplot.data.prep(filter(Xfers.HSR.1415, EXITCODE!=2), Enrolled.141001)
xferplot.1516.nonmovers<-transferplot.data.prep(filter(Xfers.HSR.1516, EXITCODE!=2), Enrolled.151001)

xferplot.1314.nonmovers$Year<-"SY13-14"
xferplot.1415.nonmovers$Year<-"SY14-15"
xferplot.1516.nonmovers$Year<-"SY15-16"

xferplot.nm<-rbind(xferplot.1314.nonmovers, xferplot.1415.nonmovers, xferplot.1516.nonmovers)
