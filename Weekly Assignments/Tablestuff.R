library(dplyr)
library(ggplot2)
library(WDI)
?WDI

DF<-cbind(rename(WDI(indicator="IT.NET.USER.P2"), web.cap=IT.NET.USER.P2),
          rename(WDI(indicator="SE.SEC.ENRR"), sec.cap= SE.SEC.ENRR),
          rename(WDI(indicator="SE.TER.ENRR"), ter.cap= SE.TER.ENRR),
          rename(WDI(indicator="SE.XPD.TOTL.GB.ZS"), edubux= SE.XPD.TOTL.GB.ZS),
          rename(WDI(indicator="NY.GDP.PCAP.CD"), gdp.cap= NY.GDP.PCAP.CD),
          rename(WDI(indicator="SH.HIV.1524.FE.ZS"), fHIV= SH.HIV.1524.FE.ZS),
          rename(WDI(indicator="SH.HIV.1524.MA.ZS"), mHIV= SH.HIV.1524.MA.ZS),
          rename(WDI(indicator="SP.MTR.1519.ZS"),teenmom= SP.MTR.1519.ZS),
          rename(WDI(indicator="SH.DYN.AIDS.ZS"), tHIV=SH.DYN.AIDS.ZS),
          rename(WDI(indicator="SP.DYN.CONU.ZS"), contrac=SP.DYN.CONU.ZS),
          rename(WDI(indicator="EG.ELC.ACCS.ZS"),elec.cap=EG.ELC.ACCS.ZS),
          rename(WDI(indicator="SE.ENR.TERT.FM.ZS"),ter.gdr=SE.ENR.TERT.FM.ZS),
          rename(WDI(indicator="SL.EMP.INSV.FE.ZS"),wrk.fem=SL.EMP.INSV.FE.ZS),
          rename(WDI(indicator="SP.POP.TOTL"),pop=SP.POP.TOTL))

attach(DF)
DF2<-as.data.frame(cbind(country,year,web.cap,elec.cap,gdp.cap,sec.cap,ter.cap,edubux,fHIV,mHIV,tHIV,teenmom,contrac, ter.gdr, wrk.fem,pop))
detach(DF)
DF<-DF2
rm(DF2)


write.csv(DF, file="WDIdata.csv")

DF<-read.csv(file="WDIdata.csv")
str(DF)
DF.c<-DF[239:1736,]
DF.c.2011<-filter(DF.c, year==2011)
DF.c.2010<-filter(DF.c, year==2010)
DF.c.2009<-filter(DF.c, year==2009)
DF.c.2011<-filter(DF.c, year==2011)


DF$web.cap<-as.numeric(as.character(DF$web.cap))
DF$sec.cap<-as.numeric(as.character(DF$sec.cap))
DF$ter.cap<-as.numeric(as.character(DF$ter.cap))
DF$edubux<-as.numeric(as.character(DF$edubux))
DF$gdp.cap<-as.numeric(as.character(DF$gdp.cap))
DF$year<-as.numeric(as.character(DF$year))
DF$fHIV<-as.numeric(as.character(DF$fHIV))
DF$mHIV<-as.numeric(as.character(DF$mHIV))
DF$teenmom<-as.numeric(as.character(DF$teenmom))
DF$tHIV<-as.numeric(as.character(DF$tHIV))
DF$contrac<-as.numeric(as.character(DF$contrac))
DF$elec.cap<-as.numeric(as.character(DF$elec.cap))
DF$ter.gdr<-as.numeric(as.character(DF$ter.gdr))
DF$wrk.fem<-as.numeric(as.character(DF$wrk.fem))
DF$pop<-as.numeric(as.character(DF$pop))


str(DF.c)
str(quantile(DF.c.2011$web.cap, na.rm=T))
DF.c.2011.q<-cbind(DF.c.2011,quartile)
summary(aov(gdp.cap~quartile, data=DF.c.2011.q))
?aov
quartile.web<- (as.integer(cut(DF.c.2011$web.cap, quantile(DF.c.2011$web.cap, probs=0:4/4, na.rm=T), include.lowest=TRUE)))
half.web<- (as.integer(cut(DF.c.2011$web.cap, quantile(DF.c.2011$web.cap, probs=0:2/2, na.rm=T), include.lowest=TRUE)))
quartile.gdp<- (as.integer(cut(DF.c.2011$gdp.cap, quantile(DF.c.2011$gdp.cap, probs=0:4/4, na.rm=T), include.lowest=TRUE)))
attach(DF.c)
plot(contrac, wrk.fem)
summary(lm(wrk.fem~contrac+gdp.cap+))
?quantile

?glm





ggplot(data = na.omit(DF.c.2011), aes(x = quartile.gdp, fill = half.web)) + 
  geom_bar(position = "fill") + 
  theme_bw()

T1 <- xtabs(~half.web + quartile.gdp, data = DF.c.2011)
T1
T2 <- prop.table(T1, 2)
T2
barplot(T2, xlab="GDP quartile", ylab="Internet use, dark=low, light=high")




chisq.test(T1)

################################################################################

ggplot(data=DF.c.2011, aes(y=ter.cap, x=gdp.cap,color=(half.web==1)))+
  geom_point(colour=half.web)+
  geom_smooth(method=lm)+
  labs(title="Tertiary Education by GDP, 2011",
       y="% College age in College",
       x="GDP per capita"
  )

ggplot(data=DF.c.2011, aes(y=log(ter.cap), x=log(gdp.cap),color=(half.web==1)))+
  geom_point(colour=half.web)+
  geom_smooth(method=lm)+
  labs(title="Tertiary Education by GDP, 2011",
       y="log % College age in College",
       x="Log GDP per capita"
  )

summary(lm(log(ter.cap)~web.cap+log(gdp.cap)*edubux, data=DF.c.2011))
summary(lm(log(ter.cap)~web.cap, data=DF.c.2011))


ggplot(data=DF.c.2011, aes(y=ter.cap, x=web.cap))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title="Tertiary Education by GDP, 2011",
       y="% College age in College",
       x="web access per capita"
  )


ggplot(data=DF.c.2011, aes(y=wrk.fem, x=web.cap))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title="Women in workforce, 2011",
       y="% Workforce female",
       x="web access per capita"
  )

summary(lm(wrk.fem~web.cap+gdp.cap+edubux, data=DF.c.2011))
summary(lm(wrk.fem~web.cap, data=DF.c.2011))

?cite
cite(WDI)
?citation
citation()
citation("WDI")


ggplot(data=DF.c.2011, aes(y=log(tHIV), x=web.cap))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title="HIV, 2011",
       y="log % Adults with HIV",
       x="web access per capita"
  )


summary(lm(log(tHIV)~web.cap+log(gdp.cap)+contrac, data=DF.c.2011))
summary(lm(log(tHIV)~web.cap, data=DF.c.2011))
)
citation()
citation("ggplot2")
citation("WDI")
