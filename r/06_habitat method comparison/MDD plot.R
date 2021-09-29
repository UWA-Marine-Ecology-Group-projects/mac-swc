
library(epibasix)
library(reshape2)


###PLOT for standard deviation from 20 points###

n <- c(200,100,40,20, 1) # A Vector (or single value) of fixed sample sizes.


vector <- SDfrom20points[['sd']]
vector<-toString(vector)
vector<-noquote(vector)
vector

sigma <- c(0.191574012184325, 0.0751443259485861, 0.161969927033118, 0.190574215429247, 0.149827155062807, 0.181181491526818, 0.148051137798127) # Vector (or single value) of fixed standard deviations sizes.
alpha <- 0.05	#  The desired Type I Error Rate
power <- 0.8	# The desired level of power, recall power = 1 - Type II Error.
two.tailed <-	FALSE # Logical, If TRUE calculations are based on a two-tailed Type I error, if FALSE, a one-sided calculation is performed.
diffDetect<-(diffDetect(n,sigma,alpha,power))
diffDetect #The Minimum Detectable Difference between two populations for fixed N (in rows) and sigma (in columns) is: 


levels(longsd3$broad)
dat<-data.table(diffDetect$n, diffDetect$delta)
dat
names(dat)[names(dat) == "0.191574012184325"] <- "Consolidated"
#names(dat)[names(dat) == "0"] <- "Hydroids"
names(dat)[names(dat) == "0.0751443259485861"] <- "Macroalgae"
names(dat)[names(dat) == "0.161969927033118"] <- "Open Water"
names(dat)[names(dat) == "0.190574215429247"] <- "Seagrasses"
names(dat)[names(dat) == "0.149827155062807"] <- "Sponges"
#(dat)[names(dat) == "0"] <- "Stony corals"
names(dat)[names(dat) == "0.181181491526818"] <- "Unconsolidated"
names(dat)[names(dat) == "0.148051137798127"] <- "Unknown"
dat

plot_data<-melt(dat, id.vars=c("V1"))

library(ggplot2)
theme_set(theme_classic())


V1 <- c(1,20,40,100,200)
TimetoAnalyse <- c(0.0166667,0.666667,1.33333,3.33333,6.66667) ## assuming a point takes 2 seconds to analyse
time<-data.frame(TimetoAnalyse,V1)

plot2<-merge.data.frame(time,plot_data)

coeff <- 10

ggplot(plot2, aes(x = V1, color = variable)) + 
  geom_line(aes(y=value), alpha=0.5, size=1.5) +
  geom_line(aes(y=TimetoAnalyse/coeff)) +
  ylim(0.0, 1.0) +
  labs(title = "Minimum Detectable Difference (20 points)", 
       x = "Sample Size (points per drop)", y = "Minimum Detectable Difference (% cover)") +
  scale_x_continuous(breaks=seq(0,200,25)) +
  scale_y_continuous(sec.axis = sec_axis(~.*coeff, name="Time to Analyse (Mins)"))



#########plot for 40 points###########


n <- c(200,100,40,20, 1) # A Vector (or single value) of fixed sample sizes.


vector2 <- SDfrom40points[['sd']]
vector2<-toString(vector2)
vector2<-noquote(vector2)
vector2

sigma2 <- c(0.162382557948498, 0.0362465696834033, 0.0974629491840174, 0.155159284569295, 0.173304424584649, 0.139588705131746, 0.159540843743063, 0.103735121284493) # Vector (or single value) of fixed standard deviations sizes.
alpha <- 0.05	#  The desired Type I Error Rate
power <- 0.8	# The desired level of power, recall power = 1 - Type II Error.
two.tailed <-	FALSE # Logical, If TRUE calculations are based on a two-tailed Type I error, if FALSE, a one-sided calculation is performed.
diffDetect2<-(diffDetect(n,sigma2,alpha,power))
diffDetect2 #The Minimum Detectable Difference between two populations for fixed N (in rows) and sigma (in columns) is: 
  

levels(longsd3$broad)
dat2<-data.table(diffDetect2$n, diffDetect2$delta)
dat2
names(dat2)[names(dat2) == "0.162382557948498"] <- "Consolidated"
names(dat2)[names(dat2) == "0.0362465696834033"] <- "Hydroids"
names(dat2)[names(dat2) == "0.0974629491840174"] <- "Macroalgae"
names(dat2)[names(dat2) == "0.155159284569295"] <- "Open Water"
names(dat2)[names(dat2) == "0.173304424584649"] <- "Seagrasses"
names(dat2)[names(dat2) == "0.139588705131746"] <- "Sponges"
#(dat)[names(dat) == "0"] <- "Stony corals"
names(dat2)[names(dat2) == "0.159540843743063"] <- "Unconsolidated"
names(dat2)[names(dat2) == "0.103735121284493"] <- "Unknown"
dat2

plot_data2<-melt(dat2, id.vars=c("V1"))

library(ggplot2)
theme_set(theme_classic())

ggplot(plot_data2, aes(x = V1, y=value, color = variable)) + 
  geom_line(aes(alpha=0.5), size=1.5) +
  ylim(0.0, 1.0) +
  labs(title = "Minimum Detectable Difference (40 points)", 
  x = "Sample Size (points per drop)", y = "Minimum Detectable Distance (% cover)") +
  scale_x_continuous(breaks=seq(0,200,25))
  


#########plot for 100 points###########


n <- c(200,100,40,20, 1) # A Vector (or single value) of fixed sample sizes.


vector3 <- SDfrom100points[['sd']]
vector3<-toString(vector3)
vector3<-noquote(vector3)
vector3

sigma3 <- c(0.141807907615703, 0.0229243435135126, 0.0823892261768419, 0.137386225508809, 0.165300196488314, 0.111571520469073, 0.0280039976487165, 0.143964994779908, 0.0888419347456298) # Vector (or single value) of fixed standard deviations sizes.
alpha <- 0.05	#  The desired Type I Error Rate
power <- 0.8	# The desired level of power, recall power = 1 - Type II Error.
two.tailed <-	FALSE # Logical, If TRUE calculations are based on a two-tailed Type I error, if FALSE, a one-sided calculation is performed.
diffDetect3<-(diffDetect(n,sigma3,alpha,power))
diffDetect3 #The Minimum Detectable Difference between two populations for fixed N (in rows) and sigma (in columns) is: 


levels(longsd3$broad)
dat3<-data.table(diffDetect3$n, diffDetect3$delta)
dat3
names(dat3)[names(dat3) == "0.141807907615703"] <- "Consolidated"
names(dat3)[names(dat3) == "0.0229243435135126"] <- "Hydroids"
names(dat3)[names(dat3) == "0.0823892261768419"] <- "Macroalgae"
names(dat3)[names(dat3) == "0.137386225508809"] <- "Open Water"
names(dat3)[names(dat3) == "0.165300196488314"] <- "Seagrasses"
names(dat3)[names(dat3) == "0.111571520469073"] <- "Sponges"
names(dat3)[names(dat3) == "0.0280039976487165"] <- "Stony corals"
names(dat3)[names(dat3) == "0.143964994779908"] <- "Unconsolidated"
names(dat3)[names(dat3) == "0.0888419347456298"] <- "Unknown"
dat3

plot_data3<-melt(dat3, id.vars=c("V1"))

library(ggplot2)
theme_set(theme_classic())

ggplot(plot_data3, aes(x = V1, y=value, color = variable)) + 
  geom_line(aes(alpha=0.5), size=1.5) +
  ylim(0.0, 1.0) +
  labs(title = "Minimum Detectable Difference (100 points)", 
       x = "Sample Size (points per drop)", y = "Minimum Detectable Distance (% cover)") +
  scale_x_continuous(breaks=seq(0,200,25))


#########plot for 200 points###########


n <- c(200,100,40,20, 1) # A Vector (or single value) of fixed sample sizes.


vector4 <- SDfrom200points[['sd']]
vector4<-toString(vector4)
vector4<-noquote(vector4)
vector4

sigma4 <- c(0.135851084199134, 0.0198357443629946, 0.0826667864064242, 0.129107850547705, 0.160166079967236, 0.0966232158100062, 0.0252629766734263, 0.138670392289807, 0.0860061342362042) # Vector (or single value) of fixed standard deviations sizes.
alpha <- 0.05	#  The desired Type I Error Rate
power <- 0.8	# The desired level of power, recall power = 1 - Type II Error.
two.tailed <-	FALSE # Logical, If TRUE calculations are based on a two-tailed Type I error, if FALSE, a one-sided calculation is performed.
diffDetect4<-(diffDetect(n,sigma4,alpha,power))
diffDetect4 #The Minimum Detectable Difference between two populations for fixed N (in rows) and sigma (in columns) is: 


levels(longsd3$broad)
dat4<-data.table(diffDetect4$n, diffDetect4$delta)
dat4
names(dat4)[names(dat4) == "0.135851084199134"] <- "Consolidated"
names(dat4)[names(dat4) == "0.0198357443629946"] <- "Hydroids"
names(dat4)[names(dat4) == "0.0826667864064242"] <- "Macroalgae"
names(dat4)[names(dat4) == "0.129107850547705"] <- "Open Water"
names(dat4)[names(dat4) == "0.160166079967236"] <- "Seagrasses"
names(dat4)[names(dat4) == "0.0966232158100062"] <- "Sponges"
names(dat4)[names(dat4) == "0.0252629766734263"] <- "Stony corals"
names(dat4)[names(dat4) == "0.138670392289807"] <- "Unconsolidated"
names(dat4)[names(dat4) == "0.0860061342362042"] <- "Unknown"
dat4

plot_data4<-melt(dat4, id.vars=c("V1"))

library(ggplot2)
theme_set(theme_classic())

ggplot(plot_data4, aes(x = V1, y=value, color = variable)) + 
  geom_line(aes(alpha=0.5), size=1.5) +
  ylim(0.0, 1.0) +
  labs(title = "Minimum Detectable Difference (200 points)", 
       x = "Sample Size (points per drop)", y = "Minimum Detectable Distance (% cover)") +
  scale_x_continuous(breaks=seq(0,200,25))
