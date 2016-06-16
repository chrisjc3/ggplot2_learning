
library(gridExtra)
library(ggplot2)
library(haven)
library(Rmisc)
library(reshape2)

#################################################################################
#PUBLICALLY Posted data;                                                        #
#Thanks to the great folks at https://www.icpsr.umich.edu/icpsrweb/landing.jsp  #
#and whoever else was involved in gathering                                     #
#################################################################################
#################################################################################
# ICPSR 36411                                                                   #
#                                                                               #  
# United States Department of Commerce. Bureau of Census, United States         #
# Department of Labor. Bureau of Labor Statistics, and Corporation for          #
# National and Community Service. Current Population Survey, September 2015     #
#################################################################################

load("C:/Users/C/Desktop/data/ICPSR_36411/DS0001/36411-0001-Data.rda")
data<-as.data.frame(da36411.0001)

#Completed survey
compdata <- data[grep("(001)",data$HUFINAL),]
#Northeast
nedata <- compdata[grep("(1)",compdata$GEREG),]
#Non-Transients
netdata <- nedata[grep("(1)",nedata$HEHOUSUT),]
#Single (for single income figures)
singdata <- netdata[grep("(03)|(04)|(06)|(07)",netdata$HRHTYPE),]
#Young/Working Age/Single Income
ynedata <- singdata[which(singdata$PRTAGE < 30 & singdata$PRTAGE >= 20),]
#Clean up
rm(data, compdata, nedata, netdata, da36411.0001, singdata)
# HETENURE = Own/Rent/Given
# HEHOUSUT = Type of Unit
# HEFMINC = Family Income
# PEEDUCA = Education
# PRTAGE = AGE
vars <- c("HETENURE","HEFAMINC","PEEDUCA", "PRTAGE", "HRHTYPE")
incdata <- ynedata[,vars]

mdata<-incdata
# REDUCE TO BASE CODES & MELT
mdata<-apply(mdata, 1, function(x) gsub("\\((\\d+)\\).+","\\1",x))
mdata<-melt(mdata)

# SEPERATE TO VARS (FOR EASY GGPLOT SELECTIONS)
# Reduces all to Var1/value combos


ownst<-mdata[which(mdata$Var1 == "HETENURE"),]
#VALUE LEGEND:#
# 1 OWNED OR BEING BOUGHT BY A HH MEMBER
# 2 RENTED FOR CASH
# 3 OCCUPIED WITHOUT PAYMENT OF CASH RENT 

finc<-mdata[which(mdata$Var1 == "HEFAMINC"),]
#VALUE LEGEND:#
# 1 LESS THAN $5,000
# 2 5,000 TO 7,499
# 3 7,500 TO 9,999
# 4 10,000 TO 12,499
# 5 12,500 TO 14,999
# 6 15,000 TO 19,999
# 7 20,000 TO 24,999
# 8 25,000 TO 29,999
# 9 30,000 TO 34,999
# 10 35,000 TO 39,999
# 11 40,000 TO 49,999
# 12 50,000 TO 59,999
# 13 60,000 TO 74,999
# 14 75,000 TO 99,999
# 15 100,000 TO 149,999
# 16 150,000 OR MORE

pedu<-mdata[which(mdata$Var1 == "PEEDUCA"),]
#VALUE LEGEND:#
# 31 LESS THAN 1ST GRADE
# 32 1ST, 2ND, 3RD OR 4TH GRADE
# 33 5TH OR 6TH GRADE
# 34 7TH OR 8TH GRADE
# 35 9TH GRADE
# 36 10TH GRADE
# 37 11TH GRADE
# 38 12TH GRADE NO DIPLOMA
# 39 HIGH SCHOOL GRAD-DIPLOMA OR EQUIV (GED)
# 40 SOME COLLEGE BUT NO DEGREE
# 41 ASSOCIATE DEGREE-OCCUPATIONAL/VOCATIONAL
# 42 ASSOCIATE DEGREE-ACADEMIC PROGRAM
# 43 BACHELOR'S DEGREE (EX: BA, AB, BS)
# 44 MASTER'S DEGREE (EX: MA, MS, MEng, MEd, MSW)
# 45 PROFESSIONAL SCHOOL DEG (EX: MD, DDS, DVM)
# 46 DOCTORATE DEGREE (EX: PhD, EdD) 

page<-mdata[which(mdata$Var1 == "PRTAGE"),]
#VALUE LEGEND:#
# 00-79 Age in Years
# 80 80-84 Years Old
# 85 85+ Years Old 

hhtype<-mdata[which(mdata$Var1 == "HRHTYPE"),]
#VALUE LEGEND:#
# 00-79 Age in Years
# 80 80-84 Years Old
# 85 85+ Years Old 

rm(mdata, ynedata, incdata)


x<-finc[,3]
# c('5000','7499','9999','12499','14999','19999','24999','29999','34999','39999','49999','59999','74999','99999','149999','150000')
# c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16')
y<-pedu[,3]
# c('1st','2nd','5th','7th','9th','10th','11th','12th','HIGH SCHOOL','SOME COLLEGE','ASSOCIATES VOCATIONAL','ASSOCIATES DEGREE','BACHELOR'S DEGREE','MASTER'S DEGREE','PROFESSIONAL DEGREE','DOCTORATE DEGREE')
# c('31','32','33','34','35','36','37','38','39','40','41','42','43','44','45','46')

dat<-cbind(x,y)
colnames(dat)<-c("x1", "y1")
dat<-as.data.frame(dat)
#######################################################
#NOW WE'RE DOWN TO WHAT WE WANT:                      #
#NORTH EASTERN YOUNG PEOPLE LIVING NON TRANSIENT      #
#######################################################

ggplot() + 
  geom_bar(data = dat, aes(x=as.character(x1),fill=as.character(y1)), position = "dodge") +
  scale_x_discrete(labels=c('-5000','7499','9999','12499','14999','19999','24999','29999','34999','39999','49999','59999','74999','99999','149999','150000+'),
                 breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),  
                 limits=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
  theme(axis.text.x =  element_text(angle=45,size=16,vjust=0.8)) +
  scale_fill_discrete(name="Education\nLevel",
                labels=c('1st','2nd','5th','7th','9th','10th','11th','12th','HIGH SCHOOL','SOME COLLEGE','ASSOCIATES VOCATIONAL','ASSOCIATES DEGREE','BACHELORS DEGREE','MASTERS DEGREE','PROFESSIONAL DEGREE','DOCTORATE DEGREE'),
                breaks=c(31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46),  
                limits=c(31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46)) +
  labs(x="Reported Income", y=NULL) +
  scale_y_continuous(breaks=NULL, labels=NULL)
###################################################################################
#This tells me that people who only graduated the 10th grade lie a lot.           #
#....or I have gotten the short end of the stick/inheritance is rempant in the NE.#              
#                                                                                 #
#Subnote; It seems like educated people hate filling out forms.                   #
###################################################################################