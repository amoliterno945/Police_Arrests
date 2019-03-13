DataPath<-"C:/Users/Anthony/Desktop/Storage 2017July/desktop/U Chicago stuff/Data Mining/Project"
dat<-read.csv(paste(DataPath,'terry-stops.csv',sep = '/'), header=TRUE)

#This is for the offenses ranking
crimes<-c(as.character(unique(dat[,15])),as.character(unique(dat[,16]))) #consolidate all crimes to one list
dat2<-read.csv(paste(DataPath,'crimes.csv',sep = '/'), header=TRUE) #save CSV file so I can manually look through and assign urgency and severity values to incidents
dat2<-cbind(dat2[,1],dat2[,2],crimes) #compile neatly







#Clean the data to ordinal factors and binary variables
Age.factors<-as.data.frame(matrix(NA,ncol=2,nrow=7)) #create blank matrix
colnames(Age.factors)<-c("Age factor", "Age range") #made headers
Age.factors[,2]<-unique(dat[,1])[c(4,7,3,2,1,5,6)] #condensed dat's info to a list of unique values
Age.factors[,1]<-as.factor(c(1,2,3,4,5,6,7)) #Assignment of values to the above unique values
(Age.factors)

Resolution.factors<-as.data.frame(matrix(NA,ncol=2,nrow=6))
colnames(Resolution.factors)<-c("Resolution factor", "Resolution data")
Resolution.factors[,2]<-unique(dat[,5])[c(1,3,2,6,4,5)]
Resolution.factors[,1]<-as.factor(c(1,2,3,4,5,6))
(Resolution.factors)

Weapon.factors<-as.data.frame(matrix(NA,ncol=2,nrow=12))
colnames(Weapon.factors)<-c("Weapon factor", "Weapon data")
Weapon.factors[,2]<-unique(dat[,6])[c(1,5,11,4,6,8,2,3,7,9,10,12)]
Weapon.factors[,1]<-as.factor(c(1,1,2,2,2,2,3,4,5,5,5,5)) #I grouped some similar weapon types into the same "grade" (factor)
(Weapon.factors)




#Officer age range (fixed age, using mid-point of all incident dates)
sort(dat[,"Reported.Date"])[1];sort(dat[,"Reported.Date"][length(dat[,1])]) #observe beginning and end date of entire dataset. (note: they are factors)
mydates<-as.Date(c("2015-03-15", "2017-03-29")) #Convert these end dates to a vector in date format
days.range<-mydates[2]-mydates[1] #find range of dates in terms of days
midpoint.date<-mydates[1]+days.range/2 #Create midpoint date
#We do not have the birthdate of the officers (only birth year), therefore, since the midpoint date (i.e. 3/21/2016) is earlier than halfway through the year (i.e. 6/30/2016), we will round down and apply "2016" as the end date calculation
OfficerAge.factors<-as.data.frame(matrix(0,ncol=3,nrow=51))
colnames(OfficerAge.factors)<-c("OfficerAge factor", "Officer age (approx.)","OfficerAge data")
OfficerAge.factors[,3]<-sort(unique(dat[,8]))
OfficerAge.factors[,2]<-2016-as.numeric(as.character(OfficerAge.factors[,3]))
for (i in 1:length(OfficerAge.factors[,2])){ #create loop to factor age based on age range (age ranges match Age.Factors table)
  if (is.na(OfficerAge.factors[i,2])) {
  OfficerAge.factors[i,1]<-1
  } else if (OfficerAge.factors[i,2]%in%1:17) {
  OfficerAge.factors[i,1]<-2
  } else if (OfficerAge.factors[i,2]%in%18:25) {
  OfficerAge.factors[i,1]<-3
  } else if (OfficerAge.factors[i,2]%in%26:35) {
  OfficerAge.factors[i,1]<-4
  } else if (OfficerAge.factors[i,2]%in%36:45) {
  OfficerAge.factors[i,1]<-5
  } else if (OfficerAge.factors[i,2]%in%46:55) {
  OfficerAge.factors[i,1]<-6
  } else if (OfficerAge.factors[i,2]>100) {
  OfficerAge.factors[i,1]<-1
  } else if (OfficerAge.factors[i,2]>55) {
  OfficerAge.factors[i,1]<-7 #The officer born in year 1900 is probably an input error so we bin them into NA (i.e. factor 1)
  }
}
OfficerAge.factors[,1]<-as.factor(OfficerAge.factors[,1]) #convert to factor
(OfficerAge.factors)



Race.factors<- as.data.frame(matrix(NA,ncol=2,nrow=9)) #standardizing officer race and subject's race to one code
Race.factors2<- as.data.frame(matrix(NA,ncol=2,nrow=9))
colnames(Race.factors)<- c("Race factor", "Race data")
colnames(Race.factors2)<- c("Race factor", "Race data")
Race.factors[,2]<- unique(dat[,10])[1:9]
Race.factors2[,2]<- unique(dat[,11])[1:9]
Race.factors<- unique(rbind(Race.factors,Race.factors2))
rownames(Race.factors)<- 1:length(Race.factors[,1])
Race.factors[,1]<- as.factor(c("White","Mix/Other","Black",0,"Native American/Alaskan","Asian","Hispanic/Latino","Hawaiian",0,"Black","Native American/Alaskan",0,"Hispanic/Latino","Mix/Other","Mix/Other")) 

Race.factors <- cbind("Code.Value"=c(1,5,3,8,7,2,4,6,8,3,7,8,4,5,5),Race.factors)
Race.factors$Code.Value <- as.factor(Race.factors$Code.Value)
Race.factors





#Replacing data values with our factors:

#Age
dat[,1]<-as.character(dat[,1])
for (i in 1:length(dat[,1])){
  for (j in 1:length(Age.factors[,2])){
    if (dat[i,1]==Age.factors[j,2]) {dat[i,1]<-Age.factors[j,1]}
  }
}
dat[,1]<-factor(dat[,1],labels=Age.factors[,2])

#Resolution 
dat[,5]<-as.character(dat[,5])
for (i in 1:length(dat[,5])){
  for (j in 1:length(Resolution.factors[,2])){
    if (dat[i,5]==Resolution.factors[j,2]) {dat[i,5]<-Resolution.factors[j,1]}
  }
}
dat[,5]<-factor(dat[,5],labels=Resolution.factors[,2])

#Weapon
dat[,6]<-as.character(dat[,6])
for (i in 1:length(dat[,6])){
  for (j in 1:length(Weapon.factors[,2])){
    if (dat[i,6]==Weapon.factors[j,2]) {dat[i,6]<-Weapon.factors[j,1]}
  }
}
dat$Weapon.Type<-factor(dat$Weapon.Type, labels= c("None","Club-Knuckles-Blackjack", "Lethal Cutting Instrument","Handgun","Rifle/Shotgun/Other"))

#Officer Age
dat[,8]<-as.character(dat[,8])
for (i in 1:length(dat[,8])){
  for (j in 1:length(OfficerAge.factors[,3])){
    if (dat[i,8]==OfficerAge.factors[j,3]) {dat[i,8]<-OfficerAge.factors[j,1]}
  }
}
dat$Officer.YOB<-factor(dat$Officer.YOB, labels=Age.factors[-2,2])

#Race - Officer
dat[,10]<-as.character(dat[,10])
for (i in 1:length(dat[,10])){
  for (j in 1:length(Race.factors[,3])){
    if (dat[i,10]==Race.factors[j,3]) {dat[i,10]<-Race.factors[j,1]}
  }
}

#Race - Subject
dat[,11]<-as.character(dat[,11])
for (i in 1:length(dat[,11])){
  for (j in 1:length(Race.factors[,3])){
    if (dat[i,11]==Race.factors[j,3]) {dat[i,11]<-Race.factors[j,1]}
  }
}

dat[,10]<-as.factor(dat[,10])
dat[,11]<-as.factor(dat[,11])
dat$Officer.Race<-factor(dat$Officer.Race,  labels=c("Caucasian","Asian","African-American","Hispanic/Latino","Mix/Other","Hawaiian","Native American/Alaskan","-")) #Race groupings by Seattle population incidence (descending)
dat$Subject.Perceived.Race <- factor(dat$Subject.Perceived.Race,  labels=c("Caucasian","Asian","African-American","Hispanic/Latino","Mix/Other","Native American/Alaskan","-")) #No subjects were perceived to be Pacific Islander, which was code 6. Therefore, only 7 categories. 

#Offenses
Initial.call.urgency<-vector() #new list that will contain levels of urgency of the crime of variable "Initial Call"
Initial.call.severity<-vector() #new list that will contain levels of severity of the crime of variable "Initial Call"

dat[,15]<-as.character(dat[,15])
dat2[,3]<-as.character(dat2[,3])
for (i in 1:length(dat[,15])){
  for (j in 1:length(dat2[,3])){
    if (dat[i,15]==dat2[j,3]) {Initial.call.urgency<-c(Initial.call.urgency,dat2[j,1]);Initial.call.severity<-c(Initial.call.severity,dat2[j,2]);break}
  }
}

Initial.call.urgency<-as.factor(Initial.call.urgency)
Initial.call.severity<-as.factor(Initial.call.severity)




Final.call.urgency<-vector()
Final.call.severity<-vector()

dat[,15]<-as.character(dat[,15])
dat2[,3]<-as.character(dat2[,3])
for (i in 1:length(dat[,15])){
  for (j in 1:length(dat2[,3])){
    if (dat[i,15]==dat2[j,3]) {Final.call.urgency<-c(Final.call.urgency,dat2[j,1]);Final.call.severity<-c(Final.call.severity,dat2[j,2]);break}
  }
}

Final.call.urgency<-as.factor(Final.call.urgency)
Final.call.severity<-as.factor(Final.call.severity)


#FINISHED RESULT
dat<-cbind(Initial.call.urgency,Initial.call.severity,Final.call.urgency,Final.call.severity,dat)

#Splitting into train and test
set.seed(123456)
mixed<-sample(1:26042,26042,replace=FALSE)
train<-data.frame(dat[mixed[1:18229],])
test<-data.frame(dat[mixed[18230:26042],])









