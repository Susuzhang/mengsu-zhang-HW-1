datjss <- read.csv("~/Desktop/dat/datjss.csv")
datsss <- read.csv("~/Desktop/dat/datsss.csv")
datstu <- read.csv("~/Desktop/dat/datstu.csv")

#exercise 1

length(unique(datstu[,1]))
#  the number of students is 340823
y<-as.vector(as.matrix(datstu[,11:16]))
length(unique(y[complete.cases(y)]))
# The number of programs is 33 according to the result

x=as.vector(as.matrix(datstu[,5:10]))
length(unique(x[complete.cases(x)]))
#The number of schools is 640

x<-as.matrix(x)
y<-as.matrix(y)
code.program<-matrix(cbind(x,y),nrow=340823*6,ncol=2)
# The original combination of 6 school choices and programs.

z<-code.program[complete.cases(code.program),]
codeprogram<-as.matrix(z[order(z[,1]),])
unique(codeprogram)
codeprogram<-codeprogram[!duplicated(codeprogram),]
#Delete the duplicate combinations of schoolcode and program
dim(codeprogram)
#3080 2

n1<-nrow(codeprogram)
for (i in 1:n1){
  
  if(codeprogram[i,2]==""){
  codeprogram[i,]=NA
  }
}
#Subsititute program="" to NA to delete the empty column

codeprogram<-codeprogram[complete.cases(codeprogram),]
dim(codeprogram)
# The number of choices is 2773


misscore<-matrix(datstu[,"score"],nrow=340823,ncol=1)
length(misscore[is.na(misscore)])
# The number of missing test score is 179887

#apply to the same school?
ppnum<-as.matrix(rep(1:6,340823))
ppchoice<-as.vector(as.matrix(datstu[,5:10]))
ppchoice<-as.matrix(ppchoice)
ppsch<-cbind(ppchoice,ppnum)
ppsch<-ppsch[complete.cases(ppsch),]
ppsame<-ppsch[order(ppsch[,1]),]
a<-as.vector(table(ppsame[,1]))
length(a)
# how to present the result?

choices<-as.matrix(datstu[,5:10])
dim(choices[complete.cases(choices),])
# The number of students who apply to less than 6 choices is 323089

#Exercise2
datapply<-cbind(datstu[,5:16],datstu[,2],datstu[,18],datstu[,1],datstu[,17])
dim(datapply)
#[1] 340823     16
#Create a dataframe which includes shoolcodes, programs, scores and admission information.
datapply<-datapply[complete.cases(datapply[,14]),]
dim(datapply)
#[1] 160935     16 
# Omit the rows whose rank places are NA
datapply<-datapply[datapply[,14]!=99,]
#Omit the rows whose rank places are 99
colnames(datapply)=c("schoolcode1","schoolcode2","schoolcode3","schoolcode4","schoolcode5","schoolcode6","choicepgm1","choicepgm2","choicepgm3","choicepgm4","choicepgm5","choicepgm6","score","rank","id","jssdistrict")

rank1<-subset.data.frame(datapply,datapply$rank==1,select = c(schoolcode1,choicepgm1,score,id,jssdistrict))
colnames(rank1)<-c("schoolcode","choicepgm","score","id","jssdistrict")
rank2<-subset.data.frame(datapply,datapply$rank==2,select = c(schoolcode2,choicepgm2,score,id,jssdistrict))
colnames(rank2)<-c("schoolcode","choicepgm","score","id","jssdistrict")
rank3<-subset.data.frame(datapply,datapply$rank==3,select = c(schoolcode3,choicepgm3,score,id,jssdistrict))
colnames(rank3)<-c("schoolcode","choicepgm","score","id","jssdistrict")
rank4<-subset.data.frame(datapply,datapply$rank==4,select = c(schoolcode4,choicepgm4,score,id,jssdistrict))
colnames(rank4)<-c("schoolcode","choicepgm","score","id","jssdistrict")
rank5<-subset.data.frame(datapply,datapply$rank==5,select = c(schoolcode5,choicepgm5,score,id,jssdistrict))
colnames(rank5)<-c("schoolcode","choicepgm","score","id","jssdistrict")
rank6<-subset.data.frame(datapply,datapply$rank==6,select = c(schoolcode6,choicepgm6,score,id,jssdistrict))
colnames(rank6)<-c("schoolcode","choicepgm","score","id","jssdistrict")

rank<-rbind(rank1[1:nrow(rank1),],rank2[1:nrow(rank2),],rank3[1:nrow(rank3),],rank4[1:nrow(rank4),],rank5[1:nrow(rank5),],rank6[1:nrow(rank6),])
# Generate the admission information 
unique(datsss[,3])
datsss_new<-datsss[!duplicated(datsss[,3]),]
datsss_new<-datsss_new[complete.cases(datsss_new),]
# Make the schoolcode in datsss be unique

datadmit<-cbind(as.data.frame(paste(rank[,1],rank[,2])),rank[,3],rank[,4])
#Combine the schoolcode and program to prepare for sorting data

quality<-as.data.frame(tapply(as.numeric(datadmit[,2]),datadmit[,1],mean))
cutoff<-as.data.frame(tapply(as.numeric(datadmit[,2]),datadmit[,1],min))
size<-as.matrix(table(datadmit[,1]))

aa<-as.vector(row.names(size))
schoolcode<-as.data.frame(as.numeric(gsub("\\D","",aa)))
program<-as.data.frame(gsub("\\d","",aa))
size<-as.data.frame(size)
new2<-cbind(schoolcode,program,cutoff,quality,size)
colnames(new2)=c("schoolcode","program","cutoff","quality","size")

datsch<-merge(new2,datsss_new,by="schoolcode",all.x=TRUE)
# Complete exercise 2

#Exercise 3

dis1<-matrix(c(datstu[,1],datstu[,5],as.vector(datstu[,17])),nrow(datstu),3)
dis1_row=c(dis1[,1])
dis1_col=c("id","schoolcode","jssdistrict")
dimnames(dis1)=list(dis1_row,dis1_col)
dis1_jss<-merge(dis1,datjss,by="jssdistrict",all.dis1=TRUE)
dis1<-merge(dis1_jss,datsss,by="schoolcode",all.dis1_jss=TRUE)
dist11<-as.matrix(sqrt((69.172*(dis1$ssslong-dis1$point_x)*cos(dis1$point_y/57.3))^2+(69.172*(dis1$ssslat-dis1$point_y))^2))
# The distance between high schools of choice1 and junior high schools.

dis2<-matrix(c(datstu[,1],datstu[,6],as.vector(datstu[,17])),nrow(datstu),3)
dimnames(dis2)=list(dis1_row,dis1_col)
dis2_jss<-merge(dis2,datjss,by="jssdistrict",all.dis2=TRUE)
dis2<-merge(dis2_jss,datsss,by="schoolcode",all.dis2_jss=TRUE)
dist22<-as.matrix(sqrt((69.172*(dis2$ssslong-dis2$point_x)*cos(dis2$point_y/57.3))^2+(69.172*(dis2$ssslat-dis2$point_y))^2))
# The distance between high schools of choice2 and junior high schools.

dis3<-matrix(c(datstu[,1],datstu[,7],as.vector(datstu[,17])),nrow(datstu),3)
dimnames(dis3)=list(dis1_row,dis1_col)
dis3_jss<-merge(dis3,datjss,by="jssdistrict",all.dis3=TRUE)
dis3<-merge(dis3_jss,datsss,by="schoolcode",all.dis3_jss=TRUE)
dist33<-as.matrix(sqrt((69.172*(dis3$ssslong-dis3$point_x)*cos(dis3$point_y/57.3))^2+(69.172*(dis3$ssslat-dis3$point_y))^2))
# The distance between high schools of choice3 and junior high schools.

dis4<-matrix(c(datstu[,1],datstu[,8],as.vector(datstu[,17])),nrow(datstu),3)
dimnames(dis4)=list(dis1_row,dis1_col)
dis4_jss<-merge(dis4,datjss,by="jssdistrict",all.dis4=TRUE)
dis4<-merge(dis4_jss,datsss,by="schoolcode",all.dis4_jss=TRUE)
dist44<-as.matrix(sqrt((69.172*(dis4$ssslong-dis4$point_x)*cos(dis4$point_y/57.3))^2+(69.172*(dis4$ssslat-dis4$point_y))^2))
# The distance between high schools of choice4 and junior high schools.

dis5<-matrix(c(datstu[,1],datstu[,9],as.vector(datstu[,17])),nrow(datstu),3)
dimnames(dis5)=list(dis1_row,dis1_col)
dis5_jss<-merge(dis5,datjss,by="jssdistrict",all.dis5=TRUE)
dis5<-merge(dis5_jss,datsss,by="schoolcode",all.dis5_jss=TRUE)
dist55<-as.matrix(sqrt((69.172*(dis5$ssslong-dis5$point_x)*cos(dis5$point_y/57.3))^2+(69.172*(dis5$ssslat-dis5$point_y))^2))
# The distance between high schools of choice5 and junior high schools.

dis6<-matrix(c(datstu[,1],datstu[,10],as.vector(datstu[,17])),nrow(datstu),3)
dimnames(dis6)=list(dis1_row,dis1_col)
dis6_jss<-merge(dis6,datjss,by="jssdistrict",all.dis6=TRUE)
dis6<-merge(dis6_jss,datsss,by="schoolcode",all.dis6_jss=TRUE)
dist66<-as.matrix(sqrt((69.172*(dis6$ssslong-dis6$point_x)*cos(dis6$point_y/57.3))^2+(69.172*(dis6$ssslat-dis6$point_y))^2))
# The distance between high schools of choice6 and junior high schools

dist<-as.matrix(cbind(dist11[,1],dist22[,1],dist33[,1],dist44[,1],dist55[,1],dist66[,1]))
# The distance between high schools of choice1-6 and students' junior high schools.

#Exerise4
new3<-matrix(c(aa,new2[,3],new2[,4]),nrow(new2),3)
new3_row=c(1:nrow(new3))
new3_col=c("schpro","cutoff","quality")
dimnames(new3)=list(new3_row,new3_col)
new3[,1]<-as.character(new3[,1])
#Prepare new3 for merge function.

choice1<-matrix(c(paste(datstu[,5],datstu[,11]),datstu[,2]),nrow(datstu),2)
choice1_row=c(1:nrow(choice1))
choice1_col=c("schpro","score")
dimnames(choice1)=list(choice1_row,choice1_col)
choice1[,1]<-as.character(choice1[,1])
#Combine the schoolcode and program to prepare for sorting data
choice11<-merge(choice1,new3,by="schpro",all.x=TRUE)
cutoff_mean1<-mean(as.numeric(choice11[,3]))
cutoff_sd1<-sd(as.numeric(choice11[,3]))
quality_mean1<-mean(as.numeric(choice11[,4]))
quality_sd1<-sd(as.numeric(choice11[,4]))
dist1_mean<-mean(dist11[,1])
dist1_sd<-sd(dist11[,1])

choice2<-matrix(c(paste(datstu[,6],datstu[,12]),datstu[,2]),nrow(datstu),2)
choice2_row=c(1:nrow(choice2))
choice2_col=c("schpro","score")
dimnames(choice2)=list(choice2_row,choice2_col)
choice2[,1]<-as.character(choice2[,1])
#Combine the schoolcode and program to prepare for sorting data
choice22<-merge(choice2,new3,by="schpro",all.x=TRUE)
cutoff_mean2<-mean(as.numeric(choice22[,3]))
cutoff_sd2<-sd(as.numeric(choice22[,3]))
quality_mean2<-mean(as.numeric(choice22[,4]))
quality_sd2<-sd(as.numeric(choice22[,4]))
dist2_mean<-mean(dist22[,1])
dist2_sd<-sd(dist22[,1])

choice3<-matrix(c(paste(datstu[,7],datstu[,13]),datstu[,2]),nrow(datstu),2)
choice2_row=c(1:nrow(choice2))
choice2_col=c("schpro","score")
dimnames(choice3)=list(choice2_row,choice2_col)
choice3[,1]<-as.character(choice3[,1])
#Combine the schoolcode and program to prepare for sorting data
choice33<-merge(choice3,new3,by="schpro",all.x=TRUE)
cutoff_mean3<-mean(as.numeric(choice33[,3]))
cutoff_sd3<-sd(as.numeric(choice33[,3]))
quality_mean3<-mean(as.numeric(choice33[,4]))
quality_sd3<-sd(as.numeric(choice33[,4]))
dist3_mean<-mean(dist33[,1])
dist3_sd<-sd(dist33[,1])

choice4<-matrix(c(paste(datstu[,8],datstu[,14]),datstu[,2]),nrow(datstu),2)
choice2_row=c(1:nrow(choice2))
choice2_col=c("schpro","score")
dimnames(choice4)=list(choice2_row,choice2_col)
choice4[,1]<-as.character(choice4[,1])
#Combine the schoolcode and program to prepare for sorting data
choice44<-merge(choice4,new3,by="schpro",all.x=TRUE)
cutoff_mean4<-mean(as.numeric(choice44[,3]))
cutoff_sd4<-sd(as.numeric(choice44[,3]))
quality_mean4<-mean(as.numeric(choice44[,4]))
quality_sd4<-sd(as.numeric(choice44[,4]))
dist4_mean<-mean(dist44[,1])
dist4_sd<-sd(dist44[,1])

choice5<-matrix(c(paste(datstu[,9],datstu[,15]),datstu[,2]),nrow(datstu),2)
choice2_row=c(1:nrow(choice2))
choice2_col=c("schpro","score")
dimnames(choice5)=list(choice2_row,choice2_col)
choice5[,1]<-as.character(choice5[,1])
#Combine the schoolcode and program to prepare for sorting data
choice55<-merge(choice5,new3,by="schpro",all.x=TRUE)
cutoff_mean5<-mean(as.numeric(choice55[,3]))
cutoff_sd5<-sd(as.numeric(choice55[,3]))
quality_mean5<-mean(as.numeric(choice55[,4]))
quality_sd5<-sd(as.numeric(choice55[,4]))
dist5_mean<-mean(dist55[,1])
dist5_sd<-sd(dist55[,1])


choice6<-matrix(c(paste(datstu[,10],datstu[,16]),datstu[,2]),nrow(datstu),2)
choice2_row=c(1:nrow(choice2))
choice2_col=c("schpro","score")
dimnames(choice6)=list(choice2_row,choice2_col)
choice6[,1]<-as.character(choice6[,1])
#Combine the schoolcode and program to prepare for sorting data
choice66<-merge(choice6,new3,by="schpro",all.x=TRUE)
cutoff_mean6<-mean(as.numeric(choice66[,3]))
cutoff_sd6<-sd(as.numeric(choice66[,3]))
quality_mean6<-mean(as.numeric(choice66[,4]))
quality_sd6<-sd(as.numeric(choice66[,4]))
dist6_mean<-mean(dist66[,1])
dist6_sd<-sd(dist66[,1])


rank_org<-matrix(c(paste(rank[,1],rank[,2]),rank[,3],rank[,5]),nrow(rank),3)
#Generate a matrix of school and program, score and jssdistrict.
rank_org[,1]<-as.character(rank_org[,1])
rank_org_row=c(1:nrow(rank_org))
rank_org_col=c("schpro","score","jssdistrict")
dimnames(rank_org)=list(rank_org_row,rank_org_col)
#Prepare to merge other data frame.
new4<-merge(rank_org,new3,by="schpro",all.x=TRUE)
# Add cutoff and quality infomation
new5<-merge(rank_org,datjss,by="jssdistrict",all.x=TRUE)
# Add jssdistrict infomation.
new6<-matrix(c(rank[,1],new5[,5],new5[,6]),139224,3)
new6_row=c(1:nrow(new6))
new6_col=c("schoolcode","x","y")
dimnames(new6)=list(new6_row,new6_col)
# Prepare to merge with datsss.
new7<-merge(new6,datsss_new,by="schoolcode",all.x=TRUE)
distance<-as.matrix(sqrt((69.172*(as.numeric(new7$ssslong)-as.numeric(new7$x))*cos(as.numeric(new7$y)/57.3))^2+(69.172*(as.numeric(new7$ssslat)-as.numeric(new7$y))^2)))
#Distance of all admitted schools.
exercise4.2<-cbind(new4[,1:5],distance[,1])
#Generate a matrix of score, cutoff, quality and distance
new8<-as.matrix(exercise4.2[order(exercise4.2$score),])
group1<-as.matrix(new8[1:34806,])
# Generate the first quantile.
group2<-as.matrix(new8[34807:69612,])
group3<-as.matrix(new8[69613:104419,])
group4<-as.matrix(new8[104420:139224,])
cutoffmeangroup1<-mean(as.numeric(group1[,4]))
cutoffsdgroup1<-sd(as.numeric(group1[,4]))
cutoffmeangroup2<-mean(as.numeric(group2[,4]))
cutoffsdgroup2<-sd(as.numeric(group2[,4]))
cutoffmeangroup3<-mean(as.numeric(group3[,4]))
cutoffsdgroup3<-sd(as.numeric(group3[,4]))
cutoffmeangroup4<-mean(as.numeric(group4[,4]))
cutoffsdgroup4<-sd(as.numeric(group4[,4]))
qualitymeangroup1<-mean(as.numeric(group1[,5]))
qualitysdgroup1<-sd(as.numeric(group1[,5]))
qualitymeangroup2<-mean(as.numeric(group2[,5]))
qualitysdgroup2<-sd(as.numeric(group2[,5]))
qualitymeangroup3<-mean(as.numeric(group3[,5]))
qualitysdgroup3<-sd(as.numeric(group3[,5]))
qualitymeangroup4<-mean(as.numeric(group4[,5]))
qualitysdgroup4<-sd(as.numeric(group4[,5]))
distancemeangroup1<-mean(as.numeric(group1[,6]))
distancesdgroup1<-sd(as.numeric(group1[,6]))
distancemeangroup2<-mean(as.numeric(group2[,6]))
distancesdgroup2<-sd(as.numeric(group2[,6]))
distancemeangroup3<-mean(as.numeric(group3[,6]))
distancesdgroup3<-sd(as.numeric(group3[,6]))
distancemeangroup4<-mean(as.numeric(group4[,6]))
distancesdgroup4<-sd(as.numeric(group4[,6]))


#Exercise5
adcutoff<-data.frame(c(datsch[,1:3]))
new9<-adcutoff[order(adcutoff[,3]),]


decile1<-data.frame(c(new9[1:230,],group=rep(1,1)))
decile2<-data.frame(c(new9[231:460,],group=rep(2,1)))
decile3<-data.frame(c(new9[461:690,],group=rep(3,1)))
decile4<-data.frame(c(new9[691:920,],group=rep(4,1)))
decile5<-data.frame(c(new9[921:1150,],group=rep(5,1)))
decile6<-data.frame(c(new9[1151:1380,],group=rep(6,1)))
decile7<-data.frame(c(new9[1381:1610,],group=rep(7,1)))
decile8<-data.frame(c(new9[1611:1840,],group=rep(8,1)))
decile9<-data.frame(c(new9[1841:2070,],group=rep(9,1)))
decile10<-data.frame(c(new9[2071:2300,],group=rep(10,1)))
decile<-rbind(decile1,decile2,decile3,decile4,decile5,decile6,decile7,decile8,decile9,decile10)
colnames(decile)<-c("code","pgm","cutoff","group")
decile$code<-gsub(" ","",decile$code)
decile$pgm<-gsub(" ","",decile$pgm)
# Omit "" before the program name
code<-as.data.frame(as.numeric(as.vector(as.matrix(datstu[,5:10]))))
pgm<-as.vector(as.matrix(datstu[,11:16]))
id<-as.vector(rep(1:340823,6))
stuchoice<-data.frame(cbind(code,pgm,id))
stuchoice[stuchoice==""]<-NA
stuchoice<-stuchoice[complete.cases(stuchoice),]#Omit the NA values
colnames(stuchoice)<-c("code","pgm","id")
stuchoice$code<-gsub(" ","",stuchoice$code)
stuchoice$pgm<-gsub(" ","",stuchoice$pgm)
# Omit "" before the program name
stuchoice_new<-merge(stuchoice,decile,by=c("code","pgm"),all.x = TRUE)
# Generate a dataframe with id and group numbers of cutoff.
zz<-data.frame(stuchoice_new[,c(3,5)])
zz<-zz[complete.cases(zz),]
zz1<-unique(zz)
zz2<-as.data.frame(table(zz1[,1]))
#Complete the exercise5.1


adquality<-data.frame(datsch[,c(1,2,4)])
new10<-adquality[order(adcutoff[,3]),]
quan1<-data.frame(c(new10[1:575,],group=rep(1,1)))
quan2<-data.frame(c(new10[576:1150,],group=rep(2,1)))
quan3<-data.frame(c(new10[1151:1752,],group=rep(3,1)))
quan4<-data.frame(c(new10[1753:2300,],group=rep(4,1)))
quan<-rbind(quan1,quan2,quan3,quan4)
colnames(quan)<-c("code","pgm","quality","group")
quan$code<-gsub(" ","",quan$code)
quan$pgm<-gsub(" ","",quan$pgm)
stuchoice$code<-gsub(" ","",stuchoice$code)
stuchoice$pgm<-gsub(" ","",stuchoice$pgm)
quan_new<-merge(stuchoice,quan,by=c("code","pgm"),all.x = TRUE)
# Generate a dataframe with id and group numbers of quality.
yy<-data.frame(quan_new[,c(3,5)])
yy<-yy[complete.cases(yy),]
yy1<-unique(yy)
yy2<-as.data.frame(table(yy1[,1]))
#Complete the exercise5.2
