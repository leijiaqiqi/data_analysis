library(data.table)
library(tidyverse)
library(dplyr)

#kidpan_data<-import("/media/ext1/Data/UNOS_2019-06/SAS Dataset/Kidney_Pancreaskidpan_data.RData")
#make a copy to protect original file
copykd <- copy(kidpan_data)
# change the type to be date
copykd$TX_DATE <- as.Date(copykd$TX_DATE, '%Y-%m-%d' )
copykd$INIT_DATE<- as.Date(copykd$INIT_DATE, '%Y-%m-%d' )
copykd$ABO1<-factor(with(copykd, ifelse(ABO %in% c('A','A1', 'A2'), 'A',
                                    ifelse(ABO %in% c('A1B','A2B','AB'), 'AB',
                                           ifelse(ABO=="", 'U', ABO))),levels=c('O','A','B','AB','U')))
##########################################
##########################################
#count waitlist and first listing, not include ABO
##########################################
##########################################


#select the patients on the wait list (include all initial date between 2015-2019)
copykd<-copykd[(copykd$DATA_WAITLIST=='Y') & (copykd$TX_DATE>as.Date('2019-01-01')| is.na(copykd$TX_DATE))
               & (copykd$INIT_DATE<as.Date('2019-01-01') & copykd$INIT_DATE>=as.Date('2015-01-01')),]

#get the set of all CTR codes
CTR_code=unique(copykd$LISTING_CTR_CODE)

##########################################
#count waitlist
##########################################


#group by the LISTING_CTR_CODE, year and month
count_WL =  copykd %>% group_by(LISTING_CTR_CODE,year(INIT_DATE),month(INIT_DATE)) %>% summarise(count = n())

names(count_WL)<-c("LISTING_CTR_CODE","year","month","count")


##########################################
#draw barchart for each CTR, and using the year and month as the x axis


#save barcharts in a pdf file
pdf(file= "count_WL_CTR.pdf" )
#each page contains 4 charts
par( mfrow= c(2,2) )

#for each CTR draw a plot
for (c in CTR_code){
  temp <- count_WL %>% filter(LISTING_CTR_CODE==c)
  temp$date <- paste(temp$year, temp$month, sep="_")
  #if the year and month is missing, add the corresponding count as 0
  for (y in 2015:2019){
    for (m in 1:12){
      date = paste(y,m,sep="_")
      if( ! date %in% temp$date){
        addZeroCount = data.frame(c,y,m,0,date)
        names(addZeroCount) = names(temp)
        temp=rbind(temp, addZeroCount)
      }
    }
  }
  #sorted by the date
  temp=temp[order(temp$year,temp$month), ]
  #plot the bar chart
  barplot(temp$count,names.arg = temp$date, xlab="year_month",ylab="number of waiting patients", main = c, ylim=c(0,140))
}
#close the pdf
dev.off()



##########################################
#count first listing
##########################################

#find the first list of the patients
firstList <- copykd %>% group_by(PT_CODE) %>% summarize(INIT_DATE=min(INIT_DATE),WL_ID_CODE=first(WL_ID_CODE), LISTING_CTR_CODE=first(LISTING_CTR_CODE), NUM_LISTINGS=n())


#rename first list
names(firstList)
names(firstList)<-c("PT_CODE","FIRST_INIT_DATE","WL_ID_CODE","FIRST_LISTING_CTR_CODE","TOTAL_NUM_LISTINGS")

#add 3 columns: initial data, first_listing_CTR, total num listing
copykd_firstList<-copykd %>% left_join(firstList %>% select(-c(WL_ID_CODE)), by=c("PT_CODE"))

#whether is first listing or not
copykd_firstList$firstListing<-ifelse(copykd_firstList$FIRST_LISTING_CTR_CODE==copykd_firstList$LISTING_CTR_CODE & copykd_firstList$FIRST_INIT_DATE==copykd_firstList$INIT_DATE,1,0)


#select the first listing data
copykd_IsfirstList<-copykd_firstList %>% filter(firstListing==1)

#group the first listing data by CTR, year and month
copykd_IsfirstList_count =  copykd_IsfirstList %>% group_by(LISTING_CTR_CODE,year(INIT_DATE),month(INIT_DATE)) %>% summarise(count = n())
names(copykd_IsfirstList_count)<-c("LISTING_CTR_CODE","year","month","count")

##########################################
#draw barchart for each CTR, and using the year and month as the x axis


#save barcharts in a pdf file
pdf(file= "copykd_IsfirstList_CTR.pdf" )
#each page contains 4 charts
par( mfrow= c(2,2) )

#for each CTR draw a plot
for (c in CTR_code){
  temp <- copykd_IsfirstList_count %>% filter(LISTING_CTR_CODE==c)
  temp$date <- paste(temp$year, temp$month, sep="_")
  #if the year and month is missing, add the corresponding count as 0
  for (y in 2015:2019){
    for (m in 1:12){
      date = paste(y,m,sep="_")
      if( ! date %in% temp$date){
        addZeroCount = data.frame(c,y,m,0,date)
        names(addZeroCount) = names(temp)
        temp=rbind(temp, addZeroCount)
      }
    }
  }
  #sorted by the date
  temp=temp[order(temp$year,temp$month), ]
  #plot the bar chart
  barplot(temp$count,names.arg = temp$date, xlab="year_month",ylab="number of waiting patients", main = c, ylim=c(0,140))
}
#close the pdf
dev.off()


#export the dataframe
fwrite(count_WL, "count_WL.csv")# count per year month CTR waitlist
fwrite(copykd_IsfirstList, "IsfirstList.csv") #list of  first listing patients
fwrite(copykd_IsfirstList_count, "IsfirstList_count.csv") # count per year month CTR first listing
fwrite(copykd_firstList, "kidpan_withFirstList.csv") #entire table with whether the patient is first listing




##########################################
##########################################
#count waitlist and first listing, including ABO
##########################################
##########################################

#select the patients on the wait list (include all initial date between 2015-2019)
copykd<-copykd[(copykd$DATA_WAITLIST=='Y') & (copykd$TX_DATE>as.Date('2019-01-01')| is.na(copykd$TX_DATE))
               & (copykd$INIT_DATE<as.Date('2019-01-01') & copykd$INIT_DATE>=as.Date('2015-01-01')),]


##########################################
#count waitlist
##########################################


#group by the LISTING_CTR_CODE, year and month
count_WL_ABO =  copykd %>% group_by(LISTING_CTR_CODE,year(INIT_DATE),month(INIT_DATE),ABO1) %>% summarise(count = n())

names(count_WL_ABO)<-c("LISTING_CTR_CODE","year","month","ABO","count")


#get the set of all CTR codes and ABO
CTR_code=unique(copykd$LISTING_CTR_CODE)
ABO_list=unique(count_WL_ABO$ABO)
##########################################
#draw barchart for each CTR, and using the year and month as the x axis


#save barcharts in a pdf file
pdf(file= "count_WL_CTR_ABO.pdf" )
#each page contains 4 charts
par( mfrow= c(2,2) )

#for each CTR and ABO draw a plot
for (c in CTR_code){
  temp1 <- count_WL_ABO %>% filter(LISTING_CTR_CODE==c)
  for (a in ABO_list){
    temp <- temp1 %>% filter(ABO==a)
    temp$date <- paste(temp$year, temp$month, sep="_")
  #if the year and month is missing, add the corresponding count as 0
    for (y in 2015:2019){
      for (m in 1:12){
        date = paste(y,m,sep="_")
        if( ! date %in% temp$date){
          addZeroCount = data.frame(c,y,m,a,0,date)
          names(addZeroCount) = names(temp)
          temp=rbind(temp, addZeroCount)
        }
      }
    }
  #sorted by the date
    temp=temp[order(temp$year,temp$month), ]
  #plot the bar chart
    barplot(temp$count,names.arg = temp$date, xlab="year_month",ylab="number of waiting patients", main = paste(c,a,sep="_"), ylim=c(0,90))
  }
}
#close the pdf
dev.off()



##########################################
#count first listing
##########################################

#find the first list of the patients
firstList <- copykd %>% group_by(PT_CODE) %>% summarize(INIT_DATE=min(INIT_DATE),WL_ID_CODE=first(WL_ID_CODE), LISTING_CTR_CODE=first(LISTING_CTR_CODE), NUM_LISTINGS=n())


#rename first list
names(firstList)
names(firstList)<-c("PT_CODE","FIRST_INIT_DATE","WL_ID_CODE","FIRST_LISTING_CTR_CODE","TOTAL_NUM_LISTINGS")

#add 3 columns: initial data, first_listing_CTR, total num listing
copykd_firstList<-copykd %>% left_join(firstList %>% select(-c(WL_ID_CODE)), by=c("PT_CODE"))

#whether is first listing or not
copykd_firstList$firstListing<-ifelse(copykd_firstList$FIRST_LISTING_CTR_CODE==copykd_firstList$LISTING_CTR_CODE & copykd_firstList$FIRST_INIT_DATE==copykd_firstList$INIT_DATE,1,0)


#select the first listing data
copykd_IsfirstList<-copykd_firstList %>% filter(firstListing==1)

#group the first listing data by CTR, year and month
copykd_IsfirstList_count_ABO =  copykd_IsfirstList %>% group_by(LISTING_CTR_CODE,year(INIT_DATE),month(INIT_DATE),ABO) %>% summarise(count = n())
names(copykd_IsfirstList_count_ABO)<-c("LISTING_CTR_CODE","year","month","ABO","count")

##########################################
#draw barchart for each CTR, and using the year and month as the x axis


#save barcharts in a pdf file
pdf(file= "copykd_IsfirstList_CTR_ABO.pdf" )
#each page contains 4 charts
par( mfrow= c(2,2) )

#for each CTR draw a plot
for (c in CTR_code){
  temp1 <- count_WL_ABO %>% filter(LISTING_CTR_CODE==c)
  for (a in ABO_list){
    temp <- temp1 %>% filter(ABO==a)
    temp$date <- paste(temp$year, temp$month, sep="_")
    #if the year and month is missing, add the corresponding count as 0
    for (y in 2015:2019){
      for (m in 1:12){
        date = paste(y,m,sep="_")
        if( ! date %in% temp$date){
          addZeroCount = data.frame(c,y,m,a,0,date)
          names(addZeroCount) = names(temp)
          temp=rbind(temp, addZeroCount)
        }
      }
    }
    #sorted by the date
    temp=temp[order(temp$year,temp$month), ]
    #plot the bar chart
    barplot(temp$count,names.arg = temp$date, xlab="year_month",ylab="number of waiting patients", main = paste(c,a,sep="_"), ylim=c(0,90))
  }
}
#close the pdf
dev.off()


#export the dataframe
fwrite(count_WL_ABO, "count_WL_ABO.csv")# count per year month CTR waitlist
fwrite(copykd_IsfirstList_count_ABO, "IsfirstList_count_ABO.csv") # count per year month CTR first listing


##########################################
#count waitlist patients for each weekday
##########################################

#group by weekdays
count_WL_weekdays =  copykd %>% group_by(LISTING_CTR_CODE,year(INIT_DATE),month(INIT_DATE),weekdays(INIT_DATE,abbreviate = TRUE),ABO1) %>% summarise(count = n())

names(count_WL_weekdays)=c("LISTING_CTR_CODE","year","month","weekday","ABO","count")

#export data
fwrite(count_WL_weekdays, "count_WL_weekdays.csv") #mean, max, min KDPI of transplanted patients

##########################################
##########################################
#transplanted patients
##########################################
##########################################

##########################################
#count transplanted patients
##########################################

#select the transplanted patients (include all initial date between 2015-2019)
copykd_transplant<-copykd[(copykd$DATA_TRANSPLANT=='Y') & (copykd$INIT_DATE<as.Date('2019-01-01') & copykd$INIT_DATE>=as.Date('2015-01-01')),]

#group by the CTR_CODE (transplant center), year and month, and ABO
count_transplant =  copykd_transplant %>% group_by(CTR_CODE,year(INIT_DATE),month(INIT_DATE),ABO1) %>% summarise(count = n())

names(count_transplant)<-c("CTR_CODE","year","month","ABO","count")
CTR_code=unique(count_transplant$CTR_CODE)
ABO_list=unique(count_transplant$ABO)
##########################################
#draw barchart for each CTR, and using the year and month as the x axis


#save barcharts in a pdf file
pdf(file= "count_transplant.pdf" )
#each page contains 4 charts
par( mfrow= c(2,2) )

#for each CTR draw a plot
for (c in CTR_code){
  temp1 <- count_transplant %>% filter(CTR_CODE==c)
  for (a in ABO_list){
    temp <- temp1 %>% filter(ABO==a)
    temp$date <- paste(temp$year, temp$month, sep="_")
    #if the year and month is missing, add the corresponding count as 0
    for (y in 2015:2019){
      for (m in 1:12){
        date = paste(y,m,sep="_")
        if( ! date %in% temp$date){
          addZeroCount = data.frame(c,y,m,a,0,date)
          names(addZeroCount) = names(temp)
          temp=rbind(temp, addZeroCount)
        }
      }
    }
    #sorted by the date
    temp=temp[order(temp$year,temp$month), ]
    #plot the bar chart
    barplot(temp$count,names.arg = temp$date, xlab="year_month",ylab="number of transplant patients", main = paste(c,a,sep="_"), ylim=c(0,10))
  }
}
#close the pdf
dev.off()

##########################################
#KDPI of transplanted patients
##########################################

#remove all patients if the KDPI is N/A
KDPI_transplant<-copykd_transplant[!is.na(copykd_transplant$KDPI) ,]


#group by year, month and ABO, calculate the mean, min and max KDPI
KDPI_transplant =  KDPI_transplant %>% group_by(CTR_CODE,year(INIT_DATE),month(INIT_DATE),ABO1) %>% summarise(mean_KDPI=mean(KDPI),median_KDPI=median(KDPI),max_KDPI=max(KDPI),min_KDPI=min(KDPI))

names(KDPI_transplant)<-c("CTR_CODE","year","month","ABO","mean_KDPI","median_KDPI","max_KDPI","min_KDPI")

##########################################
#draw barchart for each CTR, and using the year and month as the x axis


#save barcharts in a pdf file
pdf(file= "meanKDPI_transplant.pdf" )
#each page contains 4 charts
par( mfrow= c(2,2) )

#for each CTR draw a plot
for (c in CTR_code){
  temp1 <- KDPI_transplant %>% filter(CTR_CODE==c)
  for (a in ABO_list){
    temp <- temp1 %>% filter(ABO==a)
    temp$date <- paste(temp$year, temp$month, sep="_")
    #if the year and month is missing, add the corresponding count as 0
    for (y in 2015:2019){
      for (m in 1:12){
        date = paste(y,m,sep="_")
        if( ! date %in% temp$date){
          addZeroCount = data.frame(c,y,m,a,0,0,0,0,date)
          names(addZeroCount) = names(temp)
          temp=rbind(temp, addZeroCount)
        }
      }
    }
    #sorted by the date
    temp=temp[order(temp$year,temp$month), ]
    #plot the bar chart
    barplot(temp$mean_KDPI,names.arg = temp$date, xlab="year_month",ylab="mean KDPI of transplant patients", main = paste(c,a,sep="_"), ylim=c(0,1))
  }
}
#close the pdf
dev.off()


#export data
fwrite(count_transplant, "count_transplant.csv") #number of transplanted patients
fwrite(KDPI_transplant, "KDPI_transplant.csv") #mean, median,max, min KDPI of transplanted patients

