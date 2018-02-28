library(dplyr)
library(stringr)
library(ggplot2)

data <- read.csv("wazapdata.csv", encoding="UTF-8", sep=";")

#is needed to later distinguish between authors
nameA="Adrian"

data=data%>%mutate(
  #convert DAte to the date format.
        Date=as.Date(Date, "%d.%m.%y"),
        year = format(Date,format="%y"),
        hour =  as.integer(substring(Time,1,2))
        #I filtered for two year, 2016/17
        )%>%filter(year=='17'|year=='16')




#Writing during the day,by the hour
ggplot(data,aes(x=hour))+
  geom_histogram(fill="brown",binwidth=1,alpha=0.9)+
  labs(title="Numbers of Messages by Hour", subtitle="Total of two Years",
       y="Number Messages", x="Hour")



#new dataframe for number of words/messages per person

wdata=data%>%mutate(
  #is later need for summarizing per month
        nmessage=1,
  #Counts number of words. (includes emojis as words) Space is the sperator
        nwords=str_count(data$Message, "\\S+"),
  #create binary variable for author
        AorB=ifelse(Author==nameA,1,0),
  #number of words/messages A
        Awords=AorB*nwords,
        Amessages=AorB*nmessage,
  #number of words/messages B
        Bwords=(AorB-1)*nwords,
        Bmessages=(AorB-1)*nmessage,
  #create month-variable with year
        month = format(Date,format="%Y.%m")
        )%>%
  #summarise by month
        group_by(month)%>%
        summarise(
          nwords = sum(nwords),
          nmessages=sum(nmessage),
          Awords=sum(Awords),
          Amessages=sum(Amessages),
          Bwords=sum(Bwords),
          Bmessages=sum(Bmessages),
          AorB=sum(AorB)
          )
    
 #words by day   
sum(wdata$nwords)/2/365
#messages by day
sum(wdata$nmessages)/2/365


ggplot(wdata,aes(x=month))+
   geom_bar(aes(y=nwords),fill="darkgreen", color="darkgreen",stat="identity")+
   geom_bar(aes(y=nmessages),fill="orange",stat="identity",width = 0.9)+
   theme(axis.text.x = element_text(angle = 90, vjust=0.5))+
  labs(title="Messages and words sent per month", 
       subtitle=paste("Average per day:",round(sum(wdata$nwords)/2/365),"Words and",round(sum(wdata$nmessages)/2/365),"Messages"
   ),
       y="Number of Messages/ Words", x="" )


sum(wdata$Awords)/sum(wdata$nwords)*100

ggplot(wdata,aes(x=month))+
  geom_bar(aes(y=Awords),fill="blue",stat="identity")+
  geom_bar(aes(y=Bwords),fill="red",stat="identity")+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),
        legend.position = "top")+
  labs(title="Number of written words", 
       subtitle=paste0("Share of Words written by A: ",round(sum(wdata$Awords)/sum(wdata$nwords)*100),"%"),
     y=" Written words", x="")+
  geom_text(inherit.aes=F,col="blue",x=1,y=2000, size=8,label="A")+
  geom_text(inherit.aes=F,col="red",x=1,y=-2000, size=8,label="B")

ggplot(wdata,aes(x=month))+
  geom_bar(aes(y=Amessages),fill="blue",stat="identity")+
  geom_bar(aes(y=Bmessages),fill="red",stat="identity")+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5))+
  labs(title="Number of sent messages", 
       y="Sent Messages",x="",
       subtitle=paste0("Share of Messages written by A: ",round(sum(wdata$Amessages)/sum(wdata$nmessages)*100),"%"))+
  geom_text(inherit.aes=F,col="blue",x=1,y=400, size=8,label="A")+
  geom_text(inherit.aes=F,col="red",x=1,y=-400, size=8,label="B")


ggplot(wdata,aes(x=month))+
  geom_bar(aes(y=Bwords/Bmessages),fill="red",stat="identity")+
  geom_bar(aes(y=Awords/Amessages),fill="blue",stat="identity")+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5))+
  labs(title="Average words written per message", 
       subtitle=paste("Average A:",round(sum(wdata$Awords)/sum(wdata$Amessages),1) , "- Average B: ",round(sum(wdata$Bwords)/sum(wdata$Bmessages),1)),
       y="Words per Message",x="")+
  geom_text(inherit.aes=F,col="blue",x=1,y=7, size=8,label="A")+
  geom_text(inherit.aes=F,col="red",x=3,y=7, size=8,label="B")

#Wordanalysis 
#every word will be put seperatly into a list. seperator are are spaces
words <- str_match_all( data$Message, "\\S+" )
#unlist the everything an make it lowercase
words=unlist(words)%>%tolower()

#make a table of the wordlist for freqency and make it a dataframe
wordfreq=data.frame(table(words))
#sort dataframe
wordfreq=wordfreq[order(wordfreq$Freq,decreasing=TRUE),]
head(wordfreq)
wordfreq[1:6,]%>%ggplot(aes(words,Freq))+
  geom_bar(stat="identity",fill="green")#your top5 most used words. It's probably "I"
#You probably should filter this words to have a usefull result. There are some wordfilters avaible onlne.
#I didn't do it because I always write in a german dialect with my friend.


#found an emoji-table on github https://github.com/today-is-a-good-day/emojis/blob/master/emojis.csv
emoji_table <- read.csv("emojis.csv", sep=";")%>%
  select("unicode","EN","utf8")

#filter all the emojis out. If you used different ecoding all of the following wont work.
emoji=wordfreq%>%filter(substring(words,1,1)==c("<")&substring(words,4,4)==c(">"))

#a conversion of this <f0><u+009f><u+0098><u+008a> to this: <f0><9f><98><8a> (which is used in the emoji table)
#found the function here and changed it https://stackoverflow.com/questions/41541138/translation-and-mapping-of-emoticons-encoded-as-utf-8-code-in-text
convert <- function(x) {
  loc <- gregexpr(">", x)[[1]]-2
  t=paste0(substring(x, loc, loc + 1),collapse = '><')
  paste0("<",t,">")
}

#convert the the top 10 the emojis. this is a lazy solution. doesn't work for all of them. I don't know why
for (i in 1:10){
  w=convert(emoji$words[i])
  emoji[i,3]=emoji_table[grep(w, emoji_table$utf8),1]
}
#show it. couldn't figure out after a few hours how to properly display the emojis in a chart or smth. Should be easier on Mac or Linux
emoji[1:10,]%>%select(Freq,V3)


