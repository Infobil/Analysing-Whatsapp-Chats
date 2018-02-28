#If you export your Whatsappconversation, you get a TXT-file, which isn't to usefull. 
#This code should help putting it into form. This doesn't work for groupchats
#Depending on your language, you probably need to change a lot. or
#Encoding is chosen because the language is german (ü,ä,ö).
whatsapp = readLines('C:/Users/adris/Google Drive/datenspielerein/whatsapp analyse/whatsapp.txt',encoding="UTF-8")

#The names how they appear in the chat
nameA="Name1"
nameB="Name2"

#Sometimes a text is longer then just one line. I save it in extralines to remove them later
extralines=c()
#Create a matrix to fill in stuff.
sorted=matrix(nrow = length(whatsapp), ncol = 4)

#check any line of whatsapp-data
for(i in 1:length(whatsapp)) {
  
#Check if it's one of the extralines.
#If Dates aren't in this format: '08.04.15, 00:09' - Substring-numbers need to be changed.
#the condition checks for the first dot and the comma
    if(substring(whatsapp[i],9,10)==", "&substring(whatsapp[i],3,3)=="."){
      
      #fill date, time 
      sorted[i,1]=substr(whatsapp[i],0,8)
      sorted[i,2]=substr(whatsapp[i],11,15)

      #different length of names. just checks for the first two letters
      if(substr(whatsapp[i],19,20)==substr(nameA,1,2)){
         sorted[i,3]=nameA
         #Adds everything after the Name to the Message colum
         sorted[i,4]=substring(whatsapp[i],19+nchar(nameA)+1)
      }
      else{
        sorted[i,3]=nameB
        #Adds everything after the Name to the Message colum
                sorted[i,4]=substring(whatsapp[i],19+nchar(nameB)+1)
        }
      
    }
  
 # save them to "extralines" and add the content to the messagefield of the line before
  else{
  extralines=c(extralines,i)
  sorted[i-1,4]=paste(sorted[i-1,4],whatsapp[i],sep=" ")
  }
}

#create dataframe, but without the extralines
data=data.frame(sorted=sorted[-extralines,]) 
colnames(data)=c("Date","Time","Author","Message")

write.csv2(data,file="wazapdata.csv",fileEncoding = "UTF-8")
