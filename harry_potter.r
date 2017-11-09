
# i want to write a program for that
titles<-c("Philospher's Stone","Chamber of Secrets","Prisoners of Azkaben","Goblet of Fire","Order of the Phoenix","Half-Blood Prince","Deathly Hallows")
books<-list(philosophers_stone,chamber_of_secrets,prisoner_of_azkaban,goblet_of_fire,order_of_the_phoenix,half_blood_prince,deathly_hallows)

#i am repeating my steps for each chapter in each parts
#so loop should be for 7 outer
# no of chapter inner
series<-tibble()
for(i in 1:7){
  loop_control=length(books[[i]])
  for(j in 1:loop_control){
      words=str_split(books[[i]][j],pattern = " ")
      words=unlist(words)
      looper=length(words)
      tempo=tibble(rep(titles[i],looper),rep(j,looper),words)
      series<-rbind(series,tempo)
      
  }
}
names(series)=c("book","chapter","word")
#okay when i executed the above code it took some time as if it has literally forzen or sth
#i thought there is some error 
#but no it worked

#finally i have created a tibble which contains no of words in harry potter

# now doing the sentiment analysis
series%>%right_join(get_sentiments("nrc"))
series %>%right_join(get_sentiments("nrc")) %>%filter(!is.na(sentiments)) %>%count(sentiments, sort = TRUE)
get_sentiments("nrc")
?right_join


match(p1c1,get_sentiments("nrc"))
######nothing is working so doing my own analysis
# get_sentiments("nrc") returns a tibble

class(get_sentiments("nrc"))
# we have to extract first column
vc=unlist((get_sentiments("nrc")[,1]))
#now i have a vc vector which contains sentiment
#use "match" function to match two vectors
#match is returning the position of 2nd vectors
# rest is NA
# we need the position so that we can extract the sentiment corresponding to that 
#creat a second vector called feeling 

##extracting words from my tibble==series
vc
word_co=unlist(series[,3])
word_co
feel_pos=match(word_co,vc)
#now I have all the sentiments corresponding to each word in the table
class(feel_pos)
#remove na form feel_pos
feel_pos=feel_pos[!is.na(feel_pos)]
feel_pos
#feel_pos contains the position of all those words which has a match in get_sentiments
#now extract the corresponding feeling
feeling=unlist(get_sentiments("nrc")[,2])
feeling[7]
names(feeling)
# assign feeling to harry potter serires
my_feel=feeling[feel_pos]
my_feel
#now i know the type of feeling in the book 
# i will try to convert it into factor
myfeeling=factor(my_feel)
class(myfeeling)
myfeeling
sort(myfeeling)
#the above command tells me the levelso of the facotr
# now i have to write a for loop to count the feelings
a=length(myfeeling)
a
anger="anger"
an=0
anticipation="anticipation"
ant=0
disgust="disgust"
dis=0
fear="fear"
fe=0
joy="joy"
jo=0
negative="negative"
ne=0
positive="positive"
po=0
sadness="sadness"
sa=0
surprise="surprise"
su=0
trust="trust"
tr=0
for(i in 1:a){
  if(myfeeling[i]==anger)
    an=an+1
  else if(myfeeling[i]==anticipation)
    ant=ant+1
  else if(myfeeling[i]==disgust)
    dis=dis+1
  else if(myfeeling[i]==fear)
    fe=fe+1
  else if(myfeeling[i]==joy)
    jo=jo+1
  else if(myfeeling[i]==negative)
    ne=ne+1
  else if(myfeeling[i]==positive)
    po=po+1
  else if(myfeeling[i]==sadness)
    sa=sa+1
  else if(myfeeling[i]==surprise)
    su=su+1
  else if(myfeeling[i]==trust)
    tr=tr+1
  else
    print("nothing")
  
}

vect=c(an,ant,dis,fe,jo,ne,po,sa,su,tr)
vect
sum(vect)
#yes correct assignment has been done
length(myfeeling)
# both the above command are producing the same result
names(vect)=c("Anger","Anticipation","Disgust","Fear","Joy","Negative","Positive","Sadness","Surprise","Trust")
barplot(vect,main = "Harry Potter",xlab = "sentiment",ylab = "scores",col = rainbow(10))
