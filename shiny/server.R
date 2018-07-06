#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
#Libraries used:twitteR,ROAuth,RCurl,httr
library("twitteR")

library("zip")

library("ROAuth")

library("RCurl")

library("httr")

library("plusser")
library(xml2)


#Libraries used: RedditExtractoR

library("RedditExtractoR")
#Initiate message for start of log file

#library(XML)

unescape_html <- function(str){
  xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))
}

library('curl')
has_internet <- function(url1){
  !is.null(curl::nslookup(url1, error = FALSE))
}

cleanText<-function(opfile,createdfiles)
{
  file_name<-opfile
  
  #An output file is created with name of 'previous file (CleanOutput).txt'
  outputfile<-gsub(".txt","(CleanOutput).txt",opfile)
  
  #cat(outputfile,"\n")
  
  #The generated file is opened
  conn<-file(file_name,open="r")
  
  #Store the newline separated data in a list
  outp=readLines(conn)
  
  #Store length of list
  l<-length(outp)
  
  i<-1
  
  mess<-""
  
  #Initiate progress bar
  
  pb<-winProgressBar(title = "Initiating File Cleaning",label = "Cleaning...",min = 0,max=100,initial = 0)
  
  #Looping through all the sentences
  while(i<=l)
  {
    #Update progress bar
    setWinProgressBar(pb,i/l*100)
    
    #Convert to ASCII
    temp<-iconv(outp[i],to="ASCII",sub="")
    
    #Convert HTML encodings
    temp<-unescape_html(temp)
    
    #Store into string 'mess' only if not null, else reject
    if(temp!="")
      mess<-paste0(mess,"\n",temp)
    i<-i+1
  }
  #Close progress bar
  close(pb)
  
  #Generate a list from the string 'mess'
  
  newlist<-unlist(strsplit(mess,"\n",fixed = TRUE))
  
  #Retain only the unique elements
  
  newlist<-unique(newlist)
  
  #Generate the final output list
  
  finallist<-list()
  
  #Set loop counters
  i<-1
  j<-1
  #Set number of spaces
  nspace<-1
  #Stores length of 'newlist'
  l<-length(newlist)
  cat("Length of newlist",l)
  #Inititate a progress bar
  pb2<-winProgressBar(title = "Removing stray newlines",label="Cleaning",min=0,max=100)
  #Looping through the 'newlist'
  while(i<=l)
  {
    #Updating the progress bar
    setWinProgressBar(pb2,value = i/l*100)
    
    text1<-newlist[i]
    
    #Extract all characters from the string
    
    templist<-unlist(strsplit(text1,split = ""))
    
    #Count number of alphanumeric characters
    
    alphanums<-length(grep(pattern = "[:alnum:]",x = templist))
    
    #If there are more than 5 alphanumerics, store otherwise reject
    if(alphanums>5)
    {
      finallist[j]<-text1
      j<-j+1
    }
    #cat("\nNumber of alphanums ",,"\n")
    #cat(text1,"\n")
    i<-i+1
  }
  #Close the progress bar
  
  #Generate a finalized output to store into the clean output file
  close(pb2)
  l<-length(finallist)
  cat(l,"\n")
  i<-1
  finmess<-""
  pb3<-winProgressBar(title = "Editing contents",label="Cleaning",min=0,max=100)
  while(i<=l)
  {
    setWinProgressBar(pb3,value=i/l*100)
    finmess<-paste0(finmess,finallist[i],"\n")
    i<-i+1
  }
  close(pb3)
  cat(finmess,file=outputfile)
  
  #Maintain the list of files created 
  
  if(createdfiles!="")
    
    createdfiles<-paste0(outputfile,"\n",createdfiles)
  
  else
    
    createdfiles<-outputfile

  close(conn)
  
  return(createdfiles)
}

searchFB<-function(inpfile,opfile,createdfiles)
{
  
  #Libraries used:Rfacebook
  
  #This program retrieves and stores posts on Facebook pages mentioning a search string, it also looks for comments in the official page of the product 
  library("Rfacebook")
  
  #Stores the authorisation object required to connect to facebook
  #my_oauth <- fbOAuth(app_id = "753646804806755", app_secret = "0fb262458017c8ab58fe7914c111d85e")
  
  #save(my_oauth,file="authob")
  
  pb <- winProgressBar(title="Searching Facebook.This may take a while...", label="Authenticating...", min=0, max=100, initial=0,width = 400)
  start_time<-Sys.time()
  msg=""
  
  #Input File
  keyword_file<-inpfile
  
  #opening input file in read mode
  conn<-file(keyword_file,open="r")
  
  #read file line after line
  searchstr=readLines(conn)
  
  msg<-paste(msg,"Program starts execution at ",start_time,sep=" ",collapse=" ")
  
  #starting time of authentication
  msg<-paste(msg,"\nAuthentication starts ",Sys.time(),sep=" ",collapse=" ")
  
  #Stores the authorisation object required to connect to facebook
  load(file="authob")
  
  #stores finishing time of authentication
  msg<-paste(msg,"\nAuthentication completed ",Sys.time(),sep=" ",collapse = " ")
  ii<-1
  while(ii<=length(searchstr))
  {
    if(substr(searchstr[ii],1,1)=="#")
    {
      ii<-ii+1
      next()
    }
    
    else
      
   { 
    #file which stores the time_log of the entire search
    Log_file <- paste0(opfile,"/Logs/",searchstr[ii],"Log.txt")
    #stores time for searching 100 pages with name mentioned
    msg<-paste(msg,"\nStarting time for searching pages which have the given search string ",searchstr[ii],Sys.time(),sep=" ",collapse = " ")
    
    #Search all pages that mention 'searchstr', n refers to number of groups returned
    pages<-searchPages(string=searchstr[ii],my_oauth,n=100)
    
    #Removing any special characters from 'searchstr' as it is passed into getPage() which doesn't support special characters 
    searchstrg<-gsub("\\s|\\t|\\n|'|,|\"|","",searchstr[ii])
    
    #Search any pages with same name as 's`earchstrg'
    gpages<-getPage(searchstrg[ii],token = my_oauth,n = 20)
    
    #Save the number of pages returned
    ll<-length(gpages$id)
    
    #stores finishing time for searching those pages with mentioned search string
    msg<-paste(msg,"\nFinishing time for searching pages which have the given search string ",Sys.time(),sep=" ",collapse = " ")
    
    #Filename is created with 'searchstr' followed by " Search.txt"
    filename<-paste0(opfile,searchstr[ii]," Search(Facebook).txt")
    
    #Setting a loop counter to 1
    i<-1
    
    #Saving the length of the number of pages found
    l<-length((pages$id))
    print(l)
    #starting time for going through each page
    msg<-paste(msg,"\nStarting time for going through each page ",Sys.time(),sep=" ",collapse = " ")
    
    #Looping through all the pages found
    msg<-paste(msg,"\nStarting time\t\t Page Id\tNumber of relevant posts\t Finishing Time",sep=" ",collapse = " ")
    while(i<=l)
    {
      lvalue<-paste("Searching for ",searchstr[ii])
      #Update progress bar
      setWinProgressBar(pb,i/(l+ll)*100,label=lvalue)
      #starting time for fetching each page
      msg<-paste(msg,"\n",Sys.time(),"\t",pages$id[i],"\t\t",sep=" ",collapse = " ")
      if(is.na(pages$id[i])==FALSE && is.null(pages$id[i])==FALSE)
      {
        #Fetching pages from their page ids as saved in 'pages'
        gpage<-try(getPage(page=pages$id[i],token=my_oauth,n=100))
        if(inherits(gpage, "try-error")) 
        {
          print("can't use this api")
        } 
        else
        {
          #Store the messages in page posts
          mess<-gpage$message
          
          #Setting all the strings as blank 
          newmess=""
          commentmessage=""
          commentp=""
          mess1=""
          relevantLength=0
          
          pb2<-winProgressBar(title="Removing posts without keywords",label = "Checking for posts with Keywords",min=0,max = 100)
          no_of_mes<-length(gpage$message)
          
          #Going through every message and retrieving only those messages and their comments which have the keywords mentioned in them
          for(y in 1:no_of_mes)
          {
            setWinProgressBar(pb2,y/no_of_mes*100,label = "Checking whether post has keyword")
            #searching for input file
            for (o in 1:length(searchstr))
            {
              #stores message in mess1 if the message has any one keyword mentioned in it
              mess1<-grep(searchstr[o],gpage$message[y],ignore.case=TRUE,value = TRUE)
              
              #Check whether any message had the keyword or not
              if(length(mess1)==0)
              {
              }
              #if message has keyword in it will retrieve the comments
              else
              {
                #Removes any new line from individual post
                mess1=gsub("\n","",mess1)
                #Separate all the messages with two newlines
                newmess<-paste(mess1)
                
                #stores length of relevant messages
                relevantLength=relevantLength + length(mess1)
                
                #Save the message in a file
                cat(iconv(newmess, to="UTF-8",sub=""),file = filename,sep="\n",fill=TRUE,append = TRUE)
                
                #to check if ID is not empty and not na (//Removes callToAPI error)
                if(is.na(gpage$id[y])==FALSE && is.null(gpage$id[y])==FALSE)
                {
                  #Retrieving the current post
                  postcomm<-try(getPost(post = gpage$id[y],token = my_oauth,n = 200))
                  
                  if(inherits(postcomm, "try-error")) 
                  {
                    print("can't use this api")
                  } 
                  else 
                  {
                    #Storing the comment
                    commentp<-postcomm$comments
                    
                    #Storing the comment message
                    commentmessage<-commentp$message
                    commentmessage1=newmess
                    #commentmessage1=paste(commentmessage1,"\n")
                    for(z in 1:length(commentmessage))
                    {
                      if(is.null(commentmessage[z])==FALSE || is.na(commentmessage[z])==FALSE)
                      {
                        comment_without_newLine=gsub("\n","",commentmessage[z])
                        cat(iconv(comment_without_newLine,to="UTF-8",sub=""),file =filename,sep="\n",fill = TRUE,append = TRUE)
                      }
                    }#z loop closes
                    relevantLength = relevantLength + length(commentmessage)
                  }#else closes
                }#if closes
              }#else closes
            }#o loop closes
          }#y loop closes
          close(pb2)
        }#else closes
      }
      #Increment the counter by 1
      i<-i+1
      
      #pasting how many messages and comments retrieved from one page
      msg<-paste(msg,relevantLength,"\t\t",sep=" ",collapse = " ")
      
      #finishing time for fetching each page
      msg<-paste(msg,Sys.time(),sep=" ",collapse = " ")
    }
    
    #We have retrieved all relevant page posts mentioning the keyword
    
    #Now we will search for comments on the official product page of the keyword mentioned
    
    #Setting loop counter to 1
    i<-1
    
    #Looping through all the pages found
    #Saving the filename as a separate one for comments
    
    filenamecomm<-paste(opfile,searchstr[ii]," withoutBlankSpaces(Facebook).txt")
    while(i<=ll)
    {
      #setting relevantLength as 0 for each page
      relevantLength=0
      
      #starting time for fetching each page
      msg<-paste(msg,"\n",Sys.time(),"\t",gpages$id[i],"\t\t",sep=" ",collapse = " ")
      
      #Get the most recent post in the ith page retrieved
      posts<-getPost(post=gpages$id[i],token = my_oauth,n.comments = 50)
      
      #Extract the comments from the posts
      comm<-posts$comments
      
      #Retrieve the comment messages
      commess<-comm$message
      for(z in 1:length(commess))
      {
        if(is.null(commess[z])==FALSE || is.na(commess[z])==FALSE)
        {
          comment_without_newLine=gsub("\n","",commess[z])
          cat(iconv(comment_without_newLine,to="UTF-8",sub=""),file =filenamecomm,sep="\n",fill = TRUE,append = TRUE)
        }
      }
      
      #stores length of comments
      relevantLength= relevantLength + length(commess)
      
      #Increment the counter by 1
      i<-i+1
      
      #pasting how many messages and comments retrieved from one page
      msg<-paste(msg,relevantLength,"\t\t",sep=" ",collapse = " ")
      
      #finishing time for fetching each page
      msg<-paste(msg,Sys.time(),sep=" ",collapse = " ")
    }
    # if(createdfiles!="")
    # {
    #   createdfiles<-paste0(filename,"\n",filenamecomm,"\n",createdfiles)
    # }
    # 
    # else
    #   createdfiles<-paste0(filename,"\n",filenamecomm)
    # 
    #Initiate file cleaning
    createdfiles<-cleanText(filename,createdfiles)
    createdfiles<-cleanText(filenamecomm,createdfiles)
    ii<-ii+1
    }
  }
  
  
  #end time stamp of search
  end_time<-Sys.time()
  
  #calculates total time taken for execution
  total_time<-end_time - start_time
  
  msg<-paste(msg,"\n\n Total time taken is ",total_time,sep=" ",collapse = " ")
  
  #Finally add the entire log to the log_file 
  cat(msg,file=Log_file,append = TRUE)
  close(pb)
  return(createdfiles)
}
searchTW<-function(inpfile,opfile,createdfiles)
  
{
  #starting time at which execution starts
  pb <- winProgressBar(title="Searching Twitter.This may take a while...", label="Authenticating...", min=0, max=100, initial=0,width = 400)
  start_time<-Sys.time()
  msg<-""
  msg<-paste(msg,"Program starts execution at ",start_time,sep=" ",collapse=" ")
  msg<-paste(msg,"\nAuthentication starts ",Sys.time(),sep=" ",collapse=" ")
  
  #Contains the access id for the Twitter App used for accessing twitter
  access_id<-"jGTUM7YciMQu0ErgROG1rksoT"
  #Contains the access secret for the Twitter App used for accessing twitter
  access_secret<-"wKCLYlXi1ofP6We8UdOl3eMixMVR4JAfGynxlQv7lTl0EEZhfK"
  #Contains the token id for the Twitter App used for accessing twitter(used for direct authentication without browser)
  token_id<-"4030445255-weTTuRszPKDQlkUQIK2pjNrKWwGkyBJXFKGNiat"
  #Contains the token secret for the Twitter App used for accessing twitter(used for direct authentication without browser)
  token_secret<-"hJQHzMJgUqhyxbioOyKEwP4iZICdprzv2nFm1H9VhBNrL"
  #Sets up the twitter session for crawling tweets
  setup_twitter_oauth(access_id,access_secret,token_id,token_secret)
  
  #stores finishing time of authentication
  msg<-paste(msg,"\nAuthentication completed ",Sys.time(),sep=" ",collapse = " ")
  
  #keword to be searched are stored in a variable called inpfile
  filename<-inpfile
  conn<-file(filename,open="r")
  linn<-readLines(conn)
  close.connection(conn)
  #file which stores the time_log of the entire search
  Log_file <- paste0(opfile,"/Logs/",linn[1]," TwitterLog.txt")
  
  for(j in 1:length(linn)){
    #searchstr contains the keyword to be searched
    searchstr<-linn[j]
    cat("Searching for ",searchstr,"\n")
    if(substr(searchstr,1,1)=="#")
    {
      #k<-k+1
      next()
    }
    else
    {
      #Filename is created with 'searchstr' followed by " twitterSearch.txt"
      filename<-paste0(opfile,searchstr," TwitterSearch.txt")
      #searchTwitter() function returns tweets based on a keyword 'searchstr'
      
      #stores time for searching all posts with name mentioned
      msg<-paste(msg,"\nStarting time for searching posts which have the search string named: ",linn[j] ,": ", Sys.time(),sep=" ",collapse = " ")
      #Create text to be displayed on progress bar
      lvalue<-paste("Searching for",searchstr)
      #Update progress bar
      setWinProgressBar(pb,j/length(linn)*100,label=lvalue)
      
      #tweets is an object of 'status' type specified by twitteR API
      #tweets <- searchTwitter(searchString = searchstr, n=500,lang = 'en',resultType = "recent")
      tweets <- try(searchTwitter(searchString = searchstr, n=500,lang = 'en'))
      #The returned 'tweets' of type 'status' is converted into a dataframe for easy access.
      
      if(inherits(tweets, "try-error")) 
      {
        print("No results found!")
      }
      
      #The function used is 'twListToDF()'
      
      else
        
      
     { tweetdf<-twListToDF(tweets)
      
      #l is used to store the number of data elements retrieved
      
      #stores finishing time for searching those tweets with mentioned search string
      msg<-paste(msg,"\nFinishing time for searching tweets which have the given search string: ",Sys.time(),sep=" ",collapse = " ")
      
      l<-length(tweetdf$text)
      
      #i is used to store the counter
      
      i<-1
      
      #'tweetmessages' is used to store all tweets in a string
      
      tweetmessages<-""
      relevantLength=0
      #starting time for going through each tweet
      msg<-paste(msg,"\nStarting time for going through each post: ",Sys.time(),sep=" ",collapse = " ")
      
      #Looping through all the tweets found
      msg<-paste(msg,"\nStarting time\t\t Post Id\t\t\tRelevant posts Index\t\t Finishing Time",sep=" ",collapse = " ")
      
      pb2<-winProgressBar(title="Removing retweets",label = "Checking for retweets",min=0,max = 100)
      while(i<=l)
        
      {
        setWinProgressBar(pb2,i/l*100,label = "Checking for retweets")
        #starting time for fetching each post
        msg<-paste(msg,"\n",Sys.time(),"\t",tweetdf$id[i],"\t\t",sep=" ",collapse = " ")
        
        
        #Only keep tweets that are not retweets
        
        if(tweetdf$isRetweet[i]==FALSE)
          
        {
          
          #If it is not a retweet, then store it in 'tweetmessages'
          cleantweet<-gsub("\n","",tweetdf$text[i])
          tweetmessages<-paste0(tweetmessages,cleantweet,"\n")
          relevantLength=relevantLength + length(tweetmessages)
          
        }
        #pasting how many messages retrieved from one post
        msg<-paste(msg,relevantLength,"\t\t",sep=" ",collapse = " ")
        
        #finishing time for fetching each post
        msg<-paste(msg,Sys.time(),sep=" ",collapse = " ")
        
        
        #Incrementing the counter by 1
        
        i<-i+1
        
      }
      close(pb2)
      msg<-paste(msg,"\n\n\n\n")
      #Store the contents of 'tweetmessages' in a file with name 'filename'
      
      cat(iconv(tweetmessages,to ="UTF-8"),file=filename, sep= "\n")
      
      #Initiate cleaning process
      
      createdfiles<-cleanText(filename,createdfiles)
      
      #end time stamp of search
      end_time<-Sys.time()
      
      #calculates total time taken for execution
      total_time<-end_time - start_time
      
      msg<-paste(msg,"\n\n Total time taken is ",total_time,sep=" ",collapse = " ")
      
      #Finally add the entire log to the log_file 
      cat(msg,file=Log_file,append = TRUE)
      }
    }
  }
  close(pb)
  return(createdfiles)
}
#Author: TCS EIS OT Group 1

#Set start time for log file

searchRD<-function(inpfile,opfile,createdfiles)
  
{start_time<-Sys.time()

#Libraries used: RedditExtractoR

library("RedditExtractoR")

#Initiate message for start of log file

msg=""

#Input File

keyword_file<-inpfile

#opening input file in read mode

conn<-file(keyword_file,open="r")

#read file line after line

searchstr=readLines(conn)

#file which stores the time_log of the entire search

k<-1
lvalue<-paste("Searching for",searchstr[k])
pb <- winProgressBar(title="Searching Reddit.This may take a while...", label=lvalue, min=0, max=100, initial=0,width = 700)
while(k<=length(searchstr))
{
  lvalue<-paste("Searching for",searchstr[k])
  if(substr(searchstr[k],1,1)=="#")
  {
    k<-k+1
    next()
  }
  else
  {
    
    Log_file <- paste0(opfile,"/Logs/",searchstr[k]," Log(Reddit).txt")
    
    msg<-paste(msg,"\n",start_time,"Program starts execution for ",searchstr[k],sep=" ",collapse=" ")
    
    #Formats the search term to search in Reddit
    
    term<-gsub(" ","+",searchstr[k])
    
    #Formats a regular expression for filtering the returned results
    
    regular<-gsub(" ","|",searchstr[k])
    
    #Initialize a message variable that will store everything related to the post
    
    messages<-""
    
    #Store the time for search initiation
    
    search_init<-Sys.time()
    
    #Fetch results from Reddit from the given search string
    lvalue<-paste("Searching for",searchstr[k],".Search Started at",search_init,"This will take a while, please wait...")
    
    msg<-paste(msg,"\n",search_init,"Search started for ",searchstr[k])
    
    cat("Searching for ",searchstr[k])
    
    setWinProgressBar(pb, k/(length(searchstr))*100, label=lvalue)
    
    posts<-try(get_reddit(search_terms = term,regex_filter = regular,page_threshold = 1,sort_by = "relevance"))
    
    if(inherits(posts, "try-error")) 
    {
      print("can't get results")
    } 
    else
    {
    
    #Stores the end of search time
    
    search_end<-Sys.time()
    
    msg<-paste(msg,"\n",search_end,"Search completed")
    
    #Creates a file name of the format "keyword Search(Reddit).txt" for storing post text, title and first comment"
    
    filename<-paste0(opfile,searchstr[k]," Search(Reddit).txt")
    
    #Creates a file name of the format "keyword Search Comments(Reddit).txt" for storing comments
    
    #filecommname<-paste0(searchstr[k]," Search Comments(Reddit).txt")
    
    #Stores the number of posts returned
    
    l<-length(posts$id)
    
    msg<-paste(msg,"\nSearch returned ",l," number of item(s)")
    
    #Set loop counter
    
    i<-1
    
    #Saves the first post title
    
    posttitle<-posts$title[1]
    
    #The Reddit API does not differentiate between posts and their comments, so if a post has n comments
    #The same title and text is repeated n times, to remove this redundancy, every time a new title is encountered, it is 
    #saved and the comments are collected for the rest
    pb2<-winProgressBar(title="Processing Text",label = "Removing irrelevant data",min = 0,max=100,initial = 0)
    while(i<=l)
    {
      setWinProgressBar(pb2,value=i/l*100.0)
      #Checks for new post titles
      
      #Storing the post text in a variable for removing spaces
      
      posttext<-posts$post_text[i]
      
      posttext<-gsub("\n","",posttext)
      
      #posttext<-html2txt(posttext)
      
      if(posttitle!=posts$title[i])
      {
        new_post<-Sys.time()
        msg<-paste(msg,"\n",new_post,"New post title found\nThe new post id is ",posts$id[i]," Subreddit is ",posts$subreddit[i])
        #cat("New Title found at ",i,"\n")
        
        #Saving the new post title
        
        posttitle<-posts$title[i]
        
        #Checks for relevance of post title
        
        if(length(grep(regular,posttitle,ignore.case = TRUE))>0)
        {
          msg<-paste(msg,"\nThe new post is relevant")
          
          #Collects the first comment of the post
          
          comments<-posts$comment[i]
          
          posttitle<-gsub("\n","",posttitle)
          
          comments<-gsub("\n","",comments)
          
          #Checks the relevance of first comment
          
          if(length(grep(regular,comments,ignore.case = TRUE))>0)
            #Stores the relevant post title, post text and/or first comment
          {
            msg<-paste(msg,"\nThe first comment is relevant")
            if(posttext!="")
            {
              messages<-paste0(messages,posttitle,"\n",posttext,"\n",comments,"\n")
            }
            else
            {
              messages<-paste0(messages,posttitle,"\n",comments,"\n")
            }
            #cat(iconv(x=posttitle,to = "UTF-8"),"\n",iconv(x=posttext,to = "UTF-8" ),"\n",iconv(x = comments,to = "UTF-8"),file = filename,append = TRUE)
          }
          else
          {
            msg<-paste(msg,"\nThe first comment is not relevant")
            if(posttext!="")
            {
              messages<-paste0(messages,posttitle,"\n",posttext,"\n")
            }
            else
            {
              messages<-paste0(messages,posttitle,"\n")
            }
            #cat(iconv(x=posttitle,to = "UTF-8"),"\n",iconv(x =posttext,to = "UTF-8" ),file = filename,append = TRUE)
          }
        }
      }
      else
      {
        #Saves the comment
        
        new_comment<-Sys.time()
        
        comm<-posts$comment[i]
        
        comm<-gsub("\n","",comm)
        
        msg<-paste(msg,"\n",new_comment,"Comment retrieved from the post",sep="\t",collapse = "\t")
        #Checks for relevance of the comment
        
        if(length(grep(pattern = searchstr[k],comm,ignore.case = TRUE))>0)
        {
          msg<-paste(msg,"\nRelevant comment found for id ",i)
          
          messages<-paste0(messages,comm,"\n")
          #cat("\n",iconv(x=comm,to = "UTF-8"),"\n",file = filecommname,append = TRUE)
        }
        else
        {
          msg<-paste(msg,"\nRelevant comment not found for id ",i)
        }
        
      }
    
      i<-i+1
      }
    close(pb2)
    cat(messages,file = filename)
    # if(createdfiles!="")
    # createdfiles<-paste0(filename,"\n",createdfiles)
    # else
    #   createdfiles<-filename
    cat(msg,file = Log_file)
    #Initiate cleaning process
    createdfiles<-cleanText(filename,createdfiles)
    }
    k<-k+1
  }
}
close(pb)
return(createdfiles)
}
searchGP<-function(inpfile,opfile,createdfiles)
{
  pb <- winProgressBar(title="Searching Google Plus.This may take a while...", label="Authenticating...", min=0, max=100, initial=0,width = 400)
  start_time<-Sys.time()
  msg<-""
  msg<-paste(msg,"Program starts execution at ",start_time,sep=" ",collapse=" ")
  msg<-paste(msg,"\nAuthentication starts ",Sys.time(),sep=" ",collapse=" ")
  #setting up API key
  setAPIkey(apikey = "AIzaSyDR70m9zg9eCFmPp60LG1-pwUEof3StW4s")
  #stores finishing time of authentication
  msg<-paste(msg,"\nAuthentication completed ",Sys.time(),sep=" ",collapse = " ")
  
  #opfile<-"C:\\Users\\HP\\Desktop\\"
  
  # read all the keywords to be searched from the input file InputFile.txt
  filename<-inpfile
  conn<-file(filename,open="r")
  linn<-readLines(conn)
  close.connection(conn)
  
  #file which stores the time_log of the entire search
  Log_file <- paste0(opfile,"/Logs/",linn[1]," GoogleLog.txt")
  for(j in 1:length(linn))
  {
    #searchstr contains the keyword to be searched
    searchstr<-linn[j]
    cat("Searching for ",searchstr,"\n")
    if(substr(searchstr,1,1)=="#")
    {
      #k<-k+1
      next()
    }
    else
    {
      term<-gsub(" ","|",searchstr, ignore.case = TRUE)
      #file to store all the post messages for each keyword
      filename<-paste0(opfile,searchstr, " GoogleSearchMsg.txt")
      
      #stores time for searching all posts with name mentioned
      msg<-paste(msg,"\nStarting time for searching posts which have the search string named: ",linn[j] ,": ", Sys.time(),sep=" ",collapse = " ")
      #Create text to be displayed on progress bar
      lvalue<-paste("Searching for",searchstr)
      #Update progress bar
      setWinProgressBar(pb,j/length(linn)*100,label=lvalue)
      #searching through posts
      df <- searchPost(searchstr, ret = "data.frame", results = 50)
      #stores finishing time for searching those posts with mentioned search string
      msg<-paste(msg,"\nFinishing time for searching posts which have the given search string: ",Sys.time(),sep=" ",collapse = " ")
      l<-length(df$id)
      cat("Number of posts:",l,"\n")
      i<-1
      postmessages<-""
      mess<-""
      relevantLength=0
      #starting time for going through each post
      msg<-paste(msg,"\nStarting time for going through each post: ",Sys.time(),sep=" ",collapse = " ")
      #Looping through all the pages found
      msg<-paste(msg,"\nStarting time\t\t Post Id\t\t\t\tNumber of relevant posts\t\t Finishing Time",sep=" ",collapse = " ")
      pb2<-winProgressBar(title="Removing duplicates",label = "Checking for duplicate posts",min=0,max = 100)
      while(i<=l)
      { 
        setWinProgressBar(pb2,i/l*100,label = "Checking for duplicate posts")
        #starting time for fetching each post
        msg<-paste(msg,"\n",Sys.time(),"\t",df$id[i],"\t\t",sep=" ",collapse = " ")
        #mess<-grep(term,df$msg[i],ignore.case = TRUE,value = TRUE)
        # if(length(grep(searchstr,df$msg[i],ignore.case = TRUE))>0)
        # {
        relevantLength=relevantLength + length(mess)
        if(length(grep(term,df$msg[i],ignore.case = TRUE,value = TRUE)))
        {
          postmessages<-paste(postmessages,df$msg[i],"\n")
          #cat(i,"th post is ",df$msg[i],"\n")
          #cat(postmessages)
          #pasting how many messages retrieved from one post
          msg<-paste(msg,relevantLength,"\t\t",sep=" ",collapse = " ")
          
          #finishing time for fetching each post
          msg<-paste(msg,Sys.time(),sep=" ",collapse = " ")
        }
        i<-i+1
      }
      #cat(postmessages)
      close(pb2)
      msg<-paste(msg,"\n\n\n\n")
      
      #Store the contents of 'postmessages' in a file with name 'filename'
      cat(iconv(postmessages,to ="UTF-8"),file=filename)
      # if(createdfiles!="")
      #   createdfiles<-paste0(filename,"\n",createdfiles)
      # else
      #   createdfiles<-filename
      #Inititate cleaning process
      createdfiles<-cleanText(filename,createdfiles)
      
      #end time stamp of search
      end_time<-Sys.time()
      #calculates total time taken for execution
      total_time<-end_time - start_time
      
      msg<-paste(msg,"\n\n Total time taken is ",total_time,sep=" ",collapse = " ")
      
      #Finally add the entire log to the log_file 
      cat(msg,file=Log_file,append = TRUE)
    }
  }
  close(pb)
  return(createdfiles)
}
# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  zipfiles<-""
  session$onSessionEnded(function() {
    stopApp()
  })
  loc<-file.path(getwd(),"Outputs/*.txt")
  if (inherits(try(unlink(loc,force = TRUE)),"try-error"))
  {
    print("Can't delete!")
  }
  observeEvent(
    input$goButton,
    {
      if((has_internet("www.facebook.com")) && (has_internet(url1 = "plus.google.com")) && (has_internet("www.reddit.com")) && (has_internet("www.twitter.com"))== TRUE)
      {
        output$prog<-renderText("Connected")
      pwd<-getwd()
      pwd<-file.path(pwd,"Outputs//")
      createdfiles<-""
      zipop<-paste("Output",Sys.time())
      filename<-input$inputfile$datapath
      #opfilename<-input$outputfile
      opfilename<-pwd
      if(input$fb)
      {  
        output$textFB<-renderText(paste("Finished searching Facebook..."))
        createdfiles<-searchFB(filename,opfilename,createdfiles)
      }
      if(input$tw)
      {  
        output$textTW<-renderText(paste("Finished searching Twitter..."))
        createdfiles<-searchTW(filename,opfilename,createdfiles)
      }
      if(input$rd)
      {  
        output$textRD<-renderText(paste("Finished searching Reddit..."))
        createdfiles<-searchRD(filename,opfilename,createdfiles)
      }
      if(input$gp)
      {  
        output$textGP<-renderText(paste("Finished searching Google Plus..."))
        createdfiles<-searchGP(filename,opfilename,createdfiles)
      }
      print(createdfiles)
      zipfiles<-unlist(strsplit(x = createdfiles,split = "\n"))
      print(zipfiles)
      zipop<-gsub(":"," ",zipop)
      #zip(zipop,zipfiles)
      #print(zipop)
      tempf<-"Out.zip"
      # zipop<-paste0(zipop,".zip")
      # print(zipop)
      output$dl<-downloadHandler(filename = function()
      {
        paste0("Output ",gsub(":"," ",Sys.time()),".zip")
      }
      ,content =function(tempf)
        {
        zip(tempf,zipfiles)
      })
      }
      else
      {
        output$prog<-renderText("No internet connection. Please connect and try again")
      }
    }
  )
  # observe({
  #   flag<-input$goButton
  #   if(flag==TRUE)
  #   {filename<-isolate(input$inputfile)
  #   cat(filename)}
  # })
  # conn<-file(filename,open="r")
  # linn<-readLines(conn)
  # cat(filename)
  })
