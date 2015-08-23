library(shiny)
library(shinythemes)
library(DT)
library(stats)
library(doBy)
library(stringr)

blogs2 <- read.table("data/b2.txt",sep=",", fill=TRUE,quote="")
names(blogs2) = c("first","second","count")

news2 <- read.table("data/n2.txt",sep=",",fill=TRUE, quote="")
names(news2) = c("first","second","count")

twitter2 <- read.table("data/t2.txt",sep=",", fill=TRUE, quote="")
names(twitter2) = c("first","second","count")

blogs3 <- read.table("data/b3.txt",sep=",",quote="", fill=TRUE)
names(blogs3) = c("first", "second", "third", "count")

news3 <- read.table("data/n3.txt",sep=",",quote="", fill=TRUE)
names(news3) = c("first", "second", "third", "count")

twitter3 <- read.table("data/t3.txt",sep=",",quote="", fill=TRUE)
names(twitter3) = c("first", "second", "third", "count")

blogs4 <- read.table("data/b4.txt",sep=",",quote="", fill=TRUE)
names(blogs4) = c("first", "second", "third", "fourth", "count")

news4 <- read.table("data/n4.txt",sep=",",quote="", fill=TRUE)
names(news4) = c("first", "second", "third", "fourth", "count")

twitter4 <- read.table("data/t4.txt",sep=",",quote="", fill=TRUE)
names(twitter4) = c("first", "second", "third", "fourth", "count")

blogs5 <- read.table("data/b5.txt",sep=",",quote="", fill=TRUE)
names(blogs5) = c("first", "second", "third", "fourth", "fifth", "count")

news5 <- read.table("data/n5.txt",sep=",",quote="", fill=TRUE)
names(news5) = c("first", "second", "third", "fourth", "fifth", "count")

twitter5 <- read.table("data/t5.txt",sep=",",quote="", fill=TRUE)
names(twitter5) = c("first", "second", "third", "fourth", "fifth", "count")

blogs6 <- read.table("data/b6.txt",sep=",",quote="", fill=TRUE)
names(blogs6) = c("first", "second", "third", "fourth", "fifth", "sixth", "count")

news6 <- read.table("data/n6.txt",sep=",",quote="", fill=TRUE)
names(news6) = c("first", "second", "third", "fourth", "fifth", "sixth", "count")

twitter6 <- read.table("data/t6.txt",sep=",",quote="", fill=TRUE)
names(twitter6) = c("first", "second", "third", "fourth", "fifth", "sixth", "count")

blogs7 <- read.table("data/b7.txt",sep=",",quote="", fill=TRUE)
names(blogs7) = c("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "count")

news7 <- read.table("data/n7.txt",sep=",",quote="", fill=TRUE)
names(news7) = c("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "count")

twitter7 <- read.table("data/t7.txt",sep=",",quote="", fill=TRUE)
names(twitter7) = c("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "count")

blogs8 <- read.table("data/b8.txt",sep=",",quote="", fill=TRUE)
names(blogs8) = c("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "count")

news8 <- read.table("data/n8.txt",sep=",",quote="", fill=TRUE)
names(news8) = c("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "count")

twitter8 <- read.table("data/t8.txt",sep=",",quote="", fill=TRUE)
names(twitter8) = c("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "count")

blogs9 <- read.table("data/b9.txt",sep=",",quote="", fill=TRUE)
names(blogs9) = c("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth", "count")

news9 <- read.table("data/n9.txt",sep=",",quote="", fill=TRUE)
names(news9) = c("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth", "count")

twitter9 <- read.table("data/t9.txt",sep=",",quote="", fill=TRUE)
names(twitter9) = c("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth", "count")

countWords <- function(x){
  words <- length(unlist(strsplit(x, " ")))
  #words <- str_replace_all(temp, '[:punct:]', '')
}

segmentWords <- function(x){
  words_list <- strsplit(x," ")
  words <- unlist(words_list)
  #words <- str_replace_all(words, '[:punct:]', '')
  words
}
  
  
  biGrams <- function(data_source, word){
    #word = tolower(word)
    if((data_source=="blogs")==TRUE){
      blogs2 <- blogs2[which(blogs2$first==word),]
      blogs2<-orderBy(~-count, data=blogs2)
      blogs2sum <- sum(blogs2$count)
      blogs2$frequency<-blogs2$count / blogs2sum
      n_gram_data <- blogs2
    }else if(data_source=="news"){
      news2 <- news2[which(news2$first==word),]
      news2<-orderBy(~-count, data=news2)
      news2sum <- sum(news2$count)
      news2$frequency<-news2$count / news2sum
      n_gram_data <- news2
    }else if(data_source=="twitter"){
      twitter2 <- twitter2[which(twitter2$first==word),]
      twitter2<-orderBy(~-count, data=twitter2)
      twitter2sum <- sum(twitter2$count)
      twitter2$frequency<-twitter2$count / twitter2sum
      n_gram_data <- twitter2
    }else{
      n_gram_data <- data.frame(first=character(),second=character(), stringsAsFactors = FALSE)
      n_gram_data[1,1] = "not"
      n_gram_data[1,2] = "handled1"
      n_gram_data
    }
    rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
    n_gram_data[,1:2]
  }
  
  triGrams <- function(data_source, wordst){
    search_words = segmentWords(wordst)
    #search_words <- tolower(search_words)
    if((data_source=="blogs")==TRUE){
      blogs3 <- blogs3[which(blogs3$first==search_words[1] & blogs3$second==search_words[2]),]
      blogs3<-orderBy(~-count, data=blogs3)
      blogs3sum <- sum(blogs3$count)
      blogs3$frequency<-blogs3$count / blogs3sum
      n_gram_data <- blogs3
    }else if((data_source=="news")==TRUE){
      news3 <- news3[which(news3$first==search_words[1] & news3$second==search_words[2]),]
      news3<-orderBy(~-count, data=news3)
      news3sum <- sum(news3$count)
      news3$frequency<-news3$count / news3sum
      n_gram_data <- news3
    }else if((data_source=="twitter")==TRUE){
      twitter3 <- twitter3[which(twitter3$first==search_words[1] & twitter3$second==search_words[2]),]
      twitter3<-orderBy(~-count, data=twitter3)
      twitter3sum <- sum(twitter3$count)
      twitter3$frequency<-twitter3$count / twitter3sum
      n_gram_data <- twitter3
    }else{
      n_gram_data <- data.frame(first=character(),second=character(), third=character(), stringsAsFactors = FALSE)
      n_gram_data[1,1] = c("not")
      n_gram_data[1,2] = c("handled2")
      n_gram_data[1,3] = c("yet")
      n_gram_data
    }
    rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
    n_gram_data[,1:3]
  }
  
  quadGrams <- function(data_source, wordsq){
    search_words = segmentWords(wordsq)
    #search_words = tolower(search_words)
    if((data_source=="blogs")==TRUE){
      blogs4 <- blogs4[which(blogs4$first==search_words[1] & blogs4$second==search_words[2] & blogs4$third==search_words[3]),]
      blogs4<-orderBy(~-count, data=blogs4)
      blogs4sum <- sum(blogs4$count)
      blogs4$frequency<-blogs4$count / blogs4sum
      n_gram_data <- blogs4
    }else if((data_source=="news")==TRUE){
      news4 <- news4[which(news4$first==search_words[1] & news4$second==search_words[2] & news4$third==search_words[3]),]
      news4<-orderBy(~-count, data=news4)
      news4sum <- sum(news4$count)
      news4$frequency<-news4$count / news4sum
      n_gram_data <- news4
    }else if((data_source=="twitter")==TRUE){
      twitter4 <- twitter4[which(twitter4$first==search_words[1] & twitter4$second==search_words[2] & twitter4$third==search_words[3]),]
      twitter4<-orderBy(~-count, data=twitter4)
      twitter4sum <- sum(twitter4$count)
      twitter4$frequency<-twitter4$count / twitter4sum
      n_gram_data <- twitter4
    }else{
      n_gram_data <- data.frame(first=character(),second=character(), third=character(), fourth=character(), stringsAsFactors = FALSE)
      n_gram_data[1,1] = c("not")
      n_gram_data[1,2] = c("handled3")
      n_gram_data[1,3] = c("yet")
      n_gram_data[1,4] = c("really")
      n_gram_data
    }
    rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
    n_gram_data[,1:4]
  }
  
  pentaGrams <- function(data_source, wordsp){
    search_words = segmentWords(wordsp)
    #search_words = tolower(search_words)
    if((data_source=="blogs")==TRUE){
      blogs5 <- blogs5[which(blogs5$first==search_words[1] & blogs5$second==search_words[2] & blogs5$third==search_words[3]& blogs5$fourth==search_words[4]),]
      blogs5<-orderBy(~-count, data=blogs5)
      blogs5sum <- sum(blogs5$count)
      blogs5$frequency<-blogs5$count / blogs5sum
      n_gram_data <- blogs5
    }else if((data_source=="news")==TRUE){
      news5 <- news5[which(news5$first==search_words[1] & news5$second==search_words[2] & news5$third==search_words[3]& news5$fourth==search_words[4]),]
      news5<-orderBy(~-count, data=news5)
      news5sum <- sum(news5$count)
      news5$frequency<-news5$count / news5sum
      n_gram_data <- news5
    }else if((data_source=="twitter")==TRUE){
      twitter5 <- twitter5[which(twitter5$first==search_words[1] & twitter5$second==search_words[2] & twitter5$third==search_words[3]& twitter5$fourth==search_words[4]),]
      twitter5<-orderBy(~-count, data=twitter5)
      twitter5sum <- sum(twitter5$count)
      twitter5$frequency<-twitter5$count / twitter5sum
      n_gram_data <- twitter5
    }else{
      n_gram_data <- data.frame(first=character(),second=character(), third=character(), fourth=character(), fifth=character(), stringsAsFactors = FALSE)
      n_gram_data[1,1] = c("not")
      n_gram_data[1,2] = c("handled4")
      n_gram_data[1,3] = c("yet")
      n_gram_data[1,4] = c("really")
      n_gram_data[1,5] = c("uhhuh")
      n_gram_data
    }
    rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
    n_gram_data[,1:5]
  }
  
  hexaGrams <- function(data_source, wordsh){
    search_words = segmentWords(wordsh)
    #search_words = tolower(search_words)
    if((data_source=="blogs")==TRUE){
      blogs6 <- blogs6[which(blogs6$first==search_words[1] & blogs6$second==search_words[2] & blogs6$third==search_words[3]& blogs6$fourth==search_words[4] & blogs6$fifth==search_words[5]),]
      blogs6<-orderBy(~-count, data=blogs6)
      blogs6sum <- sum(blogs6$count)
      blogs6$frequency<-blogs6$count / blogs6sum
      n_gram_data <- blogs6
    }else if((data_source=="news")==TRUE){
      news6 <- news6[which(news6$first==search_words[1] & news6$second==search_words[2] & news6$third==search_words[3]& news6$fourth==search_words[4] & news6$fifth==search_words[5]),]
      news6<-orderBy(~-count, data=news6)
      news6sum <- sum(news6$count)
      news6$frequency<-news6$count / news6sum
      n_gram_data <- news6
    }else if((data_source=="twitter")==TRUE){
      twitter6 <- twitter6[which(twitter6$first==search_words[1] & twitter6$second==search_words[2] & twitter6$third==search_words[3]& twitter6$fourth==search_words[4] & twitter6$fifth==search_words[5]),]
      twitter6<-orderBy(~-count, data=twitter6)
      twitter6sum <- sum(twitter6$count)
      twitter6$frequency<-twitter6$count / twitter6sum
      n_gram_data <- twitter6
    }else{
      n_gram_data <- data.frame(first=character(),second=character(), third=character(), fourth=character(), fifth=character(), sixth=character(), stringsAsFactors = FALSE)
      n_gram_data[1,1] = c("not")
      n_gram_data[1,2] = c("handled4")
      n_gram_data[1,3] = c("yet")
      n_gram_data[1,4] = c("really")
      n_gram_data[1,5] = c("uhhuh")
      n_gram_data[1,6] = c("uhhuh")
      n_gram_data
    }
    rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
    n_gram_data[,1:6]
  }
  
  heptaGrams <- function(data_source, words7){
    search_words = segmentWords(words7)
    #search_words = tolower(search_words)
    if((data_source=="blogs")==TRUE){
      blogs7 <- blogs7[which(blogs7$first==search_words[1] & blogs7$second==search_words[2] & blogs7$third==search_words[3] & blogs7$fourth==search_words[4]& blogs7$fifth==search_words[5] & blogs7$sixth==search_words[6]),]
      blogs7<-orderBy(~-count, data=blogs7)
      blogs7sum <- sum(blogs7$count)
      blogs7$frequency<-blogs7$count / blogs7sum
      n_gram_data <- blogs7
    }else if((data_source=="news")==TRUE){
      news7 <- news7[which(news7$first==search_words[1] & news7$second==search_words[2] & news7$third==search_words[3] & news7$fourth==search_words[4]& news7$fifth==search_words[5] & news7$sixth==search_words[6]),]
      news7<-orderBy(~-count, data=news7)
      news7sum <- sum(news7$count)
      news7$frequency<-news7$count / news7sum
      n_gram_data <- news7
    }else if((data_source=="twitter")==TRUE){
      twitter7 <- twitter7[which(twitter7$first==search_words[1] & twitter7$second==search_words[2] & twitter7$third==search_words[3] & twitter7$fourth==search_words[4]& twitter7$fifth==search_words[5] & twitter7$sixth==search_words[6]),]
      twitter7<-orderBy(~-count, data=twitter7)
      twitter7sum <- sum(twitter7$count)
      twitter7$frequency<-twitter7$count / twitter7sum
      n_gram_data <- twitter7
    }else{
      n_gram_data <- data.frame(first=character(),second=character(), third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(),stringsAsFactors = FALSE)
      n_gram_data[1,1] = c("not")
      n_gram_data[1,2] = c("handled4")
      n_gram_data[1,3] = c("yet")
      n_gram_data[1,4] = c("really")
      n_gram_data[1,5] = c("uhhuh")
      n_gram_data[1,6] = c("uhhuh")
      n_gram_data[1,7] = c("uhhuh")
      n_gram_data
    }
    rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
    n_gram_data[,1:7]
  }
  
  octoGrams <- function(data_source, words8){
    search_words = segmentWords(words8)
    #search_words = tolower(search_words)
    if((data_source=="blogs")==TRUE){
      blogs8 <- blogs8[which(blogs8$first==search_words[1] & blogs8$second==search_words[2] & blogs8$third==search_words[3] & blogs8$fourth==search_words[4]& blogs8$fifth==search_words[5] & blogs8$sixth==search_words[6] & blogs8$seventh==search_words[7]),]
      blogs8<-orderBy(~-count, data=blogs8)
      blogs8sum <- sum(blogs8$count)
      blogs8$frequency<-blogs8$count / blogs8sum
      n_gram_data <- blogs8
    }else if((data_source=="news")==TRUE){
      news8 <- news8[which(news8$first==search_words[1] & news8$second==search_words[2] & news8$third==search_words[3] & news8$fourth==search_words[4]& news8$fifth==search_words[5] & news8$sixth==search_words[6] & news8$seventh==search_words[7]),]
      news8<-orderBy(~-count, data=news8)
      news8sum <- sum(news8$count)
      news8$frequency<-news8$count / news8sum
      n_gram_data <- news8
    }else if((data_source=="twitter")==TRUE){
      twitter8 <- twitter8[which(twitter8$first==search_words[1] & twitter8$second==search_words[2] & twitter8$third==search_words[3] & twitter8$fourth==search_words[4]& twitter8$fifth==search_words[5] & twitter8$sixth==search_words[6] & twitter8$seventh==search_words[7]),]
      twitter8<-orderBy(~-count, data=twitter8)
      twitter8sum <- sum(twitter8$count)
      twitter8$frequency<-twitter8$count / twitter8sum
      n_gram_data <- twitter8
    }else{
      n_gram_data <- data.frame(first=character(),second=character(), third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(), eighth=character(), stringsAsFactors = FALSE)
      n_gram_data[1,1] = c("not")
      n_gram_data[1,2] = c("handled4")
      n_gram_data[1,3] = c("yet")
      n_gram_data[1,4] = c("really")
      n_gram_data[1,5] = c("uhhuh")
      n_gram_data[1,6] = c("uhhuh")
      n_gram_data[1,7] = c("uhhuh")
      n_gram_data[1,8] = c("uhhuh")
      n_gram_data
    }
    rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
    n_gram_data[,1:8]
  }
  
  nonaGrams <- function(data_source, words9){
    search_words = segmentWords(words9)
    #search_words = tolower(search_words)
    if((data_source=="blogs")==TRUE){
      blogs9 <- blogs9[which(blogs9$first==search_words[1] & blogs9$second==search_words[2] & blogs9$third==search_words[3] & blogs9$fourth==search_words[4]& blogs9$fifth==search_words[5] & blogs9$sixth==search_words[6] & blogs9$seventh==search_words[7] & blogs9$eighth==search_words[8]),]
      blogs9<-orderBy(~-count, data=blogs9)
      blogs9sum <- sum(blogs9$count)
      blogs9$frequency<-blogs9$count / blogs9sum
      n_gram_data <- blogs9
    }else if((data_source=="news")==TRUE){
      news9 <- news9[which(news9$first==search_words[1] & news9$second==search_words[2] & news9$third==search_words[3] & news9$fourth==search_words[4]& news9$fifth==search_words[5] & news9$sixth==search_words[6] & news9$seventh==search_words[7] & news9$eighth==search_words[8]),]
      news9<-orderBy(~-count, data=news9)
      news9sum <- sum(news9$count)
      news9$frequency<-news9$count / news9sum
      n_gram_data <- news9
    }else if((data_source=="twitter")==TRUE){
      twitter9 <- twitter9[which(twitter9$first==search_words[1] & twitter9$second==search_words[2] & twitter9$third==search_words[3] & twitter9$fourth==search_words[4]& twitter9$fifth==search_words[5] & twitter9$sixth==search_words[6] & twitter9$seventh==search_words[7] & twitter9$eighth==search_words[8]),]
      twitter9<-orderBy(~-count, data=twitter9)
      twitter9sum <- sum(twitter9$count)
      twitter9$frequency<-twitter9$count / twitter9sum
      n_gram_data <- twitter9
    }else{
      n_gram_data <- data.frame(first=character(),second=character(), third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(), eighth=character(), ninth=character(), stringsAsFactors = FALSE)
      n_gram_data[1,1] = c("not")
      n_gram_data[1,2] = c("handled4")
      n_gram_data[1,3] = c("yet")
      n_gram_data[1,4] = c("really")
      n_gram_data[1,5] = c("uhhuh")
      n_gram_data[1,6] = c("uhhuh")
      n_gram_data[1,7] = c("uhhuh")
      n_gram_data[1,8] = c("uhhuh")
      n_gram_data[1,9] = c("uhhuh")
      n_gram_data
    }
    rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
    n_gram_data
  }
  
  # Define server logic required to draw a histogram
  shinyServer(function(input, output, session) {
    
    output$table = DT::renderDataTable(
      
      
      
      if((countWords(input$words_in)>8)==TRUE){
        n_gram_data <- data.frame(first=character(),second=character(), third=character(), stringsAsFactors = FALSE)
        n_gram_data[1,1] = c("Please")
        n_gram_data[1,2] = c("enter")
        n_gram_data[1,3] = c("eight")
        n_gram_data[1,4] = c("or")
        n_gram_data[1,5] = c("less")
        n_gram_data[1,6] = c("words")
        colnames(n_gram_data) <- c("first","second","third", "fourth","fifth","sixth")
        rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
        n_gram_data
      }else if((countWords(input$words_in)==0)==TRUE){
        n_gram_data <- data.frame(first=character(),second=character(), third=character(), stringsAsFactors = FALSE)
        n_gram_data[1,1] = c("Please")
        n_gram_data[1,2] = c("enter")
        n_gram_data[1,3] = c("words")
        colnames(n_gram_data) <- c("first","second","third")
        rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
        n_gram_data
      }else if((countWords(input$words_in)==1)==TRUE){
        temp = input$words_in
        temp2 = str_replace_all(temp, '[:punct:]', '')
        bi_grams <- biGrams(input$data_source, temp2)
        if((nrow(bi_grams)==0)==TRUE){
          n_gram_data <- data.frame(first=character(),prediction=character(), stringsAsFactors = FALSE)
          temp = as.character(input$words_in)
          n_gram_data[1,1] = str_replace_all(temp, '[:punct:]', '')
          n_gram_data[1,2] = c("is")
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          n_gram_data
        }else if((nrow(bi_grams)>0)==TRUE){
          n_gram_data = bi_grams
          colnames(n_gram_data) <- c("first","prediction")
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          n_gram_data
        }else{
          n_gram_data <- data.frame(first=character(),second=character(), stringsAsFactors = FALSE)
          n_gram_data[1,1] = "not"
          n_gram_data[1,2] = "handled5"
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          n_gram_data
        }
      }else if((countWords(input$words_in)==2)==TRUE){
        tri_grams <- triGrams(input$data_source, input$words_in)
        if((nrow(tri_grams)==0)==TRUE){
          segmented_words = segmentWords(input$words_in)
          bi_grams <- biGrams(input$data_source, segmented_words[2])
          if((nrow(bi_grams)==0)==TRUE){
            n_gram_data <- data.frame(first=character(),second=character(), third=character(), stringsAsFactors = FALSE)
            n_gram_data[1,1] = as.character(segmented_words[1])
            n_gram_data[1,2] = as.character(segmented_words[2])
            n_gram_data[1,3] = "is"
            rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
            n_gram_data
          }else if((nrow(bi_grams)>0)==TRUE){
            n_gram_data <- data.frame(first=character(),second=character(), third=character(), stringsAsFactors = FALSE)
            n_gram_data[1,1] = segmented_words[1]
            n_gram_data[1,2] = segmented_words[2]
            n_gram_data[1,3] = as.character(bi_grams[1,2])
            rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
            n_gram_data
          }else{
            n_gram_data <- data.frame(first=character(),second=character(), stringsAsFactors = FALSE)
            n_gram_data[1,1] = "not"
            n_gram_data[1,2] = "handled6"
            rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
            n_gram_data
          }
        }else if((nrow(tri_grams)>0)==TRUE){
          n_gram_data <- tri_grams
          colnames(n_gram_data) <- c("first","second","third")
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          n_gram_data
        }else{
          n_gram_data <- data.frame(first=character(),second=character(), third=character(), stringsAsFactors = FALSE)
          n_gram_data[1,1] = "this"
          n_gram_data[1,2] = "not"
          n_gram_data[1,3] = "handled7"
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          n_gram_data
        }
      }else if((countWords(input$words_in)==3)==TRUE){
        quad_grams <- quadGrams(input$data_source, input$words_in)
        if((nrow(quad_grams)==0)==TRUE){
          segmented_words = segmentWords(input$words_in)
          to_drive_trigrams = paste(segmented_words[2], segmented_words[3])
          tri_grams = triGrams(input$data_source, to_drive_trigrams)
          if((nrow(tri_grams)==0)==TRUE){
            segmented_words = segmentWords(input$words_in)
            to_drive_bigrams = unlist(segmented_words[3])
            bi_grams = biGrams(input$data_source, to_drive_bigrams)
            if((nrow(bi_grams)==0)==TRUE){
              n_gram_data <- data.frame(first=character(),second=character(),third=character(), fourth=character(), stringsAsFactors = FALSE)
              n_gram_data[1,1] = segmented_words[1]
              n_gram_data[1,2] = segmented_words[2]
              n_gram_data[1,3] = segmented_words[3]
              n_gram_data[1,4] = "is"
              rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
              colnames(n_gram_data) <- c("first","second","third","fourth")
              n_gram_data
            }else if((nrow(bi_grams)>0)==TRUE){
              col_2_length = nrow(bi_grams)
              col_2 = rep_len(segmented_words[2],col_2_length)
              three_of_four_cols = cbind(col_2, bi_grams)
              col_1_length = col_2_length
              col_1 = rep_len(segmented_words[1],col_1_length)
              n_gram_data = cbind(col_1, three_of_four_cols)
              rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
              colnames(n_gram_data) = c("first", "second", "third", "fourth")
              n_gram_data
            }else{
              n_gram_data <- data.frame(first=character(),second=character(), third=character(), stringsAsFactors = FALSE)
              n_gram_data[1,1] = "this"
              n_gram_data[1,2] = "not"
              n_gram_data[1,3] = "handled8"
              n_gram_data[1,4] = "yes"
              rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
              n_gram_data
            }
            
          }else if((nrow(tri_grams)>0)==TRUE){
            col_1_length = nrow(tri_grams)
            col_1 = rep_len(segmented_words[1],col_1_length)
            n_gram_data = cbind(col_1, tri_grams)
            rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
            colnames(n_gram_data) <- c("first", "second", "third", "fourth")
            n_gram_data
          }else{
            n_gram_data <- data.frame(first=character(),second=character(), third=character(),
                                      fourth=character(), stringsAsFactors = FALSE)
            n_gram_data[1,1] = "this"
            n_gram_data[1,2] = "not"
            n_gram_data[1,3] = "handled9"
            n_gram_data[1,4] = "yes"
            rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
            n_gram_data
          }
        }else if((nrow(quad_grams)>0)==TRUE){
          n_gram_data <- quad_grams
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          colnames(n_gram_data) <- c("first", "second", "third", "fourth")
          n_gram_data
        }else{
          n_gram_data <- data.frame(first=character(),second=character(), third=character(),
                                    fourth=character(), stringsAsFactors = FALSE)
          n_gram_data[1,1] = "this"
          n_gram_data[1,2] = "not"
          n_gram_data[1,3] = "handled10"
          n_gram_data[1,4] = "yes"
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          n_gram_data
        }
      }else if((countWords(input$words_in)==4)==TRUE){
        
        penta_grams <- pentaGrams(input$data_source, input$words_in[1])
        if((nrow(penta_grams)==0)==TRUE){
          segmented_words = segmentWords(input$words_in)
          last_three_cols = paste(unlist(segmented_words[2]), unlist(segmented_words[3]), unlist(segmented_words[4]))
          quad_grams <- quadGrams(input$data_source, last_three_cols)
          if((nrow(quad_grams)==0)==TRUE){
            segmented_words = segmentWords(input$words_in)
            last_two_cols = paste(unlist(segmented_words[3]), unlist(segmented_words[4]))
            tri_grams = triGrams(input$data_source, last_two_cols)
            if((nrow(tri_grams)==0)==TRUE){
              segmented_words = segmentWords(input$words_in)
              last_col = unlist(segmented_words[4])
              bi_grams = biGrams(input$data_source, last_col)
              if((nrow(bi_grams)==0)==TRUE){
                n_gram_data <- data.frame(first=character(),second=character(),
                                          third=character(), fourth=character(), fifth=character(),
                                          stringsAsFactors = FALSE)
                n_gram_data[1,1] = segmented_words[1]
                n_gram_data[1,2] = segmented_words[2]
                n_gram_data[1,3] = segmented_words[3]
                n_gram_data[1,4] = segmented_words[4]
                n_gram_data[1,5] = c("is")
                rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                n_gram_data
              }else if((nrow(bi_grams)>0)==TRUE){
                col_1_length = nrow(bi_grams)
                col_1 = rep_len(segmented_words[1],col_1_length)
                col_2_length = nrow(bi_grams)
                col_2 = rep_len(segmented_words[2],col_2_length)
                col_3_length = nrow(bi_grams)
                col_3 = rep_len(segmented_words[3],col_3_length)
                temp <- cbind(col_1, col_2)
                temp2 <- cbind(temp, col_3)
                n_gram_data = cbind(temp2, bi_grams)
                rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                colnames(n_gram_data) <- c("eq4bigrams>0","second","third","fourth","fifth")
                n_gram_data
              }else{
                n_gram_data <- data.frame(first=character(),second=character(),
                                          third=character(), fourth=character(), fifth=character(), stringsAsFactors = FALSE)
                n_gram_data[1,1] = "this"
                n_gram_data[1,2] = "not"
                n_gram_data[1,3] = "handled11"
                n_gram_data[1,4] = "yes"
                n_gram_data[1,5] = "yes"
                rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                n_gram_data
              }
            }else if((nrow(tri_grams)>0)==TRUE){
              col_1_length = nrow(tri_grams)
              col_1 = rep_len(segmented_words[1],col_1_length)
              last_four_cols = cbind(col_1, tri_grams)
              n_gram_data = cbind(col_1, last_four_cols)
              rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
              colnames(n_gram_data) <- c("eq4trigrams>0","second","third","fourth","fifth")
              n_gram_data
            }else{
              n_gram_data <- data.frame(first=character(),second=character(),
                                        third=character(), fourth=character(), fifth=character(), stringsAsFactors = FALSE)
              n_gram_data[1,1] = "this"
              n_gram_data[1,2] = "not"
              n_gram_data[1,3] = "handled12"
              n_gram_data[1,4] = "yes"
              n_gram_data[1,5] = "yes"
              rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
              n_gram_data
            }
          }else if((nrow(quad_grams)>0)==TRUE){
            col_1_length = nrow(quad_grams)
            col_1 = rep_len(segmented_words[1],col_1_length)
            n_gram_data = cbind(col_1, quad_grams)
            rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
            colnames(n_gram_data) <- c("eq4quadgrams>0","second","third","fourth","fifth")
            n_gram_data
          }else{
            n_gram_data <- data.frame(first=character(),second=character(),
                                      third=character(), fourth=character(), fifth=character(), stringsAsFactors = FALSE)
            n_gram_data[1,1] = "this"
            n_gram_data[1,2] = "not"
            n_gram_data[1,3] = "handled13"
            n_gram_data[1,4] = "yes"
            n_gram_data[1,5] = "yes"
            rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
            n_gram_data
          }
        }else if((nrow(penta_grams)>0)==TRUE){
          n_gram_data <- penta_grams
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          colnames(n_gram_data) <- c("eq4pentagrams>0", "second", "third", "fourth", "fifth")
          n_gram_data
        }else{
          n_gram_data <- data.frame(first=character(),second=character(),
                                    third=character(), fourth=character(), fifth=character(), stringsAsFactors = FALSE)
          n_gram_data[1,1] = "this"
          n_gram_data[1,2] = "not"
          n_gram_data[1,3] = "handled14"
          n_gram_data[1,4] = "yes"
          n_gram_data[1,5] = "yes"
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          n_gram_data
        }
      }else if((countWords(input$words_in)==5)==TRUE){
        hexa_grams <- hexaGrams(input$data_source, input$words_in[1])
        if((nrow(hexa_grams)==0)==TRUE){
          segmented_words = segmentWords(input$words_in)
          last_four_cols = paste(unlist(segmented_words[2]), unlist(segmented_words[3]), unlist(segmented_words[4])
                                 , unlist(segmented_words[5]))
          penta_grams <- pentaGrams(input$data_source, last_four_cols)
          if((nrow(penta_grams)==0)==TRUE){
            segmented_words = segmentWords(input$words_in)
            last_three_cols = paste(unlist(segmented_words[3]), unlist(segmented_words[4]), unlist(segmented_words[5]))
            quad_grams <- quadGrams(input$data_source, last_three_cols)
            if((nrow(quad_grams)==0)==TRUE){
              segmented_words = segmentWords(input$words_in)
              last_two_cols = paste(unlist(segmented_words[4]), unlist(segmented_words[5]))
              tri_grams = triGrams(input$data_source, last_two_cols)
              if((nrow(tri_grams)==0)==TRUE){
                segmented_words = segmentWords(input$words_in)
                last_col = unlist(segmented_words[5])
                bi_grams = biGrams(input$data_source, last_col)
                if((nrow(bi_grams)==0)==TRUE){
                  n_gram_data <- data.frame(first=character(),second=character(),
                                            third=character(), fourth=character(), fifth=character(),
                                            sixth=character(), stringsAsFactors = FALSE)
                  n_gram_data[1,1] = segmented_words[1]
                  n_gram_data[1,2] = segmented_words[2]
                  n_gram_data[1,3] = segmented_words[3]
                  n_gram_data[1,4] = segmented_words[4]
                  n_gram_data[1,5] = segmented_words[5]
                  n_gram_data[1,6] = c("is")
                  rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                  n_gram_data
                }else if((nrow(bi_grams)>0)==TRUE){ #works
                  col_1_length = nrow(bi_grams)
                  col_1 = rep_len(segmented_words[1],col_1_length)
                  col_2_length = nrow(bi_grams)
                  col_2 = rep_len(segmented_words[2],col_2_length)
                  col_3_length = nrow(bi_grams)
                  col_3 = rep_len(segmented_words[3],col_3_length)
                  col_4_length = nrow(bi_grams)
                  col_4 = rep_len(segmented_words[4],col_4_length)
                  last_three_cols = cbind(col_4, bi_grams)
                  last_four_cols = cbind(col_3,last_three_cols)
                  last_five_cols = cbind(col_2,last_four_cols)
                  n_gram_data = cbind(col_1, last_five_cols)
                  rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                  colnames(n_gram_data) <- c("eq5bigrams>0","second","third","fourth","fifth","sixth")
                  n_gram_data
                }else{
                  n_gram_data <- data.frame(first=character(),second=character(),
                                            third=character(), fourth=character(), fifth=character(), stringsAsFactors = FALSE)
                  n_gram_data[1,1] = "this"
                  n_gram_data[1,2] = "not"
                  n_gram_data[1,3] = "handledA"
                  n_gram_data[1,4] = "yes"
                  n_gram_data[1,5] = "yes"
                  n_gram_data[1,6] = "yes"
                  rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                  n_gram_data
                }
              }else if((nrow(tri_grams)>0)==TRUE){ #works
                col_1_length = nrow(tri_grams)
                col_1 = rep_len(segmented_words[1],col_1_length)
                col_2_length = nrow(tri_grams)
                col_2 = rep_len(segmented_words[2],col_2_length)
                col_3_length = nrow(tri_grams)
                col_3 = rep_len(segmented_words[3],col_3_length)
                temp = cbind(col_1, col_2)
                temp2 = cbind(temp, col_3)
                n_gram_data <- cbind(temp2, tri_grams)
                rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                colnames(n_gram_data) <- c("eq5trigrams>0","second","third","fourth","fifth", "sixth")
                n_gram_data
              }else{
                n_gram_data <- data.frame(first=character(),second=character(),
                                          third=character(), fourth=character(), fifth=character(), stringsAsFactors = FALSE)
                n_gram_data[1,1] = "this"
                n_gram_data[1,2] = "not"
                n_gram_data[1,3] = "handledB"
                n_gram_data[1,4] = "yes"
                n_gram_data[1,5] = "yes"
                n_gram_data[1,6] = "yes"
                rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                n_gram_data
              }
            }else if((nrow(quad_grams)>0)==TRUE){ #works
              col_1_length = nrow(quad_grams)
              col_1 = rep_len(segmented_words[1],col_1_length)
              col_2_length = nrow(quad_grams)
              col_2 = rep_len(segmented_words[2],col_2_length)
              temp <- cbind(col_1, col_2)
              n_gram_data <- cbind(temp, quad_grams)
              rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
              colnames(n_gram_data) <- c("eq5quadgrams>0","second","third","fourth","fifth", "sixth")
              n_gram_data
            }else{
              n_gram_data <- data.frame(first=character(),second=character(),
                                        third=character(), fourth=character(), fifth=character(), sixth=character(), stringsAsFactors = FALSE)
              n_gram_data[1,1] = "this"
              n_gram_data[1,2] = "not"
              n_gram_data[1,3] = "handledC"
              n_gram_data[1,4] = "yes"
              n_gram_data[1,5] = "yes"
              n_gram_data[1,6] = "yes"
              rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
              n_gram_data
            }
          }else if((nrow(penta_grams)>0)==TRUE){ #works
            
            col_1_length = nrow(penta_grams)
            col_1 = rep_len(segmented_words[1],col_1_length)
            n_gram_data = cbind(col_1, penta_grams)
            rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
            colnames(n_gram_data) <- c("eq5pentagrams>0","second","third","fourth","fifth", "sixth")
            n_gram_data
            
          }else{
            n_gram_data <- data.frame(first=character(),second=character(),
                                      third=character(), fourth=character(), fifth=character(),
                                      sixth=character(), stringsAsFactors = FALSE)
            n_gram_data[1,1] = "this"
            n_gram_data[1,2] = "not"
            n_gram_data[1,3] = "handledC"
            n_gram_data[1,4] = "yes"
            n_gram_data[1,5] = "yes"
            n_gram_data[1,6] = "yes"
            rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
            n_gram_data
          }
        }else if((nrow(hexa_grams)>0)==TRUE){ #works
          n_gram_data <- hexa_grams
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          colnames(n_gram_data) <- c("eq5hexagrams>0", "second", "third", "fourth", "fifth", "sixth")
          n_gram_data[,1:6]
        }else{
          n_gram_data <- data.frame(first=character(),second=character(),
                                    third=character(), fourth=character(), fifth=character(), sixth=character(), stringsAsFactors = FALSE)
          n_gram_data[1,1] = "this"
          n_gram_data[1,2] = "not"
          n_gram_data[1,3] = "hexa_grams_else"
          n_gram_data[1,4] = "yes"
          n_gram_data[1,5] = "yes"
          n_gram_data[1,6] = "yes"
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          n_gram_data
        }
      }else if((countWords(input$words_in)==6)==TRUE){
        hepta_grams <- heptaGrams(input$data_source, input$words_in[1])
        if((nrow(hepta_grams)==0)==TRUE){
          segmented_words = segmentWords(input$words_in)
          last_five_cols = paste(unlist(segmented_words[2]),unlist(segmented_words[3]), unlist(segmented_words[4]), unlist(segmented_words[5])
                                 , unlist(segmented_words[6]))
          hexa_grams <- hexaGrams(input$data_source, last_five_cols)
          if((nrow(hexa_grams)==0)==TRUE){
            segmented_words = segmentWords(input$words_in)
            last_four_cols = paste(unlist(segmented_words[3]), unlist(segmented_words[4]), unlist(segmented_words[5])
                                   , unlist(segmented_words[6]))
            penta_grams <- pentaGrams(input$data_source, last_four_cols)
            if((nrow(penta_grams)==0)==TRUE){
              segmented_words = segmentWords(input$words_in)
              last_three_cols = paste(unlist(segmented_words[4]), unlist(segmented_words[5]), unlist(segmented_words[6]))
              quad_grams <- quadGrams(input$data_source, last_three_cols)
              if((nrow(quad_grams)==0)==TRUE){
                segmented_words = segmentWords(input$words_in)
                last_two_cols = paste(unlist(segmented_words[5]), unlist(segmented_words[6]))
                tri_grams = triGrams(input$data_source, last_two_cols)
                if((nrow(tri_grams)==0)==TRUE){
                  segmented_words = segmentWords(input$words_in)
                  last_col = unlist(segmented_words[6])
                  bi_grams = biGrams(input$data_source, last_col)
                  if((nrow(bi_grams)==0)==TRUE){
                    n_gram_data <- data.frame(first=character(),second=character(),
                                              third=character(), fourth=character(), fifth=character(),
                                              sixth=character(), seventh=character(), stringsAsFactors = FALSE)
                    n_gram_data[1,1] = segmented_words[1]
                    n_gram_data[1,2] = segmented_words[2]
                    n_gram_data[1,3] = segmented_words[3]
                    n_gram_data[1,4] = segmented_words[4]
                    n_gram_data[1,5] = segmented_words[5]
                    n_gram_data[1,6] = segmented_words[6]
                    n_gram_data[1,7] = c("is")
                    rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                    n_gram_data
                  }else if((nrow(bi_grams)>0)==TRUE){ #works
                    col_1_length = nrow(bi_grams)
                    col_1 = rep_len(segmented_words[1],col_1_length)
                    col_2_length = nrow(bi_grams)
                    col_2 = rep_len(segmented_words[2],col_2_length)
                    col_3_length = nrow(bi_grams)
                    col_3 = rep_len(segmented_words[3],col_3_length)
                    col_4_length = nrow(bi_grams)
                    col_4 = rep_len(segmented_words[4],col_4_length)
                    col_5_length = nrow(bi_grams)
                    col_5 = rep_len(segmented_words[5],col_5_length)
                    temp = cbind(col_1, col_2)
                    temp2 = cbind(temp, col_3)
                    temp3 = cbind(temp2, col_4)
                    temp4 = cbind(temp3, col_5)
                    n_gram_data <- cbind(temp4, bi_grams)
                    rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                    colnames(n_gram_data) <- c("eq6bigrams>0","second","third","fourth","fifth","sixth", "seventh")
                    n_gram_data[1:7]
                  }else{
                    n_gram_data <- data.frame(first=character(),second=character(),
                                              third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(), stringsAsFactors = FALSE)
                    n_gram_data[1,1] = "this"
                    n_gram_data[1,2] = "not"
                    n_gram_data[1,3] = "handledA"
                    n_gram_data[1,4] = "yes"
                    n_gram_data[1,5] = "yes"
                    n_gram_data[1,6] = "yes"
                    n_gram_data[1,7] = "yes"
                    rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                    n_gram_data
                  }
                }else if((nrow(tri_grams)>0)==TRUE){ #works
                  col_1_length = nrow(tri_grams)
                  col_1 = rep_len(segmented_words[1],col_1_length)
                  col_2_length = nrow(tri_grams)
                  col_2 = rep_len(segmented_words[2],col_2_length)
                  col_3_length = nrow(tri_grams)
                  col_3 = rep_len(segmented_words[3],col_3_length)
                  col_4_length = nrow(tri_grams)
                  col_4 = rep_len(segmented_words[4],col_4_length)
                  temp = cbind(col_1, col_2)
                  temp2 = cbind(temp, col_3)
                  temp3 = cbind(temp2, col_4)
                  n_gram_data <- cbind(temp3, tri_grams)
                  rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                  colnames(n_gram_data) <- c("eq6trigrams>0","second","third","fourth","fifth", "sixth", "seventh")
                  n_gram_data
                }else{
                  n_gram_data <- data.frame(first=character(),second=character(),
                                            third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(),  stringsAsFactors = FALSE)
                  n_gram_data[1,1] = "this"
                  n_gram_data[1,2] = "not"
                  n_gram_data[1,3] = "handledB"
                  n_gram_data[1,4] = "yes"
                  n_gram_data[1,5] = "yes"
                  n_gram_data[1,6] = "yes"
                  n_gram_data[1,7] = "yes"
                  rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                  n_gram_data
                }
              }else if((nrow(quad_grams)>0)==TRUE){ #works
                col_1_length = nrow(quad_grams)
                col_1 = rep_len(segmented_words[1],col_1_length)
                col_2_length = nrow(quad_grams)
                col_2 = rep_len(segmented_words[2],col_2_length)
                col_3_length = nrow(quad_grams)
                col_3 = rep_len(segmented_words[3],col_3_length)
                temp <- cbind(col_1, col_2)
                temp2 <- cbind(temp, col_3)
                n_gram_data <- cbind(temp2, quad_grams)
                rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                colnames(n_gram_data) <- c("eq6quadgrams>0","second","third","fourth","fifth", "sixth", "seventh")
                n_gram_data
              }else{
                n_gram_data <- data.frame(first=character(),second=character(),
                                          third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(),stringsAsFactors = FALSE)
                n_gram_data[1,1] = "this"
                n_gram_data[1,2] = "not"
                n_gram_data[1,3] = "handledC"
                n_gram_data[1,4] = "yes"
                n_gram_data[1,5] = "yes"
                n_gram_data[1,6] = "yes"
                n_gram_data[1,7] = "yes"
                rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                n_gram_data
              }
            }else if((nrow(penta_grams)>0)==TRUE){ #works
              col_1_length = nrow(penta_grams)
              col_1 = rep_len(segmented_words[1],col_1_length)
              col_2_length = nrow(penta_grams)
              col_2 = rep_len(segmented_words[2],col_1_length)
              temp <- cbind(col_1, col_2)
              n_gram_data <- cbind(temp, penta_grams)
              rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
              colnames(n_gram_data) <- c("eq6pentagrams>0","second","third","fourth","fifth","sixth","seventh")
              n_gram_data
            }else{
              n_gram_data <- data.frame(first=character(),second=character(),
                                        third=character(), fourth=character(), fifth=character(),
                                        sixth=character(), seventh=character(), stringsAsFactors = FALSE)
              n_gram_data[1,1] = "this"
              n_gram_data[1,2] = "not"
              n_gram_data[1,3] = "handledC"
              n_gram_data[1,4] = "yes"
              n_gram_data[1,5] = "yes"
              n_gram_data[1,6] = "yes"
              n_gram_data[1,7] = "yes"
              rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
              n_gram_data
            }
          }else if((nrow(hexa_grams)>0)==TRUE){ #works
            col_1_length = nrow(hexa_grams)
            col_1 = rep_len(segmented_words[1],col_1_length)
            n_gram_data <- cbind(col_1, hexa_grams)
            rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
            colnames(n_gram_data) <- c("eq6hexagrams>0", "second", "third", "fourth", "fifth", "sixth", "seventh")
            n_gram_data[,1:7]
          }else{
            n_gram_data <- data.frame(first=character(),second=character(),
                                      third=character(), fourth=character(), fifth=character(), sixth=character(), stringsAsFactors = FALSE)
            n_gram_data[1,1] = "this"
            n_gram_data[1,2] = "not"
            n_gram_data[1,3] = "hexa_grams_else"
            n_gram_data[1,4] = "yes"
            n_gram_data[1,5] = "yes"
            n_gram_data[1,6] = "yes"
            n_gram_data[1,7] = "yes"
            rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
            n_gram_data
          }
        }else if((nrow(hepta_grams)>0)==TRUE){ #Does not work
          n_gram_data <- hepta_grams
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          colnames(n_gram_data) <- c("eq6heptagrams>0", "second", "third", "fourth", "fifth", "sixth", "seventh")
          n_gram_data[,1:7]
        }else{
          print("situation where hepta_grams are == 0 or > 0 -- case not handled")
        }
        
      }else if((countWords(input$words_in)==7)==TRUE){
        octo_grams <- octoGrams(input$data_source, input$words_in[1])
        if((nrow(octo_grams)==0)==TRUE){
          segmented_words = segmentWords(input$words_in)
          last_six_cols = paste(unlist(segmented_words[2]), unlist(segmented_words[3]),unlist(segmented_words[4]), unlist(segmented_words[5]), unlist(segmented_words[6])
                                , unlist(segmented_words[7]))
          
          hepta_grams <- heptaGrams(input$data_source, last_six_cols)
          if((nrow(hepta_grams)==0)==TRUE){
            segmented_words = segmentWords(input$words_in)
            last_five_cols = paste(unlist(segmented_words[3]),unlist(segmented_words[4]), unlist(segmented_words[5]), unlist(segmented_words[6])
                                   , unlist(segmented_words[7]))
            hexa_grams <- hexaGrams(input$data_source, last_five_cols)
            if((nrow(hexa_grams)==0)==TRUE){
              segmented_words = segmentWords(input$words_in)
              last_four_cols = paste(unlist(segmented_words[4]), unlist(segmented_words[5]), unlist(segmented_words[6])
                                     , unlist(segmented_words[7]))
              penta_grams <- pentaGrams(input$data_source, last_four_cols)
              if((nrow(penta_grams)==0)==TRUE){
                segmented_words = segmentWords(input$words_in)
                last_three_cols = paste(unlist(segmented_words[5]), unlist(segmented_words[6]), unlist(segmented_words[7]))
                quad_grams <- quadGrams(input$data_source, last_three_cols)
                if((nrow(quad_grams)==0)==TRUE){
                  segmented_words = segmentWords(input$words_in)
                  last_two_cols = paste(unlist(segmented_words[6]), unlist(segmented_words[7]))
                  tri_grams = triGrams(input$data_source, last_two_cols)
                  if((nrow(tri_grams)==0)==TRUE){
                    segmented_words = segmentWords(input$words_in)
                    last_col = unlist(segmented_words[7])
                    bi_grams = biGrams(input$data_source, last_col)
                    if((nrow(bi_grams)==0)==TRUE){
                      n_gram_data <- data.frame(first=character(),second=character(),
                                                third=character(), fourth=character(), fifth=character(),
                                                sixth=character(), seventh=character(), eighth=character(), stringsAsFactors = FALSE)
                      n_gram_data[1,1] = segmented_words[1]
                      n_gram_data[1,2] = segmented_words[2]
                      n_gram_data[1,3] = segmented_words[3]
                      n_gram_data[1,4] = segmented_words[4]
                      n_gram_data[1,5] = segmented_words[5]
                      n_gram_data[1,6] = segmented_words[6]
                      n_gram_data[1,7] = segmented_words[7]
                      n_gram_data[1,8] = c("is")
                      rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                      n_gram_data
                    }else if((nrow(bi_grams)>0)==TRUE){ #works
                      col_1_length = nrow(bi_grams)
                      col_1 = rep_len(segmented_words[1],col_1_length)
                      col_2_length = nrow(bi_grams)
                      col_2 = rep_len(segmented_words[2],col_2_length)
                      col_3_length = nrow(bi_grams)
                      col_3 = rep_len(segmented_words[3],col_3_length)
                      col_4_length = nrow(bi_grams)
                      col_4 = rep_len(segmented_words[4],col_4_length)
                      col_5_length = nrow(bi_grams)
                      col_5 = rep_len(segmented_words[5],col_5_length)
                      col_6_length = nrow(bi_grams)
                      col_6 = rep_len(segmented_words[6],col_6_length)
                      temp = cbind(col_1, col_2)
                      temp2 = cbind(temp, col_3)
                      temp3 = cbind(temp2, col_4)
                      temp4 = cbind(temp3, col_5)
                      temp5 = cbind(temp4, col_6)
                      n_gram_data <- cbind(temp5, bi_grams)
                      rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                      colnames(n_gram_data) <- c("eq7bigrams>0","second","third","fourth","fifth","sixth", "seventh", "eighth")
                      n_gram_data[1:8]
                    }else{
                      n_gram_data <- data.frame(first=character(),second=character(),
                                                third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(), eighth=character(),stringsAsFactors = FALSE)
                      n_gram_data[1,1] = "this"
                      n_gram_data[1,2] = "not"
                      n_gram_data[1,3] = "handledA"
                      n_gram_data[1,4] = "yes"
                      n_gram_data[1,5] = "yes"
                      n_gram_data[1,6] = "yes"
                      n_gram_data[1,7] = "yes"
                      n_gram_data[1,8] = "yes"
                      rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                      n_gram_data
                    }
                  }else if((nrow(tri_grams)>0)==TRUE){ #works
                    col_1_length = nrow(tri_grams)
                    col_1 = rep_len(segmented_words[1],col_1_length)
                    col_2_length = nrow(tri_grams)
                    col_2 = rep_len(segmented_words[2],col_2_length)
                    col_3_length = nrow(tri_grams)
                    col_3 = rep_len(segmented_words[3],col_3_length)
                    col_4_length = nrow(tri_grams)
                    col_4 = rep_len(segmented_words[4],col_4_length)
                    col_5_length = nrow(tri_grams)
                    col_5 = rep_len(segmented_words[5],col_5_length)
                    temp = cbind(col_1, col_2)
                    temp2 = cbind(temp, col_3)
                    temp3 = cbind(temp2, col_4)
                    temp4 = cbind(temp3, col_5)
                    n_gram_data <- cbind(temp4, tri_grams)
                    rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                    colnames(n_gram_data) <- c("eq7trigrams>0","second","third","fourth","fifth", "sixth", "seventh", "eighth")
                    n_gram_data
                  }else{
                    n_gram_data <- data.frame(first=character(),second=character(),
                                              third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(),  eighth=character(), stringsAsFactors = FALSE)
                    n_gram_data[1,1] = "this"
                    n_gram_data[1,2] = "not"
                    n_gram_data[1,3] = "handledB"
                    n_gram_data[1,4] = "yes"
                    n_gram_data[1,5] = "yes"
                    n_gram_data[1,6] = "yes"
                    n_gram_data[1,7] = "yes"
                    n_gram_data[1,8] = "yes"
                    rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                    n_gram_data
                  }
                }else if((nrow(quad_grams)>0)==TRUE){ #works
                  col_1_length = nrow(quad_grams)
                  col_1 = rep_len(segmented_words[1],col_1_length)
                  col_2_length = nrow(quad_grams)
                  col_2 = rep_len(segmented_words[2],col_2_length)
                  col_3_length = nrow(quad_grams)
                  col_3 = rep_len(segmented_words[3],col_3_length)
                  col_4_length = nrow(quad_grams)
                  col_4 = rep_len(segmented_words[4],col_4_length)
                  temp = cbind(col_1, col_2)
                  temp2 = cbind(temp, col_3)
                  temp3 = cbind(temp2, col_4)
                  n_gram_data <- cbind(temp3, quad_grams)
                  rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                  colnames(n_gram_data) <- c("eq7quadgrams>0","second","third","fourth","fifth", "sixth", "seventh", "eighth")
                  n_gram_data
                }else{
                  n_gram_data <- data.frame(first=character(),second=character(),
                                            third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(), eighth=character(), stringsAsFactors = FALSE)
                  n_gram_data[1,1] = "this"
                  n_gram_data[1,2] = "not"
                  n_gram_data[1,3] = "handledC"
                  n_gram_data[1,4] = "yes"
                  n_gram_data[1,5] = "yes"
                  n_gram_data[1,6] = "yes"
                  n_gram_data[1,7] = "yes"
                  n_gram_data[1,8] = "yes"
                  rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                  n_gram_data
                }
              }else if((nrow(penta_grams)>0)==TRUE){ #works
                col_1_length = nrow(penta_grams)
                col_1 = rep_len(segmented_words[1],col_1_length)
                col_2_length = nrow(penta_grams)
                col_2 = rep_len(segmented_words[2],col_2_length)
                col_3_length = nrow(penta_grams)
                col_3 = rep_len(segmented_words[3],col_3_length)
                temp <- cbind(col_1, col_2)
                temp2 <- cbind(temp, col_3)
                n_gram_data <- cbind(temp2, penta_grams)
                rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                colnames(n_gram_data) <- c("e7pentagrams>0","second","third","fourth","fifth","sixth","seventh", "eighth")
                n_gram_data
              }else{
                n_gram_data <- data.frame(first=character(),second=character(),
                                          third=character(), fourth=character(), fifth=character(),
                                          sixth=character(), seventh=character(), eighth=character(), stringsAsFactors = FALSE)
                n_gram_data[1,1] = "this"
                n_gram_data[1,2] = "not"
                n_gram_data[1,3] = "handledC"
                n_gram_data[1,4] = "yes"
                n_gram_data[1,5] = "yes"
                n_gram_data[1,6] = "yes"
                n_gram_data[1,7] = "yes"
                n_gram_data[1,8] = "yes"
                rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                n_gram_data
              }
            }else if((nrow(hexa_grams)>0)==TRUE){ #works
              col_1_length = nrow(hexa_grams)
              col_1 = rep_len(segmented_words[1],col_1_length)
              col_2_length = nrow(hexa_grams)
              col_2 = rep_len(segmented_words[2],col_2_length)
              temp <- cbind(col_1, col_2)
              n_gram_data <- cbind(temp, hexa_grams)
              rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
              colnames(n_gram_data) <- c("eq7hexagrams>0", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth")
              n_gram_data[,1:8]
            }else{
              n_gram_data <- data.frame(first=character(),second=character(),
                                        third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(), eighth=character(), stringsAsFactors = FALSE)
              n_gram_data[1,1] = "this"
              n_gram_data[1,2] = "not"
              n_gram_data[1,3] = "?"
              n_gram_data[1,4] = "yes"
              n_gram_data[1,5] = "yes"
              n_gram_data[1,6] = "yes"
              n_gram_data[1,7] = "yes"
              n_gram_data[1,8] = "yes"
              rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
              n_gram_data
            }
          }else if((nrow(hepta_grams)>0)==TRUE){ #works
            col_1_length = nrow(hepta_grams)
            col_1 = rep_len(segmented_words[1],col_1_length)
            n_gram_data <- cbind(col_1, hepta_grams)
            rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
            colnames(n_gram_data) <- c("eq7heptagrams>0", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth")
            n_gram_data[,1:8]
          }else{
            n_gram_data <- data.frame(first=character(),second=character(),
                                      third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(), eighth=character(), stringsAsFactors = FALSE)
            n_gram_data[1,1] = "this"
            n_gram_data[1,2] = "not"
            n_gram_data[1,3] = "problem"
            n_gram_data[1,4] = "yes"
            n_gram_data[1,5] = "yes"
            n_gram_data[1,6] = "yes"
            n_gram_data[1,7] = "yes"
            n_gram_data[1,8] = "yes"
            rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
            n_gram_data
          }
        }else if((nrow(octo_grams)>0)==TRUE){ #works
          n_gram_data <- octo_grams
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          colnames(n_gram_data) <- c("eq7octograms>0", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth")
          n_gram_data[,1:8]
        }else{
          n_gram_data <- data.frame(first=character(),second=character(),
                                    third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(), eighth=character(), stringsAsFactors = FALSE)
          n_gram_data[1,1] = "this"
          n_gram_data[1,2] = "not"
          n_gram_data[1,3] = "problem"
          n_gram_data[1,4] = "yes"
          n_gram_data[1,5] = "yes"
          n_gram_data[1,6] = "yes"
          n_gram_data[1,7] = "yes"
          n_gram_data[1,8] = "yes"
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          n_gram_data
        }
        
      }else if((countWords(input$words_in)==8)==TRUE){
        nona_grams <- nonaGrams(input$data_source, input$words_in[1])
        if((nrow(nona_grams)==0)==TRUE){
          segmented_words = segmentWords(input$words_in)
          last_seven_cols = paste(unlist(segmented_words[2]), unlist(segmented_words[3]), unlist(segmented_words[4]),unlist(segmented_words[5]), unlist(segmented_words[6]), unlist(segmented_words[7])
                                  , unlist(segmented_words[8]))
          
          octo_grams <- octoGrams(input$data_source, last_seven_cols)
          if((nrow(octo_grams)==0)==TRUE){
            segmented_words = segmentWords(input$words_in)
            last_six_cols = paste(unlist(segmented_words[3]), unlist(segmented_words[4]),unlist(segmented_words[5]), unlist(segmented_words[6]), unlist(segmented_words[7])
                                  , unlist(segmented_words[8]))
            
            hepta_grams <- heptaGrams(input$data_source, last_six_cols)
            if((nrow(hepta_grams)==0)==TRUE){
              segmented_words = segmentWords(input$words_in)
              last_five_cols = paste(unlist(segmented_words[4]),unlist(segmented_words[5]), unlist(segmented_words[6]), unlist(segmented_words[7])
                                     , unlist(segmented_words[8]))
              hexa_grams <- hexaGrams(input$data_source, last_five_cols)
              if((nrow(hexa_grams)==0)==TRUE){
                segmented_words = segmentWords(input$words_in)
                last_four_cols = paste(unlist(segmented_words[5]), unlist(segmented_words[6]), unlist(segmented_words[7])
                                       , unlist(segmented_words[8]))
                penta_grams <- pentaGrams(input$data_source, last_four_cols)
                if((nrow(penta_grams)==0)==TRUE){
                  segmented_words = segmentWords(input$words_in)
                  last_three_cols = paste(unlist(segmented_words[6]), unlist(segmented_words[7]), unlist(segmented_words[8]))
                  quad_grams <- quadGrams(input$data_source, last_three_cols)
                  if((nrow(quad_grams)==0)==TRUE){
                    segmented_words = segmentWords(input$words_in)
                    last_two_cols = paste(unlist(segmented_words[7]), unlist(segmented_words[8]))
                    tri_grams = triGrams(input$data_source, last_two_cols)
                    if((nrow(tri_grams)==0)==TRUE){
                      segmented_words = segmentWords(input$words_in)
                      last_col = unlist(segmented_words[8])
                      bi_grams = biGrams(input$data_source, last_col)
                      if((nrow(bi_grams)==0)==TRUE){
                        n_gram_data <- data.frame(first=character(),second=character(),
                                                  third=character(), fourth=character(), fifth=character(),
                                                  sixth=character(), seventh=character(), eighth=character(), ninth=character(), stringsAsFactors = FALSE)
                        n_gram_data[1,1] = segmented_words[1]
                        n_gram_data[1,2] = segmented_words[2]
                        n_gram_data[1,3] = segmented_words[3]
                        n_gram_data[1,4] = segmented_words[4]
                        n_gram_data[1,5] = segmented_words[5]
                        n_gram_data[1,6] = segmented_words[6]
                        n_gram_data[1,7] = segmented_words[7]
                        n_gram_data[1,8] = segmented_words[8]
                        n_gram_data[1,9] = c("is")
                        rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                        n_gram_data
                      }else if((nrow(bi_grams)>0)==TRUE){
                        col_1_length = nrow(bi_grams)
                        col_1 = rep_len(segmented_words[1],col_1_length)
                        col_2_length = nrow(bi_grams)
                        col_2 = rep_len(segmented_words[2],col_2_length)
                        col_3_length = nrow(bi_grams)
                        col_3 = rep_len(segmented_words[3],col_3_length)
                        col_4_length = nrow(bi_grams)
                        col_4 = rep_len(segmented_words[4],col_4_length)
                        col_5_length = nrow(bi_grams)
                        col_5 = rep_len(segmented_words[5],col_5_length)
                        col_6_length = nrow(bi_grams)
                        col_6 = rep_len(segmented_words[6],col_6_length)
                        col_7_length = nrow(bi_grams)
                        col_7 = rep_len(segmented_words[7],col_7_length)
                        temp = cbind(col_1, col_2)
                        temp2 = cbind(temp, col_3)
                        temp3 = cbind(temp2, col_4)
                        temp4 = cbind(temp3, col_5)
                        temp5 = cbind(temp4, col_6)
                        temp6 = cbind(temp5, col_7)
                        n_gram_data <- cbind(temp6, bi_grams)
                        rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                        colnames(n_gram_data) <- c("eq8bigrams>0","second","third","fourth","fifth","sixth", "seventh", "eighth", "ninth")
                        n_gram_data[1:9]
                      }else{
                        n_gram_data <- data.frame(first=character(),second=character(),
                                                  third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(), eighth=character(),ninth=character(), stringsAsFactors = FALSE)
                        n_gram_data[1,1] = "this"
                        n_gram_data[1,2] = "not"
                        n_gram_data[1,3] = "handledA"
                        n_gram_data[1,4] = "yes"
                        n_gram_data[1,5] = "yes"
                        n_gram_data[1,6] = "yes"
                        n_gram_data[1,7] = "yes"
                        n_gram_data[1,8] = "yes"
                        n_gram_data[1,9] = "yes"
                        rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                        n_gram_data
                      }
                    }else if((nrow(tri_grams)>0)==TRUE){
                      col_1_length = nrow(tri_grams)
                      col_1 = rep_len(segmented_words[1],col_1_length)
                      col_2_length = nrow(tri_grams)
                      col_2 = rep_len(segmented_words[2],col_2_length)
                      col_3_length = nrow(tri_grams)
                      col_3 = rep_len(segmented_words[3],col_3_length)
                      col_4_length = nrow(tri_grams)
                      col_4 = rep_len(segmented_words[4],col_4_length)
                      col_5_length = nrow(tri_grams)
                      col_5 = rep_len(segmented_words[5],col_5_length)
                      col_6_length = nrow(tri_grams)
                      col_6 = rep_len(segmented_words[6],col_6_length)
                      temp = cbind(col_1, col_2)
                      temp2 = cbind(temp, col_3)
                      temp3 = cbind(temp2, col_4)
                      temp4 = cbind(temp3, col_5)
                      temp5 = cbind(temp4, col_6)
                      n_gram_data <- cbind(temp5, tri_grams)
                      rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                      colnames(n_gram_data) <- c("eq8trigrams>0","second","third","fourth","fifth", "sixth", "seventh", "eighth", "ninth")
                      n_gram_data
                    }else{
                      n_gram_data <- data.frame(first=character(),second=character(),
                                                third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(),  eighth=character(), ninth=character(), stringsAsFactors = FALSE)
                      n_gram_data[1,1] = "this"
                      n_gram_data[1,2] = "not"
                      n_gram_data[1,3] = "handledB"
                      n_gram_data[1,4] = "yes"
                      n_gram_data[1,5] = "yes"
                      n_gram_data[1,6] = "yes"
                      n_gram_data[1,7] = "yes"
                      n_gram_data[1,8] = "yes"
                      n_gram_data[1,9] = "yes"
                      rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                      n_gram_data
                    }
                  }else if((nrow(quad_grams)>0)==TRUE){
                    col_1_length = nrow(quad_grams)
                    col_1 = rep_len(segmented_words[1],col_1_length)
                    col_2_length = nrow(quad_grams)
                    col_2 = rep_len(segmented_words[2],col_2_length)
                    col_3_length = nrow(quad_grams)
                    col_3 = rep_len(segmented_words[3],col_3_length)
                    col_4_length = nrow(quad_grams)
                    col_4 = rep_len(segmented_words[4],col_4_length)
                    col_5_length = nrow(quad_grams)
                    col_5 = rep_len(segmented_words[5],col_5_length)
                    temp = cbind(col_1, col_2)
                    temp2 = cbind(temp, col_3)
                    temp3 = cbind(temp2, col_4)
                    temp4 = cbind(temp3, col_5)
                    n_gram_data <- cbind(temp4, quad_grams)
                    rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                    colnames(n_gram_data) <- c("eq8quadgrams>0","second","third","fourth","fifth", "sixth", "seventh", "eighth", "ninth")
                    n_gram_data
                  }else{
                    n_gram_data <- data.frame(first=character(),second=character(),
                                              third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(), eighth=character(), ninth=character(), stringsAsFactors = FALSE)
                    n_gram_data[1,1] = "this"
                    n_gram_data[1,2] = "not"
                    n_gram_data[1,3] = "handledC"
                    n_gram_data[1,4] = "yes"
                    n_gram_data[1,5] = "yes"
                    n_gram_data[1,6] = "yes"
                    n_gram_data[1,7] = "yes"
                    n_gram_data[1,8] = "yes"
                    n_gram_data[1,9] = "yes"
                    rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                    n_gram_data
                  }
                }else if((nrow(penta_grams)>0)==TRUE){
                  
                  #       dat <- data.frame(x = numeric(0), y = numeric(0))
                  #        withProgress(message = 'Making plot', value = 0, {
                  #          # Number of times we'll go through the loop
                  #          n <- 30
                  #          
                  #          for (i in 1:n) {
                  #            # Each time through the loop, add another row of data. This is
                  #            # a stand-in for a long-running computation.
                  #            dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
                  #            
                  #            # Increment the progress bar, and update the detail text.
                  #            incProgress(1/n, detail = paste("Doing part", i))
                  #            
                  #            # Pause for 0.1 seconds to simulate a long computation.
                  #            Sys.sleep(1)
                  
                  #         }
                  #       }
                  #        )
                  
                  
                  col_1_length = nrow(penta_grams)
                  col_1 = rep_len(segmented_words[1],col_1_length)
                  col_2_length = nrow(penta_grams)
                  col_2 = rep_len(segmented_words[2],col_2_length)
                  col_3_length = nrow(penta_grams)
                  col_3 = rep_len(segmented_words[3],col_3_length)
                  col_4_length = nrow(penta_grams)
                  col_4 = rep_len(segmented_words[4],col_4_length)
                  temp = cbind(col_1, col_2)
                  temp2 = cbind(temp, col_3)
                  temp3 = cbind(temp2, col_4)
                  n_gram_data <- cbind(temp3, penta_grams)
                  rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                  colnames(n_gram_data) <- c("e8pentagrams>0","second","third","fourth","fifth","sixth","seventh", "eighth", "ninth")
                  n_gram_data
                }else{
                  n_gram_data <- data.frame(first=character(),second=character(),
                                            third=character(), fourth=character(), fifth=character(),
                                            sixth=character(), seventh=character(), eighth=character(), ninth=character(), stringsAsFactors = FALSE)
                  n_gram_data[1,1] = "this"
                  n_gram_data[1,2] = "not"
                  n_gram_data[1,3] = "handledC"
                  n_gram_data[1,4] = "yes"
                  n_gram_data[1,5] = "yes"
                  n_gram_data[1,6] = "yes"
                  n_gram_data[1,7] = "yes"
                  n_gram_data[1,8] = "yes"
                  n_gram_data[1,9] = "yes"
                  rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                  n_gram_data
                }
              }else if((nrow(hexa_grams)>0)==TRUE){
                col_1_length = nrow(hexa_grams)
                col_1 = rep_len(segmented_words[1],col_1_length)
                col_2_length = nrow(hexa_grams)
                col_2 = rep_len(segmented_words[2],col_2_length)
                col_3_length = nrow(hexa_grams)
                col_3 = rep_len(segmented_words[3],col_3_length)
                temp <- cbind(col_1, col_2)
                temp2 <- cbind(temp, col_3)
                n_gram_data <- cbind(temp2, hexa_grams)
                rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                colnames(n_gram_data) <- c("eq8hexagrams>0", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth")
                n_gram_data[,1:9]
              }else{
                n_gram_data <- data.frame(first=character(),second=character(),
                                          third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(), eighth=character(), ninth=character(),  stringsAsFactors = FALSE)
                n_gram_data[1,1] = "this"
                n_gram_data[1,2] = "not"
                n_gram_data[1,3] = "yes"
                n_gram_data[1,4] = "yes"
                n_gram_data[1,5] = "yes"
                n_gram_data[1,6] = "yes"
                n_gram_data[1,7] = "yes"
                n_gram_data[1,8] = "yes"
                n_gram_data[1,9] = "yes"
                rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                n_gram_data
              }
            }else if((nrow(hepta_grams)>0)==TRUE){
              col_1_length = nrow(hepta_grams)
              col_1 = rep_len(segmented_words[1],col_1_length)
              col_2_length = nrow(hepta_grams)
              col_2 = rep_len(segmented_words[2],col_2_length)
              temp <- cbind(col_1, col_2)
              n_gram_data <- cbind(temp, hepta_grams)
              rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
              colnames(n_gram_data) <- c("eq8heptagrams>0", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth")
              n_gram_data[,1:9]
            }else{
              n_gram_data <- data.frame(first=character(),second=character(),
                                        third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(), eighth=character(), ninth=character(), stringsAsFactors = FALSE)
              n_gram_data[1,1] = "this"
              n_gram_data[1,2] = "not"
              n_gram_data[1,3] = "problem"
              n_gram_data[1,4] = "yes"
              n_gram_data[1,5] = "yes"
              n_gram_data[1,6] = "yes"
              n_gram_data[1,7] = "yes"
              n_gram_data[1,8] = "yes"
              n_gram_data[1,9] = "yes"
              rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
              n_gram_data
            }
          }else if((nrow(octo_grams)>0)==TRUE){
            col_1_length = nrow(octo_grams)
            col_1 = rep_len(segmented_words[1],col_1_length)
            n_gram_data <- cbind(col_1, octo_grams)
            rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
            colnames(n_gram_data) <- c("eq8octograms>0", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth")
            n_gram_data[,1:9]
          }else{
            n_gram_data <- data.frame(first=character(),second=character(),
                                      third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(), eighth=character(), ninth=character(), stringsAsFactors = FALSE)
            n_gram_data[1,1] = "this"
            n_gram_data[1,2] = "not"
            n_gram_data[1,3] = "problem"
            n_gram_data[1,4] = "yes"
            n_gram_data[1,5] = "yes"
            n_gram_data[1,6] = "yes"
            n_gram_data[1,7] = "yes"
            n_gram_data[1,8] = "yes"
            n_gram_data[1,9] = "yes"
            rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
            n_gram_data
          }
          
        }else if((nrow(nona_grams)>0)==TRUE){ #works
          n_gram_data <- nona_grams
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          colnames(n_gram_data) <- c("eq8nonagrams>0", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth")
          n_gram_data[,1:9]
          
        }else{
          n_gram_data <- data.frame(first=character(),second=character(),
                                    third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(), eighth=character(), ninth=character(), stringsAsFactors = FALSE)
          n_gram_data[1,1] = "this"
          n_gram_data[1,2] = "not"
          n_gram_data[1,3] = "problem"
          n_gram_data[1,4] = "yes"
          n_gram_data[1,5] = "yes"
          n_gram_data[1,6] = "yes"
          n_gram_data[1,7] = "yes"
          n_gram_data[1,8] = "yes"
          n_gram_data[1,9] = "yes"
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          n_gram_data
          
        }
        
        
      }else{
        n_gram_data <- data.frame(first=character(),second=character(), stringsAsFactors = FALSE)
        n_gram_data[1,1] = c("Please")
        n_gram_data[1,2] = c("enter")
        n_gram_data[1,3] = c("words")
      }, options = list(lengthChange = FALSE))
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #####################################################
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    output$line = renderText({
      
      if((countWords(input$words_in)>8)==TRUE){
        problem <- paste("Please","enter","eight", "or", "less", "words")
        n_gram_data <- problem
      }else if((countWords(input$words_in)==0)==TRUE){
        problem <- paste("Please","enter","words")
        n_gram_data <- problem
      }else if((countWords(input$words_in)==1)==TRUE){
        bi_grams <- biGrams(input$data_source, input$words_in)
        if((nrow(bi_grams)==0)==TRUE){
          n_gram_data <- paste(input$words_in,"is")
          n_gram_data
        }else if((nrow(bi_grams)>0)==TRUE){
          n_gram_data <- paste(bi_grams[1,1],bi_grams[1,2])
          n_gram_data
        }else{
          n_gram_data <- paste("not","handled15")
          n_gram_data
        }
      }else if((countWords(input$words_in)==2)==TRUE){
        tri_grams <- triGrams(input$data_source, input$words_in)
        if((nrow(tri_grams)==0)==TRUE){
          segmented_words = segmentWords(input$words_in)
          to_drive_bigrams = unlist(segmented_words[2])
          bi_grams = biGrams(input$data_source, to_drive_bigrams)
          if((nrow(bi_grams)==0)==TRUE){
            first = as.character(segmented_words[1])
            second = as.character(segmented_words[2])
            third = "is"
            n_gram_data = paste(first,second,third)
            n_gram_data
          }else if((nrow(bi_grams)>0)==TRUE){
            first = as.character(segmented_words[1])
            second = as.character(segmented_words[2])
            third = as.character(bi_grams[1,2])
            n_gram_data = paste(first,second,third)
            n_gram_data
          }else{
            n_gram_data <- paste("not","handled16")
            n_gram_data
          }
        }else if((nrow(tri_grams)>0)==TRUE){
          n_gram_data <- paste(tri_grams[1,1],tri_grams[1,2], tri_grams[1,3])
          n_gram_data
        }else{
          n_gram_data <- paste("not","handled17")
          n_gram_data
        }
      }else if((countWords(input$words_in)==3)==TRUE){
        quad_grams <- quadGrams(input$data_source, input$words_in)
        segmented_words = segmentWords(input$words_in)
        if((nrow(quad_grams)==0)==TRUE){
          to_drive_trigrams = paste(unlist(segmented_words[2]), unlist(segmented_words[3]))
          tri_grams = triGrams(input$data_source, to_drive_trigrams)
          if((nrow(tri_grams)==0)==TRUE){
            #segmented_words = segmentWords(input$words_in)
            to_drive_bigrams = unlist(segmented_words[3])
            bi_grams = biGrams(input$data_source, to_drive_bigrams)
            if((nrow(bi_grams)==0)==TRUE){
              n_gram_data <- paste(segmented_words[1], segmented_words[2], segmented_words[3], "is")
              n_gram_data
            }else if((nrow(bi_grams)>0)==TRUE){
              first = as.character(segmented_words[1])
              second = as.character(segmented_words[2])
              third = as.character(segmented_words[3])
              fourth = as.character(bi_grams[1,2])
              n_gram_data = paste(first, second, third, fourth)
              n_gram_data
            }else{
              n_gram_data <- paste("not","handled18")
              n_gram_data
            }
          }else if((nrow(tri_grams)>0)==TRUE){
            #segmented_words = segmentWords(input$words_in)
            first = as.character(segmented_words[1])
            n_gram_data <- paste(first, tri_grams[1,1],tri_grams[1,2], tri_grams[1,3])
            n_gram_data
          }else{
            n_gram_data <- paste("not","handled19")
            n_gram_data
          }
        }else if((nrow(quad_grams)>0)==TRUE){
          n_gram_data <- paste(quad_grams[1,1],quad_grams[1,2], quad_grams[1,3], quad_grams[1,4])
          n_gram_data
        }else{
          n_gram_data <- paste("not","handled20")
          n_gram_data
        }
      }else if((countWords(input$words_in)==4)==TRUE){
        penta_grams <- pentaGrams(input$data_source, input$words_in)
        segmented_words = segmentWords(input$words_in)
        if((nrow(penta_grams)==0)==TRUE){
          to_drive_quadgrams = paste(unlist(segmented_words[2]), unlist(segmented_words[3]), unlist(segmented_words[4]))
          quad_grams <- quadGrams(input$data_source, to_drive_quadgrams)
          if((nrow(quad_grams)==0)==TRUE){
            segmented_words = segmentWords(input$words_in)
            to_drive_trigrams = paste(unlist(segmented_words[3]), unlist(segmented_words[4]))
            tri_grams = triGrams(input$data_source, to_drive_trigrams)
            if((nrow(tri_grams)==0)==TRUE){
              segmented_words = segmentWords(input$words_in)
              to_drive_bigrams = unlist(segmented_words[4])
              bi_grams = biGrams(input$data_source, to_drive_bigrams)
              if((nrow(bi_grams)==0)==TRUE){
                first = as.character(segmented_words[1])
                second = as.character(segmented_words[2])
                third = as.character(segmented_words[3])
                fourth = as.character(segmented_words[4])
                fifth = "is"
                n_gram_data = paste(first, second, third, fourth, fifth)
              }else if((nrow(bi_grams)>0)==TRUE){
                n_gram_data <- paste(bi_grams[1,1],bi_grams[1,2])
                segmented_words_2 = segmentWords(n_gram_data)
                first = as.character(segmented_words[1])
                second = as.character(segmented_words[2])
                third = as.character(segmented_words[3])
                fourth = as.character(segmented_words_2[1])
                fifth = as.character(segmented_words_2[2])
                n_gram_data = paste(first, second, third, fourth, fifth)
              }else{
                n_gram_data <- paste("not","handled21","yet")
                n_gram_data
              }
            }else if((nrow(tri_grams)>0)==TRUE){
              n_gram_data <- paste(tri_grams[1,1],tri_grams[1,2], tri_grams[1,3])
              n_gram_data = paste(segmented_words[1], segmented_words[2], n_gram_data)
              n_gram_data
            }else{
              n_gram_data <- paste("not","handled22","yet")
              n_gram_data
            }
          }else if((nrow(quad_grams)>0)==TRUE){
            n_gram_data <- paste(quad_grams[1,1], quad_grams[1,2], quad_grams[1,3], quad_grams[1,4])
            n_gram_data = paste(segmented_words[1],n_gram_data)
            n_gram_data
          }else{
            n_gram_data <- paste("not","handled23","yet")
            n_gram_data
          }
        }else if((nrow(penta_grams)>0)==TRUE){
          n_gram_data = paste(penta_grams[1,1], penta_grams[1,2], penta_grams[1,3], penta_grams[1,4], penta_grams[1,5])
          n_gram_data
        }else{
          n_gram_data <- paste("not","handled24","yet")
          n_gram_data
        }
      }else if((countWords(input$words_in)==5)==TRUE){
        hexa_grams <- hexaGrams(input$data_source, input$words_in[1])
        segmented_words = segmentWords(input$words_in)
        if((nrow(hexa_grams)==0)==TRUE){
          to_drive_pentagrams = paste(unlist(segmented_words[2]), unlist(segmented_words[3]), unlist(segmented_words[4])
                                      , unlist(segmented_words[5]))
          penta_grams <- pentaGrams(input$data_source, to_drive_pentagrams)
          if((nrow(penta_grams)==0)==TRUE){
            to_drive_quadgrams = paste(unlist(segmented_words[3]), unlist(segmented_words[4]), unlist(segmented_words[5]))
            quad_grams <- quadGrams(input$data_source, to_drive_quadgrams)
            if((nrow(quad_grams)==0)==TRUE){
              to_drive_trigrams = paste(unlist(segmented_words[4]), unlist(segmented_words[5]))
              tri_grams = triGrams(input$data_source, to_drive_trigrams)
              if((nrow(tri_grams)==0)==TRUE){
                to_drive_bigrams = unlist(segmented_words[5])
                bi_grams = biGrams(input$data_source, to_drive_bigrams)
                if((nrow(bi_grams)==0)==TRUE){
                  first = as.character(segmented_words[1])
                  second = as.character(segmented_words[2])
                  third = as.character(segmented_words[3])
                  fourth = as.character(segmented_words[4])
                  fifth = as.character(segmented_words[5])
                  sixth = "is"
                  n_gram_data = paste(first, second, third, fourth, fifth, sixth)
                }else if((nrow(bi_grams)>0)==TRUE){
                  n_gram_data <- paste(bi_grams[1,1],bi_grams[1,2])
                  segmented_words_2 = segmentWords(n_gram_data)
                  first = as.character(segmented_words[1])
                  second = as.character(segmented_words[2])
                  third = as.character(segmented_words[3])
                  fourth = as.character(segmented_words[4])
                  fifth = as.character(segmented_words_2[1])
                  sixth = as.character(segmented_words_2[2])
                  n_gram_data = paste(first, second, third, fourth, fifth, sixth)
                  
                }else{
                  n_gram_data <- paste("Error number #.")
                  n_gram_data
                }
              }else if((nrow(tri_grams)>0)==TRUE){
                n_gram_data <- paste(tri_grams[1,1],tri_grams[1,2], tri_grams[1,3])
                n_gram_data = paste(segmented_words[1], segmented_words[2], segmented_words[3],n_gram_data)
                n_gram_data
                
              }else{
                n_gram_data <- paste("Error number #.")
                n_gram_data
              }
            }else if((nrow(quad_grams)>0)==TRUE){ #works
              n_gram_data <- paste(quad_grams[1,1], quad_grams[1,2], quad_grams[1,3], quad_grams[1,4])
              n_gram_data = paste(segmented_words[1],segmented_words[2], n_gram_data)
              n_gram_data
            }else{
              n_gram_data <- paste("Error number #.")
              n_gram_data
            }
          }else if((nrow(penta_grams)>0)==TRUE){
            n_gram_data =  paste(segmented_words[1],penta_grams[1,1], penta_grams[1,2], penta_grams[1,3], penta_grams[1,4], penta_grams[1,5])
            n_gram_data
          }else{
            n_gram_data <- paste("Error number #.")
            n_gram_data
          }
        }else if((nrow(hexa_grams)>0)==TRUE){ #works
          n_gram_data <- paste(hexa_grams[1,1], hexa_grams[1,2], hexa_grams[1,3], hexa_grams[1,4], hexa_grams[1,5], hexa_grams[1,6])
          n_gram_data
        }else{
          n_gram_data <- paste("Error number #.")
          n_gram_data
        }
      }else if((countWords(input$words_in)==6)==TRUE){
        hepta_grams <- heptaGrams(input$data_source, input$words_in[1])
        segmented_words = segmentWords(input$words_in)
        if((nrow(hepta_grams)==0)==TRUE){
          to_drive_hexagrams = paste(unlist(segmented_words[2]),unlist(segmented_words[3]), unlist(segmented_words[4]), unlist(segmented_words[5])
                                     , unlist(segmented_words[6]))
          hexa_grams <- hexaGrams(input$data_source, to_drive_hexagrams)
          segmented_words = segmentWords(input$words_in)
          if((nrow(hexa_grams)==0)==TRUE){
            to_drive_pentagrams = paste(unlist(segmented_words[3]), unlist(segmented_words[4]), unlist(segmented_words[5])
                                        , unlist(segmented_words[6]))
            penta_grams <- pentaGrams(input$data_source, to_drive_pentagrams)
            if((nrow(penta_grams)==0)==TRUE){
              to_drive_quadgrams = paste(unlist(segmented_words[4]), unlist(segmented_words[5]), unlist(segmented_words[6]))
              quad_grams <- quadGrams(input$data_source, to_drive_quadgrams)
              if((nrow(quad_grams)==0)==TRUE){
                to_drive_trigrams = paste(unlist(segmented_words[5]), unlist(segmented_words[6]))
                tri_grams = triGrams(input$data_source, to_drive_trigrams)
                if((nrow(tri_grams)==0)==TRUE){
                  to_drive_bigrams = unlist(segmented_words[6])
                  bi_grams = biGrams(input$data_source, to_drive_bigrams)
                  if((nrow(bi_grams)==0)==TRUE){
                    first = as.character(segmented_words[1])
                    second = as.character(segmented_words[2])
                    third = as.character(segmented_words[3])
                    fourth = as.character(segmented_words[4])
                    fifth = as.character(segmented_words[5])
                    sixth = as.character(segmented_words[6])
                    seventh = "is"
                    n_gram_data = paste(first, second, third, fourth, fifth, sixth, seventh)
                  }else if((nrow(bi_grams)>0)==TRUE){
                    n_gram_data <- paste(bi_grams[1,1],bi_grams[1,2])
                    segmented_words_2 = segmentWords(n_gram_data)
                    first = as.character(segmented_words[1])
                    second = as.character(segmented_words[2])
                    third = as.character(segmented_words[3])
                    fourth = as.character(segmented_words[4])
                    fifth = as.character(segmented_words[5])
                    sixth = as.character(segmented_words_2[1])
                    seventh = as.character(segmented_words_2[2])
                    n_gram_data = paste(first, second, third, fourth, fifth, sixth, seventh)
                    
                  }else{
                    n_gram_data <- paste("Error number #.")
                    n_gram_data
                  }
                }else if((nrow(tri_grams)>0)==TRUE){
                  n_gram_data <- paste(tri_grams[1,1],tri_grams[1,2], tri_grams[1,3])
                  n_gram_data = paste(segmented_words[1], segmented_words[2], segmented_words[3], segmented_words[4], n_gram_data)
                  n_gram_data
                  
                }else{
                  n_gram_data <- paste("Error number #.")
                  n_gram_data
                }
              }else if((nrow(quad_grams)>0)==TRUE){ #works
                n_gram_data <- paste(quad_grams[1,1], quad_grams[1,2], quad_grams[1,3], quad_grams[1,4])
                n_gram_data = paste(segmented_words[1],segmented_words[2],segmented_words[3], n_gram_data)
                n_gram_data
              }else{
                n_gram_data <- paste("Error number #.")
                n_gram_data
              }
            }else if((nrow(penta_grams)>0)==TRUE){
              n_gram_data =  paste(segmented_words[1],penta_grams[1,1], penta_grams[1,2], penta_grams[1,3], penta_grams[1,4], penta_grams[1,5])
              n_gram_data = paste(segmented_words[1],segmented_words[2], n_gram_data)
              n_gram_data
            }else{
              n_gram_data <- paste("Error number #.")
              n_gram_data
            }
          }else if((nrow(hexa_grams)>0)==TRUE){
            n_gram_data <- paste(hexa_grams[1,1], hexa_grams[1,2], hexa_grams[1,3], hexa_grams[1,4], hexa_grams[1,5], hexa_grams[1,6])
            n_gram_data = paste(segmented_words[1], n_gram_data)
            n_gram_data
          }else{
            n_gram_data <- paste("Error number #.")
            n_gram_data
          }
        }else if((nrow(hepta_grams)>0)==TRUE){
          n_gram_data <- paste(hepta_grams[1,1], hepta_grams[1,2], hepta_grams[1,3], hepta_grams[1,4], hepta_grams[1,5], hepta_grams[1,6], hepta_grams[1,7])
          n_gram_data
        }else{
          n_gram_data <- paste("Error number #.")
          n_gram_data
          
        }
      }else if((countWords(input$words_in)==7)==TRUE){
        octo_grams <- octoGrams(input$data_source, input$words_in[1])
        segmented_words = segmentWords(input$words_in)
        if((nrow(octo_grams)==0)==TRUE){
          to_drive_hepta_grams = paste(unlist(segmented_words[2]), unlist(segmented_words[3]), unlist(segmented_words[4])
                                       , unlist(segmented_words[5],  unlist(segmented_words[6])))
          
          if((nrow(hepta_grams)==0)==TRUE){
            to_drive_hexagrams = paste(unlist(segmented_words[3]), unlist(segmented_words[4]), unlist(segmented_words[5])
                                       , unlist(segmented_words[6]))
            hexa_grams <- hexaGrams(input$data_source, to_drive_hexagrams)
            segmented_words = segmentWords(input$words_in)
            if((nrow(hexa_grams)==0)==TRUE){
              to_drive_pentagrams = paste(unlist(segmented_words[3]), unlist(segmented_words[4]), unlist(segmented_words[5])
                                          , unlist(segmented_words[6]))
              penta_grams <- pentaGrams(input$data_source, to_drive_pentagrams)
              if((nrow(penta_grams)==0)==TRUE){
                to_drive_quadgrams = paste(unlist(segmented_words[4]), unlist(segmented_words[5]), unlist(segmented_words[6]))
                quad_grams <- quadGrams(input$data_source, to_drive_quadgrams)
                if((nrow(quad_grams)==0)==TRUE){
                  to_drive_trigrams = paste(unlist(segmented_words[5]), unlist(segmented_words[6]))
                  tri_grams = triGrams(input$data_source, to_drive_trigrams)
                  if((nrow(tri_grams)==0)==TRUE){
                    to_drive_bigrams = unlist(segmented_words[6])
                    bi_grams = biGrams(input$data_source, to_drive_bigrams)
                    if((nrow(bi_grams)==0)==TRUE){
                      first = as.character(segmented_words[1])
                      second = as.character(segmented_words[2])
                      third = as.character(segmented_words[3])
                      fourth = as.character(segmented_words[4])
                      fifth = as.character(segmented_words[5])
                      sixth = as.character(segmented_words[6])
                      seventh = "is"
                      n_gram_data = paste(first, second, third, fourth, fifth, sixth, seventh)
                    }else if((nrow(bi_grams)>0)==TRUE){
                      n_gram_data <- paste(bi_grams[1,1],bi_grams[1,2])
                      segmented_words_2 = segmentWords(n_gram_data)
                      first = as.character(segmented_words[1])
                      second = as.character(segmented_words[2])
                      third = as.character(segmented_words[3])
                      fourth = as.character(segmented_words[4])
                      fifth = as.character(segmented_words[4])
                      sixth = as.character(segmented_words_2[1])
                      seventh = as.character(segmented_words_2[2])
                      n_gram_data = paste(first, second, third, fourth, fifth, sixth, seventh)
                      
                    }else{
                      n_gram_data <- paste("Error number #.")
                      n_gram_data
                    }
                  }else if((nrow(tri_grams)>0)==TRUE){
                    n_gram_data <- paste(tri_grams[1,1],tri_grams[1,2], tri_grams[1,3])
                    n_gram_data = paste(segmented_words[1], segmented_words[2], segmented_words[3], segmented_words[4], n_gram_data)
                    n_gram_data
                    
                  }else{
                    n_gram_data <- paste("Error number #.")
                    n_gram_data
                  }
                }else if((nrow(quad_grams)>0)==TRUE){ 
                  n_gram_data <- paste(quad_grams[1,1], quad_grams[1,2], quad_grams[1,3], quad_grams[1,4])
                  n_gram_data = paste(segmented_words[1],segmented_words[2], segmented_words[3],n_gram_data)
                  n_gram_data
                }else{
                  n_gram_data <- paste("Error number #.")
                  n_gram_data
                }
              }else if((nrow(penta_grams)>0)==TRUE){
                n_gram_data =  paste(segmented_words[1],penta_grams[1,1], penta_grams[1,2], penta_grams[1,3], penta_grams[1,4], penta_grams[1,5])
                n_gram_data = paste(segmented_words[1],segmented_words[2], n_gram_data)
                n_gram_data
              }else{
                n_gram_data <- paste("Error number #.")
                n_gram_data
              }
            }else if((nrow(hexa_grams)>0)==TRUE){
              n_gram_data <- paste(hexa_grams[1,1], hexa_grams[1,2], hexa_grams[1,3], hexa_grams[1,4], hexa_grams[1,5], hexa_grams[1,6])
              n_gram_data = paste(segmented_words[1],n_gram_data)
              n_gram_data
            }else{
              n_gram_data <- paste("Error number #.")
              n_gram_data
            }
          }else if((nrow(hepta_grams)>0)==TRUE){
            n_gram_data <- paste(hepta_grams[1,1], hepta_grams[1,2], hepta_grams[1,3], hepta_grams[1,4], hepta_grams[1,5], hepta_grams[1,6], hepta_grams[1,7])
            n_gram_data
          }else{
            n_gram_data <- paste("Error number #.")
            n_gram_data
            
          }
        }else if((nrow(octo_grams)>0)==TRUE){
          n_gram_data <- paste(octo_grams[1,1], octo_grams[1,2], octo_grams[1,3], octo_grams[1,4], octo_grams[1,5], octo_grams[1,6], octo_grams[1,7], octo_grams[1,8])
          n_gram_data
          
        }else{
          n_gram_data <- paste("Error number #.")
          n_gram_data
          
        }  
      }
      
      ##end of renderText function below
      
    }
    )
    
    #shiny server function
    
  }
  )
  
#}

biGrams <- function(data_source, word){
  #word = tolower(word)
  if((data_source=="blogs")==TRUE){
    blogs2 <- blogs2[which(blogs2$first==word),]
    blogs2<-orderBy(~-count, data=blogs2)
    blogs2sum <- sum(blogs2$count)
    blogs2$frequency<-blogs2$count / blogs2sum
    n_gram_data <- blogs2
  }else if(data_source=="news"){
    news2 <- news2[which(news2$first==word),]
    news2<-orderBy(~-count, data=news2)
    news2sum <- sum(news2$count)
    news2$frequency<-news2$count / news2sum
    n_gram_data <- news2
  }else if(data_source=="twitter"){
    twitter2 <- twitter2[which(twitter2$first==word),]
    twitter2<-orderBy(~-count, data=twitter2)
    twitter2sum <- sum(twitter2$count)
    twitter2$frequency<-twitter2$count / twitter2sum
    n_gram_data <- twitter2
  }else{
    n_gram_data <- data.frame(first=character(),second=character(), stringsAsFactors = FALSE)
    n_gram_data[1,1] = "not"
    n_gram_data[1,2] = "handled1"
    n_gram_data
  }
  rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
  n_gram_data[,1:2]
}

triGrams <- function(data_source, wordst){
  search_words = segmentWords(wordst)
  #search_words <- tolower(search_words)
  if((data_source=="blogs")==TRUE){
    blogs3 <- blogs3[which(blogs3$first==search_words[1] & blogs3$second==search_words[2]),]
    blogs3<-orderBy(~-count, data=blogs3)
    blogs3sum <- sum(blogs3$count)
    blogs3$frequency<-blogs3$count / blogs3sum
    n_gram_data <- blogs3
  }else if((data_source=="news")==TRUE){
    news3 <- news3[which(news3$first==search_words[1] & news3$second==search_words[2]),]
    news3<-orderBy(~-count, data=news3)
    news3sum <- sum(news3$count)
    news3$frequency<-news3$count / news3sum
    n_gram_data <- news3
  }else if((data_source=="twitter")==TRUE){
    twitter3 <- twitter3[which(twitter3$first==search_words[1] & twitter3$second==search_words[2]),]
    twitter3<-orderBy(~-count, data=twitter3)
    twitter3sum <- sum(twitter3$count)
    twitter3$frequency<-twitter3$count / twitter3sum
    n_gram_data <- twitter3
  }else{
    n_gram_data <- data.frame(first=character(),second=character(), third=character(), stringsAsFactors = FALSE)
    n_gram_data[1,1] = c("not")
    n_gram_data[1,2] = c("handled2")
    n_gram_data[1,3] = c("yet")
    n_gram_data
  }
  rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
  n_gram_data[,1:3]
}

quadGrams <- function(data_source, wordsq){
  search_words = segmentWords(wordsq)
  #search_words = tolower(search_words)
  if((data_source=="blogs")==TRUE){
    blogs4 <- blogs4[which(blogs4$first==search_words[1] & blogs4$second==search_words[2] & blogs4$third==search_words[3]),]
    blogs4<-orderBy(~-count, data=blogs4)
    blogs4sum <- sum(blogs4$count)
    blogs4$frequency<-blogs4$count / blogs4sum
    n_gram_data <- blogs4
  }else if((data_source=="news")==TRUE){
    news4 <- news4[which(news4$first==search_words[1] & news4$second==search_words[2] & news4$third==search_words[3]),]
    news4<-orderBy(~-count, data=news4)
    news4sum <- sum(news4$count)
    news4$frequency<-news4$count / news4sum
    n_gram_data <- news4
  }else if((data_source=="twitter")==TRUE){
    twitter4 <- twitter4[which(twitter4$first==search_words[1] & twitter4$second==search_words[2] & twitter4$third==search_words[3]),]
    twitter4<-orderBy(~-count, data=twitter4)
    twitter4sum <- sum(twitter4$count)
    twitter4$frequency<-twitter4$count / twitter4sum
    n_gram_data <- twitter4
  }else{
    n_gram_data <- data.frame(first=character(),second=character(), third=character(), fourth=character(), stringsAsFactors = FALSE)
    n_gram_data[1,1] = c("not")
    n_gram_data[1,2] = c("handled3")
    n_gram_data[1,3] = c("yet")
    n_gram_data[1,4] = c("really")
    n_gram_data
  }
  rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
  n_gram_data[,1:4]
}

pentaGrams <- function(data_source, wordsp){
  search_words = segmentWords(wordsp)
  #search_words = tolower(search_words)
  if((data_source=="blogs")==TRUE){
    blogs5 <- blogs5[which(blogs5$first==search_words[1] & blogs5$second==search_words[2] & blogs5$third==search_words[3]& blogs5$fourth==search_words[4]),]
    blogs5<-orderBy(~-count, data=blogs5)
    blogs5sum <- sum(blogs5$count)
    blogs5$frequency<-blogs5$count / blogs5sum
    n_gram_data <- blogs5
  }else if((data_source=="news")==TRUE){
    news5 <- news5[which(news5$first==search_words[1] & news5$second==search_words[2] & news5$third==search_words[3]& news5$fourth==search_words[4]),]
    news5<-orderBy(~-count, data=news5)
    news5sum <- sum(news5$count)
    news5$frequency<-news5$count / news5sum
    n_gram_data <- news5
  }else if((data_source=="twitter")==TRUE){
    twitter5 <- twitter5[which(twitter5$first==search_words[1] & twitter5$second==search_words[2] & twitter5$third==search_words[3]& twitter5$fourth==search_words[4]),]
    twitter5<-orderBy(~-count, data=twitter5)
    twitter5sum <- sum(twitter5$count)
    twitter5$frequency<-twitter5$count / twitter5sum
    n_gram_data <- twitter5
  }else{
    n_gram_data <- data.frame(first=character(),second=character(), third=character(), fourth=character(), fifth=character(), stringsAsFactors = FALSE)
    n_gram_data[1,1] = c("not")
    n_gram_data[1,2] = c("handled4")
    n_gram_data[1,3] = c("yet")
    n_gram_data[1,4] = c("really")
    n_gram_data[1,5] = c("uhhuh")
    n_gram_data
  }
  rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
  n_gram_data[,1:5]
}

hexaGrams <- function(data_source, wordsh){
  search_words = segmentWords(wordsh)
  #search_words = tolower(search_words)
  if((data_source=="blogs")==TRUE){
    blogs6 <- blogs6[which(blogs6$first==search_words[1] & blogs6$second==search_words[2] & blogs6$third==search_words[3]& blogs6$fourth==search_words[4] & blogs6$fifth==search_words[5]),]
    blogs6<-orderBy(~-count, data=blogs6)
    blogs6sum <- sum(blogs6$count)
    blogs6$frequency<-blogs6$count / blogs6sum
    n_gram_data <- blogs6
  }else if((data_source=="news")==TRUE){
    news6 <- news6[which(news6$first==search_words[1] & news6$second==search_words[2] & news6$third==search_words[3]& news6$fourth==search_words[4] & news6$fifth==search_words[5]),]
    news6<-orderBy(~-count, data=news6)
    news6sum <- sum(news6$count)
    news6$frequency<-news6$count / news6sum
    n_gram_data <- news6
  }else if((data_source=="twitter")==TRUE){
    twitter6 <- twitter6[which(twitter6$first==search_words[1] & twitter6$second==search_words[2] & twitter6$third==search_words[3]& twitter6$fourth==search_words[4] & twitter6$fifth==search_words[5]),]
    twitter6<-orderBy(~-count, data=twitter6)
    twitter6sum <- sum(twitter6$count)
    twitter6$frequency<-twitter6$count / twitter6sum
    n_gram_data <- twitter6
  }else{
    n_gram_data <- data.frame(first=character(),second=character(), third=character(), fourth=character(), fifth=character(), sixth=character(), stringsAsFactors = FALSE)
    n_gram_data[1,1] = c("not")
    n_gram_data[1,2] = c("handled4")
    n_gram_data[1,3] = c("yet")
    n_gram_data[1,4] = c("really")
    n_gram_data[1,5] = c("uhhuh")
    n_gram_data[1,6] = c("uhhuh")
    n_gram_data
  }
  rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
  n_gram_data[,1:6]
}

heptaGrams <- function(data_source, words7){
  search_words = segmentWords(words7)
  #search_words = tolower(search_words)
  if((data_source=="blogs")==TRUE){
    blogs7 <- blogs7[which(blogs7$first==search_words[1] & blogs7$second==search_words[2] & blogs7$third==search_words[3] & blogs7$fourth==search_words[4]& blogs7$fifth==search_words[5] & blogs7$sixth==search_words[6]),]
    blogs7<-orderBy(~-count, data=blogs7)
    blogs7sum <- sum(blogs7$count)
    blogs7$frequency<-blogs7$count / blogs7sum
    n_gram_data <- blogs7
  }else if((data_source=="news")==TRUE){
    news7 <- news7[which(news7$first==search_words[1] & news7$second==search_words[2] & news7$third==search_words[3] & news7$fourth==search_words[4]& news7$fifth==search_words[5] & news7$sixth==search_words[6]),]
    news7<-orderBy(~-count, data=news7)
    news7sum <- sum(news7$count)
    news7$frequency<-news7$count / news7sum
    n_gram_data <- news7
  }else if((data_source=="twitter")==TRUE){
    twitter7 <- twitter7[which(twitter7$first==search_words[1] & twitter7$second==search_words[2] & twitter7$third==search_words[3] & twitter7$fourth==search_words[4]& twitter7$fifth==search_words[5] & twitter7$sixth==search_words[6]),]
    twitter7<-orderBy(~-count, data=twitter7)
    twitter7sum <- sum(twitter7$count)
    twitter7$frequency<-twitter7$count / twitter7sum
    n_gram_data <- twitter7
  }else{
    n_gram_data <- data.frame(first=character(),second=character(), third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(),stringsAsFactors = FALSE)
    n_gram_data[1,1] = c("not")
    n_gram_data[1,2] = c("handled4")
    n_gram_data[1,3] = c("yet")
    n_gram_data[1,4] = c("really")
    n_gram_data[1,5] = c("uhhuh")
    n_gram_data[1,6] = c("uhhuh")
    n_gram_data[1,7] = c("uhhuh")
    n_gram_data
  }
  rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
  n_gram_data[,1:7]
}

octoGrams <- function(data_source, words8){
  search_words = segmentWords(words8)
  #search_words = tolower(search_words)
  if((data_source=="blogs")==TRUE){
    blogs8 <- blogs8[which(blogs8$first==search_words[1] & blogs8$second==search_words[2] & blogs8$third==search_words[3] & blogs8$fourth==search_words[4]& blogs8$fifth==search_words[5] & blogs8$sixth==search_words[6] & blogs8$seventh==search_words[7]),]
    blogs8<-orderBy(~-count, data=blogs8)
    blogs8sum <- sum(blogs8$count)
    blogs8$frequency<-blogs8$count / blogs8sum
    n_gram_data <- blogs8
  }else if((data_source=="news")==TRUE){
    news8 <- news8[which(news8$first==search_words[1] & news8$second==search_words[2] & news8$third==search_words[3] & news8$fourth==search_words[4]& news8$fifth==search_words[5] & news8$sixth==search_words[6] & news8$seventh==search_words[7]),]
    news8<-orderBy(~-count, data=news8)
    news8sum <- sum(news8$count)
    news8$frequency<-news8$count / news8sum
    n_gram_data <- news8
  }else if((data_source=="twitter")==TRUE){
    twitter8 <- twitter8[which(twitter8$first==search_words[1] & twitter8$second==search_words[2] & twitter8$third==search_words[3] & twitter8$fourth==search_words[4]& twitter8$fifth==search_words[5] & twitter8$sixth==search_words[6] & twitter8$seventh==search_words[7]),]
    twitter8<-orderBy(~-count, data=twitter8)
    twitter8sum <- sum(twitter8$count)
    twitter8$frequency<-twitter8$count / twitter8sum
    n_gram_data <- twitter8
  }else{
    n_gram_data <- data.frame(first=character(),second=character(), third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(), eighth=character(), stringsAsFactors = FALSE)
    n_gram_data[1,1] = c("not")
    n_gram_data[1,2] = c("handled4")
    n_gram_data[1,3] = c("yet")
    n_gram_data[1,4] = c("really")
    n_gram_data[1,5] = c("uhhuh")
    n_gram_data[1,6] = c("uhhuh")
    n_gram_data[1,7] = c("uhhuh")
    n_gram_data[1,8] = c("uhhuh")
    n_gram_data
  }
  rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
  n_gram_data[,1:8]
}

nonaGrams <- function(data_source, words9){
  search_words = segmentWords(words9)
  #search_words = tolower(search_words)
  if((data_source=="blogs")==TRUE){
    blogs9 <- blogs9[which(blogs9$first==search_words[1] & blogs9$second==search_words[2] & blogs9$third==search_words[3] & blogs9$fourth==search_words[4]& blogs9$fifth==search_words[5] & blogs9$sixth==search_words[6] & blogs9$seventh==search_words[7] & blogs9$eighth==search_words[8]),]
    blogs9<-orderBy(~-count, data=blogs9)
    blogs9sum <- sum(blogs9$count)
    blogs9$frequency<-blogs9$count / blogs9sum
    n_gram_data <- blogs9
  }else if((data_source=="news")==TRUE){
    news9 <- news9[which(news9$first==search_words[1] & news9$second==search_words[2] & news9$third==search_words[3] & news9$fourth==search_words[4]& news9$fifth==search_words[5] & news9$sixth==search_words[6] & news9$seventh==search_words[7] & news9$eighth==search_words[8]),]
    news9<-orderBy(~-count, data=news9)
    news9sum <- sum(news9$count)
    news9$frequency<-news9$count / news9sum
    n_gram_data <- news9
  }else if((data_source=="twitter")==TRUE){
    twitter9 <- twitter9[which(twitter9$first==search_words[1] & twitter9$second==search_words[2] & twitter9$third==search_words[3] & twitter9$fourth==search_words[4]& twitter9$fifth==search_words[5] & twitter9$sixth==search_words[6] & twitter9$seventh==search_words[7] & twitter9$eighth==search_words[8]),]
    twitter9<-orderBy(~-count, data=twitter9)
    twitter9sum <- sum(twitter9$count)
    twitter9$frequency<-twitter9$count / twitter9sum
    n_gram_data <- twitter9
  }else{
    n_gram_data <- data.frame(first=character(),second=character(), third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(), eighth=character(), ninth=character(), stringsAsFactors = FALSE)
    n_gram_data[1,1] = c("not")
    n_gram_data[1,2] = c("handled4")
    n_gram_data[1,3] = c("yet")
    n_gram_data[1,4] = c("really")
    n_gram_data[1,5] = c("uhhuh")
    n_gram_data[1,6] = c("uhhuh")
    n_gram_data[1,7] = c("uhhuh")
    n_gram_data[1,8] = c("uhhuh")
    n_gram_data[1,9] = c("uhhuh")
    n_gram_data
  }
  rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
  n_gram_data
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  output$table = DT::renderDataTable(
    

    
    if((countWords(input$words_in)>8)==TRUE){
      n_gram_data <- data.frame(first=character(),second=character(), third=character(), stringsAsFactors = FALSE)
      n_gram_data[1,1] = c("Please")
      n_gram_data[1,2] = c("enter")
      n_gram_data[1,3] = c("eight")
      n_gram_data[1,4] = c("or")
      n_gram_data[1,5] = c("less")
      n_gram_data[1,6] = c("words")
      colnames(n_gram_data) <- c("first","second","third", "fourth","fifth","sixth")
      rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
      n_gram_data
    }else if((countWords(input$words_in)==0)==TRUE){
      n_gram_data <- data.frame(first=character(),second=character(), third=character(), stringsAsFactors = FALSE)
      n_gram_data[1,1] = c("Please")
      n_gram_data[1,2] = c("enter")
      n_gram_data[1,3] = c("words")
      colnames(n_gram_data) <- c("first","second","third")
      rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
      n_gram_data
    }else if((countWords(input$words_in)==1)==TRUE){
      temp = as.character(input$words_in)
      temp2 = str_replace_all(temp, '[:punct:]', '')
      bi_grams <- biGrams(input$data_source, temp2)
      if((nrow(bi_grams)==0)==TRUE){
        n_gram_data <- data.frame(first=character(),second=character(), stringsAsFactors = FALSE)
        n_gram_data[1,1] = as.character(input$words_in)
        n_gram_data[1,2] = c("is")
        rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
        n_gram_data
      }else if((nrow(bi_grams)>0)==TRUE){
        n_gram_data = bi_grams
        colnames(n_gram_data) <- c("first","second")
        rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
        n_gram_data
      }else{
        n_gram_data <- data.frame(first=character(),second=character(), stringsAsFactors = FALSE)
        n_gram_data[1,1] = "not"
        n_gram_data[1,2] = "handled5"
        rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
        n_gram_data
      }
    }else if((countWords(input$words_in)==2)==TRUE){
      tri_grams <- triGrams(input$data_source, input$words_in)
      if((nrow(tri_grams)==0)==TRUE){
        segmented_words = segmentWords(input$words_in)
        bi_grams <- biGrams(input$data_source, segmented_words[2])
        if((nrow(bi_grams)==0)==TRUE){
          n_gram_data <- data.frame(first=character(),second=character(), third=character(), stringsAsFactors = FALSE)
          n_gram_data[1,1] = as.character(segmented_words[1])
          n_gram_data[1,2] = as.character(segmented_words[2])
          n_gram_data[1,3] = "is"
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          n_gram_data
        }else if((nrow(bi_grams)>0)==TRUE){
          n_gram_data <- data.frame(first=character(),second=character(), third=character(), stringsAsFactors = FALSE)
          n_gram_data[1,1] = segmented_words[1]
          n_gram_data[1,2] = segmented_words[2]
          n_gram_data[1,3] = as.character(bi_grams[1,2])
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          n_gram_data
        }else{
          n_gram_data <- data.frame(first=character(),second=character(), stringsAsFactors = FALSE)
          n_gram_data[1,1] = "not"
          n_gram_data[1,2] = "handled6"
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          n_gram_data
        }
      }else if((nrow(tri_grams)>0)==TRUE){
        n_gram_data <- tri_grams
        colnames(n_gram_data) <- c("first","second","third")
        rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
        n_gram_data
      }else{
        n_gram_data <- data.frame(first=character(),second=character(), third=character(), stringsAsFactors = FALSE)
        n_gram_data[1,1] = "this"
        n_gram_data[1,2] = "not"
        n_gram_data[1,3] = "handled7"
        rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
        n_gram_data
      }
    }else if((countWords(input$words_in)==3)==TRUE){
      quad_grams <- quadGrams(input$data_source, input$words_in)
      if((nrow(quad_grams)==0)==TRUE){
        segmented_words = segmentWords(input$words_in)
        to_drive_trigrams = paste(segmented_words[2], segmented_words[3])
        tri_grams = triGrams(input$data_source, to_drive_trigrams)
        if((nrow(tri_grams)==0)==TRUE){
          segmented_words = segmentWords(input$words_in)
          to_drive_bigrams = unlist(segmented_words[3])
          bi_grams = biGrams(input$data_source, to_drive_bigrams)
          if((nrow(bi_grams)==0)==TRUE){
            n_gram_data <- data.frame(first=character(),second=character(),third=character(), fourth=character(), stringsAsFactors = FALSE)
            n_gram_data[1,1] = segmented_words[1]
            n_gram_data[1,2] = segmented_words[2]
            n_gram_data[1,3] = segmented_words[3]
            n_gram_data[1,4] = "is"
            rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
            colnames(n_gram_data) <- c("first","second","third","fourth")
            n_gram_data
          }else if((nrow(bi_grams)>0)==TRUE){
            col_2_length = nrow(bi_grams)
            col_2 = rep_len(segmented_words[2],col_2_length)
            three_of_four_cols = cbind(col_2, bi_grams)
            col_1_length = col_2_length
            col_1 = rep_len(segmented_words[1],col_1_length)
            n_gram_data = cbind(col_1, three_of_four_cols)
            rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
            colnames(n_gram_data) = c("first", "second", "third", "fourth")
            n_gram_data
          }else{
            n_gram_data <- data.frame(first=character(),second=character(), third=character(), stringsAsFactors = FALSE)
            n_gram_data[1,1] = "this"
            n_gram_data[1,2] = "not"
            n_gram_data[1,3] = "handled8"
            n_gram_data[1,4] = "yes"
            rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
            n_gram_data
          }
          
        }else if((nrow(tri_grams)>0)==TRUE){
          col_1_length = nrow(tri_grams)
          col_1 = rep_len(segmented_words[1],col_1_length)
          n_gram_data = cbind(col_1, tri_grams)
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          colnames(n_gram_data) <- c("first", "second", "third", "fourth")
          n_gram_data
        }else{
          n_gram_data <- data.frame(first=character(),second=character(), third=character(),
                                    fourth=character(), stringsAsFactors = FALSE)
          n_gram_data[1,1] = "this"
          n_gram_data[1,2] = "not"
          n_gram_data[1,3] = "handled9"
          n_gram_data[1,4] = "yes"
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          n_gram_data
        }
      }else if((nrow(quad_grams)>0)==TRUE){
        n_gram_data <- quad_grams
        rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
        colnames(n_gram_data) <- c("first", "second", "third", "fourth")
        n_gram_data
      }else{
        n_gram_data <- data.frame(first=character(),second=character(), third=character(),
                                  fourth=character(), stringsAsFactors = FALSE)
        n_gram_data[1,1] = "this"
        n_gram_data[1,2] = "not"
        n_gram_data[1,3] = "handled10"
        n_gram_data[1,4] = "yes"
        rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
        n_gram_data
      }
    }else if((countWords(input$words_in)==4)==TRUE){
      
      penta_grams <- pentaGrams(input$data_source, input$words_in[1])
      if((nrow(penta_grams)==0)==TRUE){
        segmented_words = segmentWords(input$words_in)
        last_three_cols = paste(unlist(segmented_words[2]), unlist(segmented_words[3]), unlist(segmented_words[4]))
        quad_grams <- quadGrams(input$data_source, last_three_cols)
        if((nrow(quad_grams)==0)==TRUE){
          segmented_words = segmentWords(input$words_in)
          last_two_cols = paste(unlist(segmented_words[3]), unlist(segmented_words[4]))
          tri_grams = triGrams(input$data_source, last_two_cols)
          if((nrow(tri_grams)==0)==TRUE){
            segmented_words = segmentWords(input$words_in)
            last_col = unlist(segmented_words[4])
            bi_grams = biGrams(input$data_source, last_col)
            if((nrow(bi_grams)==0)==TRUE){
              n_gram_data <- data.frame(first=character(),second=character(),
                                        third=character(), fourth=character(), fifth=character(),
                                        stringsAsFactors = FALSE)
              n_gram_data[1,1] = segmented_words[1]
              n_gram_data[1,2] = segmented_words[2]
              n_gram_data[1,3] = segmented_words[3]
              n_gram_data[1,4] = segmented_words[4]
              n_gram_data[1,5] = c("is")
              rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
              n_gram_data
            }else if((nrow(bi_grams)>0)==TRUE){
              col_1_length = nrow(bi_grams)
              col_1 = rep_len(segmented_words[1],col_1_length)
              col_2_length = nrow(bi_grams)
              col_2 = rep_len(segmented_words[2],col_2_length)
              col_3_length = nrow(bi_grams)
              col_3 = rep_len(segmented_words[3],col_3_length)
              temp <- cbind(col_1, col_2)
              temp2 <- cbind(temp, col_3)
              n_gram_data = cbind(temp2, bi_grams)
              rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
              colnames(n_gram_data) <- c("first","second","third","fourth","fifth")
              n_gram_data
            }else{
              n_gram_data <- data.frame(first=character(),second=character(),
                                        third=character(), fourth=character(), fifth=character(), stringsAsFactors = FALSE)
              n_gram_data[1,1] = "this"
              n_gram_data[1,2] = "not"
              n_gram_data[1,3] = "handled11"
              n_gram_data[1,4] = "yes"
              n_gram_data[1,5] = "yes"
              rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
              n_gram_data
            }
          }else if((nrow(tri_grams)>0)==TRUE){
            col_1_length = nrow(tri_grams)
            col_1 = rep_len(segmented_words[1],col_1_length)
            last_four_cols = cbind(col_1, tri_grams)
            n_gram_data = cbind(col_1, last_four_cols)
            rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
            colnames(n_gram_data) <- c("first","second","third","fourth","fifth")
            n_gram_data
          }else{
            n_gram_data <- data.frame(first=character(),second=character(),
                                      third=character(), fourth=character(), fifth=character(), stringsAsFactors = FALSE)
            n_gram_data[1,1] = "this"
            n_gram_data[1,2] = "not"
            n_gram_data[1,3] = "handled12"
            n_gram_data[1,4] = "yes"
            n_gram_data[1,5] = "yes"
            rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
            n_gram_data
          }
        }else if((nrow(quad_grams)>0)==TRUE){
          col_1_length = nrow(quad_grams)
          col_1 = rep_len(segmented_words[1],col_1_length)
          n_gram_data = cbind(col_1, quad_grams)
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          colnames(n_gram_data) <- c("first","second","third","fourth","fifth")
          n_gram_data
        }else{
          n_gram_data <- data.frame(first=character(),second=character(),
                                    third=character(), fourth=character(), fifth=character(), stringsAsFactors = FALSE)
          n_gram_data[1,1] = "this"
          n_gram_data[1,2] = "not"
          n_gram_data[1,3] = "handled13"
          n_gram_data[1,4] = "yes"
          n_gram_data[1,5] = "yes"
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          n_gram_data
        }
      }else if((nrow(penta_grams)>0)==TRUE){
        n_gram_data <- penta_grams
        rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
        colnames(n_gram_data) <- c("first", "second", "third", "fourth", "fifth")
        n_gram_data
      }else{
        n_gram_data <- data.frame(first=character(),second=character(),
                                  third=character(), fourth=character(), fifth=character(), stringsAsFactors = FALSE)
        n_gram_data[1,1] = "this"
        n_gram_data[1,2] = "not"
        n_gram_data[1,3] = "handled14"
        n_gram_data[1,4] = "yes"
        n_gram_data[1,5] = "yes"
        rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
        n_gram_data
      }
    }else if((countWords(input$words_in)==5)==TRUE){
      hexa_grams <- hexaGrams(input$data_source, input$words_in[1])
      if((nrow(hexa_grams)==0)==TRUE){
        segmented_words = segmentWords(input$words_in)
        last_four_cols = paste(unlist(segmented_words[2]), unlist(segmented_words[3]), unlist(segmented_words[4])
                               , unlist(segmented_words[5]))
        penta_grams <- pentaGrams(input$data_source, last_four_cols)
        if((nrow(penta_grams)==0)==TRUE){
          segmented_words = segmentWords(input$words_in)
          last_three_cols = paste(unlist(segmented_words[3]), unlist(segmented_words[4]), unlist(segmented_words[5]))
          quad_grams <- quadGrams(input$data_source, last_three_cols)
          if((nrow(quad_grams)==0)==TRUE){
            segmented_words = segmentWords(input$words_in)
            last_two_cols = paste(unlist(segmented_words[4]), unlist(segmented_words[5]))
            tri_grams = triGrams(input$data_source, last_two_cols)
            if((nrow(tri_grams)==0)==TRUE){
              segmented_words = segmentWords(input$words_in)
              last_col = unlist(segmented_words[5])
              bi_grams = biGrams(input$data_source, last_col)
              if((nrow(bi_grams)==0)==TRUE){
                n_gram_data <- data.frame(first=character(),second=character(),
                                          third=character(), fourth=character(), fifth=character(),
                                          sixth=character(), stringsAsFactors = FALSE)
                n_gram_data[1,1] = segmented_words[1]
                n_gram_data[1,2] = segmented_words[2]
                n_gram_data[1,3] = segmented_words[3]
                n_gram_data[1,4] = segmented_words[4]
                n_gram_data[1,5] = segmented_words[5]
                n_gram_data[1,6] = c("is")
                rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                n_gram_data
              }else if((nrow(bi_grams)>0)==TRUE){ #works
                col_1_length = nrow(bi_grams)
                col_1 = rep_len(segmented_words[1],col_1_length)
                col_2_length = nrow(bi_grams)
                col_2 = rep_len(segmented_words[2],col_2_length)
                col_3_length = nrow(bi_grams)
                col_3 = rep_len(segmented_words[3],col_3_length)
                col_4_length = nrow(bi_grams)
                col_4 = rep_len(segmented_words[4],col_4_length)
                last_three_cols = cbind(col_4, bi_grams)
                last_four_cols = cbind(col_3,last_three_cols)
                last_five_cols = cbind(col_2,last_four_cols)
                n_gram_data = cbind(col_1, last_five_cols)
                rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                colnames(n_gram_data) <- c("first","second","third","fourth","fifth","sixth")
                n_gram_data
              }else{
                n_gram_data <- data.frame(first=character(),second=character(),
                                          third=character(), fourth=character(), fifth=character(), stringsAsFactors = FALSE)
                n_gram_data[1,1] = "this"
                n_gram_data[1,2] = "not"
                n_gram_data[1,3] = "handledA"
                n_gram_data[1,4] = "yes"
                n_gram_data[1,5] = "yes"
                n_gram_data[1,6] = "yes"
                rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                n_gram_data
              }
            }else if((nrow(tri_grams)>0)==TRUE){ #works
              col_1_length = nrow(tri_grams)
              col_1 = rep_len(segmented_words[1],col_1_length)
              col_2_length = nrow(tri_grams)
              col_2 = rep_len(segmented_words[2],col_2_length)
              col_3_length = nrow(tri_grams)
              col_3 = rep_len(segmented_words[3],col_3_length)
              temp = cbind(col_1, col_2)
              temp2 = cbind(temp, col_3)
              n_gram_data <- cbind(temp2, tri_grams)
              rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
              colnames(n_gram_data) <- c("first","second","third","fourth","fifth", "sixth")
              n_gram_data
            }else{
              n_gram_data <- data.frame(first=character(),second=character(),
                                        third=character(), fourth=character(), fifth=character(), stringsAsFactors = FALSE)
              n_gram_data[1,1] = "this"
              n_gram_data[1,2] = "not"
              n_gram_data[1,3] = "handledB"
              n_gram_data[1,4] = "yes"
              n_gram_data[1,5] = "yes"
              n_gram_data[1,6] = "yes"
              rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
              n_gram_data
            }
          }else if((nrow(quad_grams)>0)==TRUE){ #works
            col_1_length = nrow(quad_grams)
            col_1 = rep_len(segmented_words[1],col_1_length)
            col_2_length = nrow(quad_grams)
            col_2 = rep_len(segmented_words[2],col_2_length)
            temp <- cbind(col_1, col_2)
            n_gram_data <- cbind(temp, quad_grams)
            rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
            colnames(n_gram_data) <- c("first","second","third","fourth","fifth", "sixth")
            n_gram_data
          }else{
            n_gram_data <- data.frame(first=character(),second=character(),
                                      third=character(), fourth=character(), fifth=character(), sixth=character(), stringsAsFactors = FALSE)
            n_gram_data[1,1] = "this"
            n_gram_data[1,2] = "not"
            n_gram_data[1,3] = "handledC"
            n_gram_data[1,4] = "yes"
            n_gram_data[1,5] = "yes"
            n_gram_data[1,6] = "yes"
            rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
            n_gram_data
          }
        }else if((nrow(penta_grams)>0)==TRUE){ #works

          col_1_length = nrow(penta_grams)
          col_1 = rep_len(segmented_words[1],col_1_length)
          n_gram_data = cbind(col_1, penta_grams)
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          colnames(n_gram_data) <- c("first","second","third","fourth","fifth", "sixth")
          n_gram_data
          
        }else{
          n_gram_data <- data.frame(first=character(),second=character(),
                                    third=character(), fourth=character(), fifth=character(),
                                    sixth=character(), stringsAsFactors = FALSE)
          n_gram_data[1,1] = "this"
          n_gram_data[1,2] = "not"
          n_gram_data[1,3] = "handledC"
          n_gram_data[1,4] = "yes"
          n_gram_data[1,5] = "yes"
          n_gram_data[1,6] = "yes"
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          n_gram_data
        }
      }else if((nrow(hexa_grams)>0)==TRUE){ #works
        n_gram_data <- hexa_grams
        rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
        colnames(n_gram_data) <- c("first", "second", "third", "fourth", "fifth", "sixth")
        n_gram_data[,1:6]
      }else{
        n_gram_data <- data.frame(first=character(),second=character(),
                                  third=character(), fourth=character(), fifth=character(), sixth=character(), stringsAsFactors = FALSE)
        n_gram_data[1,1] = "this"
        n_gram_data[1,2] = "not"
        n_gram_data[1,3] = "hexa_grams_else"
        n_gram_data[1,4] = "yes"
        n_gram_data[1,5] = "yes"
        n_gram_data[1,6] = "yes"
        rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
        n_gram_data
      }
    }else if((countWords(input$words_in)==6)==TRUE){
      hepta_grams <- heptaGrams(input$data_source, input$words_in[1])
      if((nrow(hepta_grams)==0)==TRUE){
        segmented_words = segmentWords(input$words_in)
        last_five_cols = paste(unlist(segmented_words[2]),unlist(segmented_words[3]), unlist(segmented_words[4]), unlist(segmented_words[5])
                               , unlist(segmented_words[6]))
        hexa_grams <- hexaGrams(input$data_source, last_five_cols)
        if((nrow(hexa_grams)==0)==TRUE){
          segmented_words = segmentWords(input$words_in)
          last_four_cols = paste(unlist(segmented_words[3]), unlist(segmented_words[4]), unlist(segmented_words[5])
                                 , unlist(segmented_words[6]))
          penta_grams <- pentaGrams(input$data_source, last_four_cols)
          if((nrow(penta_grams)==0)==TRUE){
            segmented_words = segmentWords(input$words_in)
            last_three_cols = paste(unlist(segmented_words[4]), unlist(segmented_words[5]), unlist(segmented_words[6]))
            quad_grams <- quadGrams(input$data_source, last_three_cols)
            if((nrow(quad_grams)==0)==TRUE){
              segmented_words = segmentWords(input$words_in)
              last_two_cols = paste(unlist(segmented_words[5]), unlist(segmented_words[6]))
              tri_grams = triGrams(input$data_source, last_two_cols)
              if((nrow(tri_grams)==0)==TRUE){
                segmented_words = segmentWords(input$words_in)
                last_col = unlist(segmented_words[6])
                bi_grams = biGrams(input$data_source, last_col)
                if((nrow(bi_grams)==0)==TRUE){
                  n_gram_data <- data.frame(first=character(),second=character(),
                                            third=character(), fourth=character(), fifth=character(),
                                            sixth=character(), seventh=character(), stringsAsFactors = FALSE)
                  n_gram_data[1,1] = segmented_words[1]
                  n_gram_data[1,2] = segmented_words[2]
                  n_gram_data[1,3] = segmented_words[3]
                  n_gram_data[1,4] = segmented_words[4]
                  n_gram_data[1,5] = segmented_words[5]
                  n_gram_data[1,6] = segmented_words[6]
                  n_gram_data[1,7] = c("is")
                  rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                  n_gram_data
                }else if((nrow(bi_grams)>0)==TRUE){ #works
                  col_1_length = nrow(bi_grams)
                  col_1 = rep_len(segmented_words[1],col_1_length)
                  col_2_length = nrow(bi_grams)
                  col_2 = rep_len(segmented_words[2],col_2_length)
                  col_3_length = nrow(bi_grams)
                  col_3 = rep_len(segmented_words[3],col_3_length)
                  col_4_length = nrow(bi_grams)
                  col_4 = rep_len(segmented_words[4],col_4_length)
                  col_5_length = nrow(bi_grams)
                  col_5 = rep_len(segmented_words[5],col_5_length)
                  temp = cbind(col_1, col_2)
                  temp2 = cbind(temp, col_3)
                  temp3 = cbind(temp2, col_4)
                  temp4 = cbind(temp3, col_5)
                  n_gram_data <- cbind(temp4, bi_grams)
                  rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                  colnames(n_gram_data) <- c("first","second","third","fourth","fifth","sixth", "seventh")
                  n_gram_data[1:7]
                }else{
                  n_gram_data <- data.frame(first=character(),second=character(),
                                            third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(), stringsAsFactors = FALSE)
                  n_gram_data[1,1] = "this"
                  n_gram_data[1,2] = "not"
                  n_gram_data[1,3] = "handledA"
                  n_gram_data[1,4] = "yes"
                  n_gram_data[1,5] = "yes"
                  n_gram_data[1,6] = "yes"
                  n_gram_data[1,7] = "yes"
                  rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                  n_gram_data
                }
              }else if((nrow(tri_grams)>0)==TRUE){ #works
                col_1_length = nrow(tri_grams)
                col_1 = rep_len(segmented_words[1],col_1_length)
                col_2_length = nrow(tri_grams)
                col_2 = rep_len(segmented_words[2],col_2_length)
                col_3_length = nrow(tri_grams)
                col_3 = rep_len(segmented_words[3],col_3_length)
                col_4_length = nrow(tri_grams)
                col_4 = rep_len(segmented_words[4],col_4_length)
                temp = cbind(col_1, col_2)
                temp2 = cbind(temp, col_3)
                temp3 = cbind(temp2, col_4)
                n_gram_data <- cbind(temp3, tri_grams)
                rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                colnames(n_gram_data) <- c("first","second","third","fourth","fifth", "sixth", "seventh")
                n_gram_data
              }else{
                n_gram_data <- data.frame(first=character(),second=character(),
                                          third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(),  stringsAsFactors = FALSE)
                n_gram_data[1,1] = "this"
                n_gram_data[1,2] = "not"
                n_gram_data[1,3] = "handledB"
                n_gram_data[1,4] = "yes"
                n_gram_data[1,5] = "yes"
                n_gram_data[1,6] = "yes"
                n_gram_data[1,7] = "yes"
                rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                n_gram_data
              }
            }else if((nrow(quad_grams)>0)==TRUE){ #works
              col_1_length = nrow(quad_grams)
              col_1 = rep_len(segmented_words[1],col_1_length)
              col_2_length = nrow(quad_grams)
              col_2 = rep_len(segmented_words[2],col_2_length)
              col_3_length = nrow(quad_grams)
              col_3 = rep_len(segmented_words[3],col_3_length)
              temp <- cbind(col_1, col_2)
              temp2 <- cbind(temp, col_3)
              n_gram_data <- cbind(temp2, quad_grams)
              rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
              colnames(n_gram_data) <- c("first","second","third","fourth","fifth", "sixth", "seventh")
              n_gram_data
            }else{
              n_gram_data <- data.frame(first=character(),second=character(),
                                        third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(),stringsAsFactors = FALSE)
              n_gram_data[1,1] = "this"
              n_gram_data[1,2] = "not"
              n_gram_data[1,3] = "handledC"
              n_gram_data[1,4] = "yes"
              n_gram_data[1,5] = "yes"
              n_gram_data[1,6] = "yes"
              n_gram_data[1,7] = "yes"
              rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
              n_gram_data
            }
          }else if((nrow(penta_grams)>0)==TRUE){ #works
            col_1_length = nrow(penta_grams)
            col_1 = rep_len(segmented_words[1],col_1_length)
            col_2_length = nrow(penta_grams)
            col_2 = rep_len(segmented_words[2],col_1_length)
            temp <- cbind(col_1, col_2)
            n_gram_data <- cbind(temp, penta_grams)
            rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
            colnames(n_gram_data) <- c("first","second","third","fourth","fifth","sixth","seventh")
            n_gram_data
          }else{
            n_gram_data <- data.frame(first=character(),second=character(),
                                      third=character(), fourth=character(), fifth=character(),
                                      sixth=character(), seventh=character(), stringsAsFactors = FALSE)
            n_gram_data[1,1] = "this"
            n_gram_data[1,2] = "not"
            n_gram_data[1,3] = "handledC"
            n_gram_data[1,4] = "yes"
            n_gram_data[1,5] = "yes"
            n_gram_data[1,6] = "yes"
            n_gram_data[1,7] = "yes"
            rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
            n_gram_data
          }
        }else if((nrow(hexa_grams)>0)==TRUE){ #works
          col_1_length = nrow(hexa_grams)
          col_1 = rep_len(segmented_words[1],col_1_length)
          n_gram_data <- cbind(col_1, hexa_grams)
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          colnames(n_gram_data) <- c("first", "second", "third", "fourth", "fifth", "sixth", "seventh")
          n_gram_data[,1:7]
        }else{
          n_gram_data <- data.frame(first=character(),second=character(),
                                    third=character(), fourth=character(), fifth=character(), sixth=character(), stringsAsFactors = FALSE)
          n_gram_data[1,1] = "this"
          n_gram_data[1,2] = "not"
          n_gram_data[1,3] = "hexa_grams_else"
          n_gram_data[1,4] = "yes"
          n_gram_data[1,5] = "yes"
          n_gram_data[1,6] = "yes"
          n_gram_data[1,7] = "yes"
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          n_gram_data
        }
      }else if((nrow(hepta_grams)>0)==TRUE){ #Does not work
        n_gram_data <- hepta_grams
        rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
        colnames(n_gram_data) <- c("first", "second", "third", "fourth", "fifth", "sixth", "seventh")
        n_gram_data[,1:7]
      }else{
        n_gram_data <- data.frame(first=character(),second=character(),
                                  third=character(), fourth=character(), fifth=character(), sixth=character(), stringsAsFactors = FALSE)
        n_gram_data[1,1] = "this"
        n_gram_data[1,2] = "not"
        n_gram_data[1,3] = "hexa_grams_else"
        n_gram_data[1,4] = "yes"
        n_gram_data[1,5] = "yes"
        n_gram_data[1,6] = "yes"
        n_gram_data[1,7] = "yes"
        rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
        n_gram_data
      }
      
    }else if((countWords(input$words_in)==7)==TRUE){
      octo_grams <- octoGrams(input$data_source, input$words_in[1])
      if((nrow(octo_grams)==0)==TRUE){
        segmented_words = segmentWords(input$words_in)
        last_six_cols = paste(unlist(segmented_words[2]), unlist(segmented_words[3]),unlist(segmented_words[4]), unlist(segmented_words[5]), unlist(segmented_words[6])
                              , unlist(segmented_words[7]))
        
        hepta_grams <- heptaGrams(input$data_source, last_six_cols)
        if((nrow(hepta_grams)==0)==TRUE){
          segmented_words = segmentWords(input$words_in)
          last_five_cols = paste(unlist(segmented_words[3]),unlist(segmented_words[4]), unlist(segmented_words[5]), unlist(segmented_words[6])
                                 , unlist(segmented_words[7]))
          hexa_grams <- hexaGrams(input$data_source, last_five_cols)
          if((nrow(hexa_grams)==0)==TRUE){
            segmented_words = segmentWords(input$words_in)
            last_four_cols = paste(unlist(segmented_words[4]), unlist(segmented_words[5]), unlist(segmented_words[6])
                                   , unlist(segmented_words[7]))
            penta_grams <- pentaGrams(input$data_source, last_four_cols)
            if((nrow(penta_grams)==0)==TRUE){
              segmented_words = segmentWords(input$words_in)
              last_three_cols = paste(unlist(segmented_words[5]), unlist(segmented_words[6]), unlist(segmented_words[7]))
              quad_grams <- quadGrams(input$data_source, last_three_cols)
              if((nrow(quad_grams)==0)==TRUE){
                segmented_words = segmentWords(input$words_in)
                last_two_cols = paste(unlist(segmented_words[6]), unlist(segmented_words[7]))
                tri_grams = triGrams(input$data_source, last_two_cols)
                if((nrow(tri_grams)==0)==TRUE){
                  segmented_words = segmentWords(input$words_in)
                  last_col = unlist(segmented_words[7])
                  bi_grams = biGrams(input$data_source, last_col)
                  if((nrow(bi_grams)==0)==TRUE){
                    n_gram_data <- data.frame(first=character(),second=character(),
                                              third=character(), fourth=character(), fifth=character(),
                                              sixth=character(), seventh=character(), eighth=character(), stringsAsFactors = FALSE)
                    n_gram_data[1,1] = segmented_words[1]
                    n_gram_data[1,2] = segmented_words[2]
                    n_gram_data[1,3] = segmented_words[3]
                    n_gram_data[1,4] = segmented_words[4]
                    n_gram_data[1,5] = segmented_words[5]
                    n_gram_data[1,6] = segmented_words[6]
                    n_gram_data[1,7] = segmented_words[7]
                    n_gram_data[1,8] = c("is")
                    rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                    n_gram_data
                  }else if((nrow(bi_grams)>0)==TRUE){ #works
                    col_1_length = nrow(bi_grams)
                    col_1 = rep_len(segmented_words[1],col_1_length)
                    col_2_length = nrow(bi_grams)
                    col_2 = rep_len(segmented_words[2],col_2_length)
                    col_3_length = nrow(bi_grams)
                    col_3 = rep_len(segmented_words[3],col_3_length)
                    col_4_length = nrow(bi_grams)
                    col_4 = rep_len(segmented_words[4],col_4_length)
                    col_5_length = nrow(bi_grams)
                    col_5 = rep_len(segmented_words[5],col_5_length)
                    col_6_length = nrow(bi_grams)
                    col_6 = rep_len(segmented_words[6],col_6_length)
                    temp = cbind(col_1, col_2)
                    temp2 = cbind(temp, col_3)
                    temp3 = cbind(temp2, col_4)
                    temp4 = cbind(temp3, col_5)
                    temp5 = cbind(temp4, col_6)
                    n_gram_data <- cbind(temp5, bi_grams)
                    rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                    colnames(n_gram_data) <- c("first","second","third","fourth","fifth","sixth", "seventh", "eighth")
                    n_gram_data[1:8]
                  }else{
                    n_gram_data <- data.frame(first=character(),second=character(),
                                              third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(), eighth=character(),stringsAsFactors = FALSE)
                    n_gram_data[1,1] = "this"
                    n_gram_data[1,2] = "not"
                    n_gram_data[1,3] = "handledA"
                    n_gram_data[1,4] = "yes"
                    n_gram_data[1,5] = "yes"
                    n_gram_data[1,6] = "yes"
                    n_gram_data[1,7] = "yes"
                    n_gram_data[1,8] = "yes"
                    rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                    n_gram_data
                  }
                }else if((nrow(tri_grams)>0)==TRUE){ #works
                  col_1_length = nrow(tri_grams)
                  col_1 = rep_len(segmented_words[1],col_1_length)
                  col_2_length = nrow(tri_grams)
                  col_2 = rep_len(segmented_words[2],col_2_length)
                  col_3_length = nrow(tri_grams)
                  col_3 = rep_len(segmented_words[3],col_3_length)
                  col_4_length = nrow(tri_grams)
                  col_4 = rep_len(segmented_words[4],col_4_length)
                  col_5_length = nrow(tri_grams)
                  col_5 = rep_len(segmented_words[5],col_5_length)
                  temp = cbind(col_1, col_2)
                  temp2 = cbind(temp, col_3)
                  temp3 = cbind(temp2, col_4)
                  temp4 = cbind(temp3, col_5)
                  n_gram_data <- cbind(temp4, tri_grams)
                  rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                  colnames(n_gram_data) <- c("first","second","third","fourth","fifth", "sixth", "seventh", "eighth")
                  n_gram_data
                }else{
                  n_gram_data <- data.frame(first=character(),second=character(),
                                            third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(),  eighth=character(), stringsAsFactors = FALSE)
                  n_gram_data[1,1] = "this"
                  n_gram_data[1,2] = "not"
                  n_gram_data[1,3] = "handledB"
                  n_gram_data[1,4] = "yes"
                  n_gram_data[1,5] = "yes"
                  n_gram_data[1,6] = "yes"
                  n_gram_data[1,7] = "yes"
                  n_gram_data[1,8] = "yes"
                  rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                  n_gram_data
                }
              }else if((nrow(quad_grams)>0)==TRUE){ #works
                col_1_length = nrow(quad_grams)
                col_1 = rep_len(segmented_words[1],col_1_length)
                col_2_length = nrow(quad_grams)
                col_2 = rep_len(segmented_words[2],col_2_length)
                col_3_length = nrow(quad_grams)
                col_3 = rep_len(segmented_words[3],col_3_length)
                col_4_length = nrow(quad_grams)
                col_4 = rep_len(segmented_words[4],col_4_length)
                temp = cbind(col_1, col_2)
                temp2 = cbind(temp, col_3)
                temp3 = cbind(temp2, col_4)
                n_gram_data <- cbind(temp3, quad_grams)
                rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                colnames(n_gram_data) <- c("first","second","third","fourth","fifth", "sixth", "seventh", "eighth")
                n_gram_data
              }else{
                n_gram_data <- data.frame(first=character(),second=character(),
                                          third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(), eighth=character(), stringsAsFactors = FALSE)
                n_gram_data[1,1] = "this"
                n_gram_data[1,2] = "not"
                n_gram_data[1,3] = "handledC"
                n_gram_data[1,4] = "yes"
                n_gram_data[1,5] = "yes"
                n_gram_data[1,6] = "yes"
                n_gram_data[1,7] = "yes"
                n_gram_data[1,8] = "yes"
                rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                n_gram_data
              }
            }else if((nrow(penta_grams)>0)==TRUE){ #works
              col_1_length = nrow(penta_grams)
              col_1 = rep_len(segmented_words[1],col_1_length)
              col_2_length = nrow(penta_grams)
              col_2 = rep_len(segmented_words[2],col_2_length)
              col_3_length = nrow(penta_grams)
              col_3 = rep_len(segmented_words[3],col_3_length)
              temp <- cbind(col_1, col_2)
              temp2 <- cbind(temp, col_3)
              n_gram_data <- cbind(temp2, penta_grams)
              rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
              colnames(n_gram_data) <- c("first","second","third","fourth","fifth","sixth","seventh", "eighth")
              n_gram_data
            }else{
              n_gram_data <- data.frame(first=character(),second=character(),
                                        third=character(), fourth=character(), fifth=character(),
                                        sixth=character(), seventh=character(), eighth=character(), stringsAsFactors = FALSE)
              n_gram_data[1,1] = "this"
              n_gram_data[1,2] = "not"
              n_gram_data[1,3] = "handledC"
              n_gram_data[1,4] = "yes"
              n_gram_data[1,5] = "yes"
              n_gram_data[1,6] = "yes"
              n_gram_data[1,7] = "yes"
              n_gram_data[1,8] = "yes"
              rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
              n_gram_data
            }
          }else if((nrow(hexa_grams)>0)==TRUE){ #works
            col_1_length = nrow(hexa_grams)
            col_1 = rep_len(segmented_words[1],col_1_length)
            col_2_length = nrow(hexa_grams)
            col_2 = rep_len(segmented_words[2],col_2_length)
            temp <- cbind(col_1, col_2)
            n_gram_data <- cbind(temp, hexa_grams)
            rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
            colnames(n_gram_data) <- c("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth")
            n_gram_data[,1:8]
          }else{
            n_gram_data <- data.frame(first=character(),second=character(),
                                      third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(), eighth=character(), stringsAsFactors = FALSE)
            n_gram_data[1,1] = "this"
            n_gram_data[1,2] = "not"
            n_gram_data[1,3] = "?"
            n_gram_data[1,4] = "yes"
            n_gram_data[1,5] = "yes"
            n_gram_data[1,6] = "yes"
            n_gram_data[1,7] = "yes"
            n_gram_data[1,8] = "yes"
            rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
            n_gram_data
          }
        }else if((nrow(hepta_grams)>0)==TRUE){ #works
          col_1_length = nrow(hepta_grams)
          col_1 = rep_len(segmented_words[1],col_1_length)
          n_gram_data <- cbind(col_1, hepta_grams)
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          colnames(n_gram_data) <- c("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth")
          n_gram_data[,1:8]
        }else{
          n_gram_data <- data.frame(first=character(),second=character(),
                                    third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(), eighth=character(), stringsAsFactors = FALSE)
          n_gram_data[1,1] = "this"
          n_gram_data[1,2] = "not"
          n_gram_data[1,3] = "problem"
          n_gram_data[1,4] = "yes"
          n_gram_data[1,5] = "yes"
          n_gram_data[1,6] = "yes"
          n_gram_data[1,7] = "yes"
          n_gram_data[1,8] = "yes"
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          n_gram_data
        }
      }else if((nrow(octo_grams)>0)==TRUE){ #works
        n_gram_data <- octo_grams
        rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
        colnames(n_gram_data) <- c("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth")
        n_gram_data[,1:8]
      }else{
        n_gram_data <- data.frame(first=character(),second=character(),
                                  third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(), eighth=character(), stringsAsFactors = FALSE)
        n_gram_data[1,1] = "this"
        n_gram_data[1,2] = "not"
        n_gram_data[1,3] = "problem"
        n_gram_data[1,4] = "yes"
        n_gram_data[1,5] = "yes"
        n_gram_data[1,6] = "yes"
        n_gram_data[1,7] = "yes"
        n_gram_data[1,8] = "yes"
        rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
        n_gram_data
      }
      
    }else if((countWords(input$words_in)==8)==TRUE){
      nona_grams <- nonaGrams(input$data_source, input$words_in[1])
      if((nrow(nona_grams)==0)==TRUE){
        segmented_words = segmentWords(input$words_in)
        last_seven_cols = paste(unlist(segmented_words[2]), unlist(segmented_words[3]), unlist(segmented_words[4]),unlist(segmented_words[5]), unlist(segmented_words[6]), unlist(segmented_words[7])
                                , unlist(segmented_words[8]))
        
        octo_grams <- octoGrams(input$data_source, last_seven_cols)
        if((nrow(octo_grams)==0)==TRUE){
          segmented_words = segmentWords(input$words_in)
          last_six_cols = paste(unlist(segmented_words[3]), unlist(segmented_words[4]),unlist(segmented_words[5]), unlist(segmented_words[6]), unlist(segmented_words[7])
                                , unlist(segmented_words[8]))
          
          hepta_grams <- heptaGrams(input$data_source, last_six_cols)
          if((nrow(hepta_grams)==0)==TRUE){
            segmented_words = segmentWords(input$words_in)
            last_five_cols = paste(unlist(segmented_words[4]),unlist(segmented_words[5]), unlist(segmented_words[6]), unlist(segmented_words[7])
                                   , unlist(segmented_words[8]))
            hexa_grams <- hexaGrams(input$data_source, last_five_cols)
            if((nrow(hexa_grams)==0)==TRUE){
              segmented_words = segmentWords(input$words_in)
              last_four_cols = paste(unlist(segmented_words[5]), unlist(segmented_words[6]), unlist(segmented_words[7])
                                     , unlist(segmented_words[8]))
              penta_grams <- pentaGrams(input$data_source, last_four_cols)
              if((nrow(penta_grams)==0)==TRUE){
                segmented_words = segmentWords(input$words_in)
                last_three_cols = paste(unlist(segmented_words[6]), unlist(segmented_words[7]), unlist(segmented_words[8]))
                quad_grams <- quadGrams(input$data_source, last_three_cols)
                if((nrow(quad_grams)==0)==TRUE){
                  segmented_words = segmentWords(input$words_in)
                  last_two_cols = paste(unlist(segmented_words[7]), unlist(segmented_words[8]))
                  tri_grams = triGrams(input$data_source, last_two_cols)
                  if((nrow(tri_grams)==0)==TRUE){
                    segmented_words = segmentWords(input$words_in)
                    last_col = unlist(segmented_words[8])
                    bi_grams = biGrams(input$data_source, last_col)
                    if((nrow(bi_grams)==0)==TRUE){
                      n_gram_data <- data.frame(first=character(),second=character(),
                                                third=character(), fourth=character(), fifth=character(),
                                                sixth=character(), seventh=character(), eighth=character(), ninth=character(), stringsAsFactors = FALSE)
                      n_gram_data[1,1] = segmented_words[1]
                      n_gram_data[1,2] = segmented_words[2]
                      n_gram_data[1,3] = segmented_words[3]
                      n_gram_data[1,4] = segmented_words[4]
                      n_gram_data[1,5] = segmented_words[5]
                      n_gram_data[1,6] = segmented_words[6]
                      n_gram_data[1,7] = segmented_words[7]
                      n_gram_data[1,8] = segmented_words[8]
                      n_gram_data[1,9] = c("is")
                      rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                      n_gram_data
                    }else if((nrow(bi_grams)>0)==TRUE){
                      col_1_length = nrow(bi_grams)
                      col_1 = rep_len(segmented_words[1],col_1_length)
                      col_2_length = nrow(bi_grams)
                      col_2 = rep_len(segmented_words[2],col_2_length)
                      col_3_length = nrow(bi_grams)
                      col_3 = rep_len(segmented_words[3],col_3_length)
                      col_4_length = nrow(bi_grams)
                      col_4 = rep_len(segmented_words[4],col_4_length)
                      col_5_length = nrow(bi_grams)
                      col_5 = rep_len(segmented_words[5],col_5_length)
                      col_6_length = nrow(bi_grams)
                      col_6 = rep_len(segmented_words[6],col_6_length)
                      col_7_length = nrow(bi_grams)
                      col_7 = rep_len(segmented_words[7],col_7_length)
                      temp = cbind(col_1, col_2)
                      temp2 = cbind(temp, col_3)
                      temp3 = cbind(temp2, col_4)
                      temp4 = cbind(temp3, col_5)
                      temp5 = cbind(temp4, col_6)
                      temp6 = cbind(temp5, col_7)
                      n_gram_data <- cbind(temp6, bi_grams)
                      rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                      colnames(n_gram_data) <- c("first","second","third","fourth","fifth","sixth", "seventh", "eighth", "ninth")
                      n_gram_data[1:9]
                    }else{
                      n_gram_data <- data.frame(first=character(),second=character(),
                                                third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(), eighth=character(),ninth=character(), stringsAsFactors = FALSE)
                      n_gram_data[1,1] = "this"
                      n_gram_data[1,2] = "not"
                      n_gram_data[1,3] = "handledA"
                      n_gram_data[1,4] = "yes"
                      n_gram_data[1,5] = "yes"
                      n_gram_data[1,6] = "yes"
                      n_gram_data[1,7] = "yes"
                      n_gram_data[1,8] = "yes"
                      n_gram_data[1,9] = "yes"
                      rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                      n_gram_data
                    }
                  }else if((nrow(tri_grams)>0)==TRUE){
                    col_1_length = nrow(tri_grams)
                    col_1 = rep_len(segmented_words[1],col_1_length)
                    col_2_length = nrow(tri_grams)
                    col_2 = rep_len(segmented_words[2],col_2_length)
                    col_3_length = nrow(tri_grams)
                    col_3 = rep_len(segmented_words[3],col_3_length)
                    col_4_length = nrow(tri_grams)
                    col_4 = rep_len(segmented_words[4],col_4_length)
                    col_5_length = nrow(tri_grams)
                    col_5 = rep_len(segmented_words[5],col_5_length)
                    col_6_length = nrow(tri_grams)
                    col_6 = rep_len(segmented_words[6],col_6_length)
                    temp = cbind(col_1, col_2)
                    temp2 = cbind(temp, col_3)
                    temp3 = cbind(temp2, col_4)
                    temp4 = cbind(temp3, col_5)
                    temp5 = cbind(temp4, col_6)
                    n_gram_data <- cbind(temp5, tri_grams)
                    rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                    colnames(n_gram_data) <- c("first","second","third","fourth","fifth", "sixth", "seventh", "eighth", "ninth")
                    n_gram_data
                  }else{
                    n_gram_data <- data.frame(first=character(),second=character(),
                                              third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(),  eighth=character(), ninth=character(), stringsAsFactors = FALSE)
                    n_gram_data[1,1] = "this"
                    n_gram_data[1,2] = "not"
                    n_gram_data[1,3] = "handledB"
                    n_gram_data[1,4] = "yes"
                    n_gram_data[1,5] = "yes"
                    n_gram_data[1,6] = "yes"
                    n_gram_data[1,7] = "yes"
                    n_gram_data[1,8] = "yes"
                    n_gram_data[1,9] = "yes"
                    rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                    n_gram_data
                  }
                }else if((nrow(quad_grams)>0)==TRUE){
                  col_1_length = nrow(quad_grams)
                  col_1 = rep_len(segmented_words[1],col_1_length)
                  col_2_length = nrow(quad_grams)
                  col_2 = rep_len(segmented_words[2],col_2_length)
                  col_3_length = nrow(quad_grams)
                  col_3 = rep_len(segmented_words[3],col_3_length)
                  col_4_length = nrow(quad_grams)
                  col_4 = rep_len(segmented_words[4],col_4_length)
                  col_5_length = nrow(quad_grams)
                  col_5 = rep_len(segmented_words[5],col_5_length)
                  temp = cbind(col_1, col_2)
                  temp2 = cbind(temp, col_3)
                  temp3 = cbind(temp2, col_4)
                  temp4 = cbind(temp3, col_5)
                  n_gram_data <- cbind(temp4, quad_grams)
                  rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                  colnames(n_gram_data) <- c("first","second","third","fourth","fifth", "sixth", "seventh", "eighth", "ninth")
                  n_gram_data
                }else{
                  n_gram_data <- data.frame(first=character(),second=character(),
                                            third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(), eighth=character(), ninth=character(), stringsAsFactors = FALSE)
                  n_gram_data[1,1] = "this"
                  n_gram_data[1,2] = "not"
                  n_gram_data[1,3] = "handledC"
                  n_gram_data[1,4] = "yes"
                  n_gram_data[1,5] = "yes"
                  n_gram_data[1,6] = "yes"
                  n_gram_data[1,7] = "yes"
                  n_gram_data[1,8] = "yes"
                  n_gram_data[1,9] = "yes"
                  rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                  n_gram_data
                }
              }else if((nrow(penta_grams)>0)==TRUE){
                
          
                    
                
                    
                
                col_1_length = nrow(penta_grams)
                col_1 = rep_len(segmented_words[1],col_1_length)
                col_2_length = nrow(penta_grams)
                col_2 = rep_len(segmented_words[2],col_2_length)
                col_3_length = nrow(penta_grams)
                col_3 = rep_len(segmented_words[3],col_3_length)
                col_4_length = nrow(penta_grams)
                col_4 = rep_len(segmented_words[4],col_4_length)
                temp = cbind(col_1, col_2)
                temp2 = cbind(temp, col_3)
                temp3 = cbind(temp2, col_4)
                n_gram_data <- cbind(temp3, penta_grams)
                rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                colnames(n_gram_data) <- c("first","second","third","fourth","fifth","sixth","seventh", "eighth", "ninth")
                n_gram_data
              }else{
                n_gram_data <- data.frame(first=character(),second=character(),
                                          third=character(), fourth=character(), fifth=character(),
                                          sixth=character(), seventh=character(), eighth=character(), ninth=character(), stringsAsFactors = FALSE)
                n_gram_data[1,1] = "this"
                n_gram_data[1,2] = "not"
                n_gram_data[1,3] = "handledC"
                n_gram_data[1,4] = "yes"
                n_gram_data[1,5] = "yes"
                n_gram_data[1,6] = "yes"
                n_gram_data[1,7] = "yes"
                n_gram_data[1,8] = "yes"
                n_gram_data[1,9] = "yes"
                rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
                n_gram_data
              }
            }else if((nrow(hexa_grams)>0)==TRUE){
              col_1_length = nrow(hexa_grams)
              col_1 = rep_len(segmented_words[1],col_1_length)
              col_2_length = nrow(hexa_grams)
              col_2 = rep_len(segmented_words[2],col_2_length)
              col_3_length = nrow(hexa_grams)
              col_3 = rep_len(segmented_words[3],col_3_length)
              temp <- cbind(col_1, col_2)
              temp2 <- cbind(temp, col_3)
              n_gram_data <- cbind(temp2, hexa_grams)
              rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
              colnames(n_gram_data) <- c("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth")
              n_gram_data[,1:9]
            }else{
              n_gram_data <- data.frame(first=character(),second=character(),
                                        third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(), eighth=character(), ninth=character(),  stringsAsFactors = FALSE)
              n_gram_data[1,1] = "this"
              n_gram_data[1,2] = "not"
              n_gram_data[1,3] = "yes"
              n_gram_data[1,4] = "yes"
              n_gram_data[1,5] = "yes"
              n_gram_data[1,6] = "yes"
              n_gram_data[1,7] = "yes"
              n_gram_data[1,8] = "yes"
              n_gram_data[1,9] = "yes"
              rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
              n_gram_data
            }
          }else if((nrow(hepta_grams)>0)==TRUE){
            col_1_length = nrow(hepta_grams)
            col_1 = rep_len(segmented_words[1],col_1_length)
            col_2_length = nrow(hepta_grams)
            col_2 = rep_len(segmented_words[2],col_2_length)
            temp <- cbind(col_1, col_2)
            n_gram_data <- cbind(temp, hepta_grams)
            rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
            colnames(n_gram_data) <- c("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth")
            n_gram_data[,1:9]
          }else{
            n_gram_data <- data.frame(first=character(),second=character(),
                                      third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(), eighth=character(), ninth=character(), stringsAsFactors = FALSE)
            n_gram_data[1,1] = "this"
            n_gram_data[1,2] = "not"
            n_gram_data[1,3] = "problem"
            n_gram_data[1,4] = "yes"
            n_gram_data[1,5] = "yes"
            n_gram_data[1,6] = "yes"
            n_gram_data[1,7] = "yes"
            n_gram_data[1,8] = "yes"
            n_gram_data[1,9] = "yes"
            rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
            n_gram_data
          }
        }else if((nrow(octo_grams)>0)==TRUE){
          col_1_length = nrow(octo_grams)
          col_1 = rep_len(segmented_words[1],col_1_length)
          n_gram_data <- cbind(col_1, octo_grams)
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          colnames(n_gram_data) <- c("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth")
          n_gram_data[,1:9]
        }else{
          n_gram_data <- data.frame(first=character(),second=character(),
                                    third=character(), fourth=character(), fifth=character(), sixth=character(), seventh=character(), eighth=character(), ninth=character(), stringsAsFactors = FALSE)
          n_gram_data[1,1] = "this"
          n_gram_data[1,2] = "not"
          n_gram_data[1,3] = "problem"
          n_gram_data[1,4] = "yes"
          n_gram_data[1,5] = "yes"
          n_gram_data[1,6] = "yes"
          n_gram_data[1,7] = "yes"
          n_gram_data[1,8] = "yes"
          n_gram_data[1,9] = "yes"
          rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
          n_gram_data
        }
      }else if((nrow(nona_grams)>0)==TRUE){ #works
        n_gram_data <- nona_grams
        rownames(n_gram_data) <- seq(length=nrow(n_gram_data))
        colnames(n_gram_data) <- c("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth")
        n_gram_data[,1:9]
      }else{
        print("help")
      } 
      }else{
      n_gram_data <- data.frame(first=character(),second=character(), stringsAsFactors = FALSE)
      n_gram_data[1,1] = c("Please")
      n_gram_data[1,2] = c("enter")
      n_gram_data[1,3] = c("words")
    }, options = list(lengthChange = FALSE))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #####################################################
  
  
  

  output$line = renderText({
    
    if((countWords(input$words_in)>8)==TRUE){
      problem <- paste("Please","enter","eight", "or", "less", "words")
      n_gram_data <- problem
    }else if((countWords(input$words_in)==0)==TRUE){
      problem <- paste("Please","enter","words")
      n_gram_data <- problem
    }else if((countWords(input$words_in)==1)==TRUE){
      temp = as.character(input$words_in)
      temp2 = str_replace_all(temp, '[:punct:]', '')
      bi_grams <- biGrams(input$data_source, temp2)
      if((nrow(bi_grams)==0)==TRUE){
        n_gram_data <- paste(input$words_in,"is")
        n_gram_data
      }else if((nrow(bi_grams)>0)==TRUE){
        n_gram_data <- paste(bi_grams[1,1],bi_grams[1,2])
        n_gram_data
      }else{
        n_gram_data <- paste("not","handled15")
        n_gram_data
      }
    }else if((countWords(input$words_in)==2)==TRUE){
      tri_grams <- triGrams(input$data_source, input$words_in)
      if((nrow(tri_grams)==0)==TRUE){
        segmented_words = segmentWords(input$words_in)
        to_drive_bigrams = unlist(segmented_words[2])
        bi_grams = biGrams(input$data_source, to_drive_bigrams)
        if((nrow(bi_grams)==0)==TRUE){
          first = as.character(segmented_words[1])
          second = as.character(segmented_words[2])
          third = "is"
          n_gram_data = paste(first,second,third)
          n_gram_data
        }else if((nrow(bi_grams)>0)==TRUE){
          first = as.character(segmented_words[1])
          second = as.character(segmented_words[2])
          third = as.character(bi_grams[1,2])
          n_gram_data = paste(first,second,third)
          n_gram_data
        }else{
          n_gram_data <- paste("not","handled16")
          n_gram_data
        }
      }else if((nrow(tri_grams)>0)==TRUE){
        n_gram_data <- paste(tri_grams[1,1],tri_grams[1,2], tri_grams[1,3])
        n_gram_data
      }else{
        n_gram_data <- paste("not","handled17")
        n_gram_data
      }
    }else if((countWords(input$words_in)==3)==TRUE){
      quad_grams <- quadGrams(input$data_source, input$words_in)
      segmented_words = segmentWords(input$words_in)
      if((nrow(quad_grams)==0)==TRUE){
        to_drive_trigrams = paste(unlist(segmented_words[2]), unlist(segmented_words[3]))
        tri_grams = triGrams(input$data_source, to_drive_trigrams)
        if((nrow(tri_grams)==0)==TRUE){
          #segmented_words = segmentWords(input$words_in)
          to_drive_bigrams = unlist(segmented_words[3])
          bi_grams = biGrams(input$data_source, to_drive_bigrams)
          if((nrow(bi_grams)==0)==TRUE){
            n_gram_data <- paste(segmented_words[1], segmented_words[2], segmented_words[3], "is")
            n_gram_data
          }else if((nrow(bi_grams)>0)==TRUE){
            first = as.character(segmented_words[1])
            second = as.character(segmented_words[2])
            third = as.character(segmented_words[3])
            fourth = as.character(bi_grams[1,2])
            n_gram_data = paste(first, second, third, fourth)
            n_gram_data
          }else{
            n_gram_data <- paste("not","handled18")
            n_gram_data
          }
        }else if((nrow(tri_grams)>0)==TRUE){
          #segmented_words = segmentWords(input$words_in)
          first = as.character(segmented_words[1])
          n_gram_data <- paste(first, tri_grams[1,1],tri_grams[1,2], tri_grams[1,3])
          n_gram_data
        }else{
          n_gram_data <- paste("not","handled19")
          n_gram_data
        }
      }else if((nrow(quad_grams)>0)==TRUE){
        n_gram_data <- paste(quad_grams[1,1],quad_grams[1,2], quad_grams[1,3], quad_grams[1,4])
        n_gram_data
      }else{
        n_gram_data <- paste("not","handled20")
        n_gram_data
      }
    }else if((countWords(input$words_in)==4)==TRUE){
      penta_grams <- pentaGrams(input$data_source, input$words_in)
      segmented_words = segmentWords(input$words_in)
      if((nrow(penta_grams)==0)==TRUE){
        to_drive_quadgrams = paste(unlist(segmented_words[2]), unlist(segmented_words[3]), unlist(segmented_words[4]))
        quad_grams <- quadGrams(input$data_source, to_drive_quadgrams)
        if((nrow(quad_grams)==0)==TRUE){
          segmented_words = segmentWords(input$words_in)
          to_drive_trigrams = paste(unlist(segmented_words[3]), unlist(segmented_words[4]))
          tri_grams = triGrams(input$data_source, to_drive_trigrams)
          if((nrow(tri_grams)==0)==TRUE){
            segmented_words = segmentWords(input$words_in)
            to_drive_bigrams = unlist(segmented_words[4])
            bi_grams = biGrams(input$data_source, to_drive_bigrams)
            if((nrow(bi_grams)==0)==TRUE){
              first = as.character(segmented_words[1])
              second = as.character(segmented_words[2])
              third = as.character(segmented_words[3])
              fourth = as.character(segmented_words[4])
              fifth = "is"
              n_gram_data = paste(first, second, third, fourth, fifth)
            }else if((nrow(bi_grams)>0)==TRUE){
              n_gram_data <- paste(bi_grams[1,1],bi_grams[1,2])
              segmented_words_2 = segmentWords(n_gram_data)
              first = as.character(segmented_words[1])
              second = as.character(segmented_words[2])
              third = as.character(segmented_words[3])
              fourth = as.character(segmented_words_2[1])
              fifth = as.character(segmented_words_2[2])
              n_gram_data = paste(first, second, third, fourth, fifth)
            }else{
              n_gram_data <- paste("not","handled21","yet")
              n_gram_data
            }
          }else if((nrow(tri_grams)>0)==TRUE){
            n_gram_data <- paste(tri_grams[1,1],tri_grams[1,2], tri_grams[1,3])
            n_gram_data = paste(segmented_words[1], segmented_words[2], n_gram_data)
            n_gram_data
          }else{
            n_gram_data <- paste("not","handled22","yet")
            n_gram_data
          }
        }else if((nrow(quad_grams)>0)==TRUE){
          n_gram_data <- paste(quad_grams[1,1], quad_grams[1,2], quad_grams[1,3], quad_grams[1,4])
          n_gram_data = paste(segmented_words[1],n_gram_data)
          n_gram_data
        }else{
          n_gram_data <- paste("not","handled23","yet")
          n_gram_data
        }
      }else if((nrow(penta_grams)>0)==TRUE){
        n_gram_data = paste(penta_grams[1,1], penta_grams[1,2], penta_grams[1,3], penta_grams[1,4], penta_grams[1,5])
        n_gram_data
      }else{
        n_gram_data <- paste("not","handled24","yet")
        n_gram_data
      }
    }else if((countWords(input$words_in)==5)==TRUE){
      hexa_grams <- hexaGrams(input$data_source, input$words_in[1])
      segmented_words = segmentWords(input$words_in)
      if((nrow(hexa_grams)==0)==TRUE){
        to_drive_pentagrams = paste(unlist(segmented_words[2]), unlist(segmented_words[3]), unlist(segmented_words[4])
                                    , unlist(segmented_words[5]))
        penta_grams <- pentaGrams(input$data_source, to_drive_pentagrams)
        if((nrow(penta_grams)==0)==TRUE){
          to_drive_quadgrams = paste(unlist(segmented_words[3]), unlist(segmented_words[4]), unlist(segmented_words[5]))
          quad_grams <- quadGrams(input$data_source, to_drive_quadgrams)
          if((nrow(quad_grams)==0)==TRUE){
            to_drive_trigrams = paste(unlist(segmented_words[4]), unlist(segmented_words[5]))
            tri_grams = triGrams(input$data_source, to_drive_trigrams)
            if((nrow(tri_grams)==0)==TRUE){
              to_drive_bigrams = unlist(segmented_words[5])
              bi_grams = biGrams(input$data_source, to_drive_bigrams)
              if((nrow(bi_grams)==0)==TRUE){
                first = as.character(segmented_words[1])
                second = as.character(segmented_words[2])
                third = as.character(segmented_words[3])
                fourth = as.character(segmented_words[4])
                fifth = as.character(segmented_words[5])
                sixth = "is"
                n_gram_data = paste(first, second, third, fourth, fifth, sixth)
              }else if((nrow(bi_grams)>0)==TRUE){
                n_gram_data <- paste(bi_grams[1,1],bi_grams[1,2])
                segmented_words_2 = segmentWords(n_gram_data)
                first = as.character(segmented_words[1])
                second = as.character(segmented_words[2])
                third = as.character(segmented_words[3])
                fourth = as.character(segmented_words[4])
                fifth = as.character(segmented_words_2[1])
                sixth = as.character(segmented_words_2[2])
                n_gram_data = paste(first, second, third, fourth, fifth, sixth)
                
              }else{
                n_gram_data <- paste("Error number #.")
                n_gram_data
              }
            }else if((nrow(tri_grams)>0)==TRUE){
              n_gram_data <- paste(tri_grams[1,1],tri_grams[1,2], tri_grams[1,3])
              n_gram_data = paste(segmented_words[1], segmented_words[2], segmented_words[3],n_gram_data)
              n_gram_data
              
            }else{
              n_gram_data <- paste("Error number #.")
              n_gram_data
            }
          }else if((nrow(quad_grams)>0)==TRUE){ #works
            n_gram_data <- paste(quad_grams[1,1], quad_grams[1,2], quad_grams[1,3], quad_grams[1,4])
            n_gram_data = paste(segmented_words[1],segmented_words[2], n_gram_data)
            n_gram_data
          }else{
            n_gram_data <- paste("Error number #.")
            n_gram_data
          }
        }else if((nrow(penta_grams)>0)==TRUE){
          n_gram_data =  paste(segmented_words[1],penta_grams[1,1], penta_grams[1,2], penta_grams[1,3], penta_grams[1,4], penta_grams[1,5])
          n_gram_data
        }else{
          n_gram_data <- paste("Error number #.")
          n_gram_data
        }
      }else if((nrow(hexa_grams)>0)==TRUE){ #works
        n_gram_data <- paste(hexa_grams[1,1], hexa_grams[1,2], hexa_grams[1,3], hexa_grams[1,4], hexa_grams[1,5], hexa_grams[1,6])
        n_gram_data
      }else{
        n_gram_data <- paste("Error number #.")
        n_gram_data
      }
  }else if((countWords(input$words_in)==6)==TRUE){
    hepta_grams <- heptaGrams(input$data_source, input$words_in[1])
    segmented_words = segmentWords(input$words_in)
    if((nrow(hepta_grams)==0)==TRUE){
      to_drive_hexagrams = paste(unlist(segmented_words[2]),unlist(segmented_words[3]), unlist(segmented_words[4]), unlist(segmented_words[5])
                                  , unlist(segmented_words[6]))
    hexa_grams <- hexaGrams(input$data_source, to_drive_hexagrams)
    segmented_words = segmentWords(input$words_in)
    if((nrow(hexa_grams)==0)==TRUE){
      to_drive_pentagrams = paste(unlist(segmented_words[3]), unlist(segmented_words[4]), unlist(segmented_words[5])
                                  , unlist(segmented_words[6]))
      penta_grams <- pentaGrams(input$data_source, to_drive_pentagrams)
      if((nrow(penta_grams)==0)==TRUE){
        to_drive_quadgrams = paste(unlist(segmented_words[4]), unlist(segmented_words[5]), unlist(segmented_words[6]))
        quad_grams <- quadGrams(input$data_source, to_drive_quadgrams)
        if((nrow(quad_grams)==0)==TRUE){
          to_drive_trigrams = paste(unlist(segmented_words[5]), unlist(segmented_words[6]))
          tri_grams = triGrams(input$data_source, to_drive_trigrams)
          if((nrow(tri_grams)==0)==TRUE){
            to_drive_bigrams = unlist(segmented_words[6])
            bi_grams = biGrams(input$data_source, to_drive_bigrams)
            if((nrow(bi_grams)==0)==TRUE){
              first = as.character(segmented_words[1])
              second = as.character(segmented_words[2])
              third = as.character(segmented_words[3])
              fourth = as.character(segmented_words[4])
              fifth = as.character(segmented_words[5])
              sixth = as.character(segmented_words[6])
              seventh = "is"
              n_gram_data = paste(first, second, third, fourth, fifth, sixth, seventh)
            }else if((nrow(bi_grams)>0)==TRUE){
              n_gram_data <- paste(bi_grams[1,1],bi_grams[1,2])
              segmented_words_2 = segmentWords(n_gram_data)
              first = as.character(segmented_words[1])
              second = as.character(segmented_words[2])
              third = as.character(segmented_words[3])
              fourth = as.character(segmented_words[4])
              fifth = as.character(segmented_words[5])
              sixth = as.character(segmented_words_2[1])
              seventh = as.character(segmented_words_2[2])
              n_gram_data = paste(first, second, third, fourth, fifth, sixth, seventh)
              
            }else{
              n_gram_data <- paste("Error number #.")
              n_gram_data
            }
          }else if((nrow(tri_grams)>0)==TRUE){
            n_gram_data <- paste(tri_grams[1,1],tri_grams[1,2], tri_grams[1,3])
            n_gram_data = paste(segmented_words[1], segmented_words[2], segmented_words[3], segmented_words[4], n_gram_data)
            n_gram_data
            
          }else{
            n_gram_data <- paste("Error number #.")
            n_gram_data
          }
        }else if((nrow(quad_grams)>0)==TRUE){ #works
          n_gram_data <- paste(quad_grams[1,1], quad_grams[1,2], quad_grams[1,3], quad_grams[1,4])
          n_gram_data = paste(segmented_words[1],segmented_words[2],segmented_words[3], n_gram_data)
          n_gram_data
        }else{
          n_gram_data <- paste("Error number #.")
          n_gram_data
        }
      }else if((nrow(penta_grams)>0)==TRUE){
        n_gram_data =  paste(segmented_words[1],penta_grams[1,1], penta_grams[1,2], penta_grams[1,3], penta_grams[1,4], penta_grams[1,5])
        n_gram_data = paste(segmented_words[1],segmented_words[2], n_gram_data)
        n_gram_data
      }else{
        n_gram_data <- paste("Error number #.")
        n_gram_data
      }
    }else if((nrow(hexa_grams)>0)==TRUE){
      n_gram_data <- paste(hexa_grams[1,1], hexa_grams[1,2], hexa_grams[1,3], hexa_grams[1,4], hexa_grams[1,5], hexa_grams[1,6])
      n_gram_data = paste(segmented_words[1], n_gram_data)
      n_gram_data
    }else{
      n_gram_data <- paste("Error number #.")
      n_gram_data
    }
    }else if((nrow(hepta_grams)>0)==TRUE){
      n_gram_data <- paste(hepta_grams[1,1], hepta_grams[1,2], hepta_grams[1,3], hepta_grams[1,4], hepta_grams[1,5], hepta_grams[1,6], hepta_grams[1,7])
      n_gram_data
    }else{
      n_gram_data <- paste("Error number #.")
      n_gram_data
      
    }
  }else if((countWords(input$words_in)==7)==TRUE){
    octo_grams <- octoGrams(input$data_source, input$words_in[1])
    segmented_words = segmentWords(input$words_in)
    if((nrow(octo_grams)==0)==TRUE){
    to_drive_hepta_grams = paste(unlist(segmented_words[2]), unlist(segmented_words[3]), unlist(segmented_words[4]), unlist(segmented_words[5])
                                   , unlist(segmented_words[6],  unlist(segmented_words[7])))
    hepta_grams <- heptaGrams(input$data_source, to_drive_hepta_grams)
    if((nrow(hepta_grams)==0)==TRUE){
      to_drive_hexagrams = paste(unlist(segmented_words[4]), unlist(segmented_words[5]), unlist(segmented_words[6])
                                 , unlist(segmented_words[7]))
      hexa_grams <- hexaGrams(input$data_source, to_drive_hexagrams)
      segmented_words = segmentWords(input$words_in)
      if((nrow(hexa_grams)==0)==TRUE){
        to_drive_pentagrams = paste(unlist(segmented_words[4]), unlist(segmented_words[5]), unlist(segmented_words[6])
                                    , unlist(segmented_words[7]))
        penta_grams <- pentaGrams(input$data_source, to_drive_pentagrams)
        if((nrow(penta_grams)==0)==TRUE){
          to_drive_quadgrams = paste(unlist(segmented_words[5]), unlist(segmented_words[6]), unlist(segmented_words[7]))
          quad_grams <- quadGrams(input$data_source, to_drive_quadgrams)
          if((nrow(quad_grams)==0)==TRUE){
            to_drive_trigrams = paste(unlist(segmented_words[6]), unlist(segmented_words[7]))
            tri_grams = triGrams(input$data_source, to_drive_trigrams)
            if((nrow(tri_grams)==0)==TRUE){
              to_drive_bigrams = unlist(segmented_words[7])
              bi_grams = biGrams(input$data_source, to_drive_bigrams)
              if((nrow(bi_grams)==0)==TRUE){
                first = as.character(segmented_words[1])
                second = as.character(segmented_words[2])
                third = as.character(segmented_words[3])
                fourth = as.character(segmented_words[4])
                fifth = as.character(segmented_words[5])
                sixth = as.character(segmented_words[6])
                seventh = "is"
                n_gram_data = paste(first, second, third, fourth, fifth, sixth, seventh)
              }else if((nrow(bi_grams)>0)==TRUE){
                n_gram_data <- paste(bi_grams[1,1],bi_grams[1,2])
                segmented_words_2 = segmentWords(n_gram_data)
                first = as.character(segmented_words[1])
                second = as.character(segmented_words[2])
                third = as.character(segmented_words[3])
                fourth = as.character(segmented_words[4])
                fifth = as.character(segmented_words[5])
                sixth = as.character(segmented_words_2[1])
                seventh = as.character(segmented_words_2[2])
                n_gram_data = paste(first, second, third, fourth, fifth, sixth, seventh)
                
              }else{
                n_gram_data <- paste("Error number #.")
                n_gram_data
              }
            }else if((nrow(tri_grams)>0)==TRUE){
              n_gram_data <- paste(tri_grams[1,1],tri_grams[1,2], tri_grams[1,3])
              n_gram_data = paste(segmented_words[1], segmented_words[2], segmented_words[3], segmented_words[4], segmented_words[5], n_gram_data)
              n_gram_data
              
            }else{
              n_gram_data <- paste("Error number #.")
              n_gram_data
            }
          }else if((nrow(quad_grams)>0)==TRUE){ 
            n_gram_data <- paste(quad_grams[1,1], quad_grams[1,2], quad_grams[1,3], quad_grams[1,4], quad_grams[1,5])
            n_gram_data = paste(segmented_words[1],segmented_words[2], segmented_words[3],n_gram_data)
            n_gram_data
          }else{
            n_gram_data <- paste("Error number #.")
            n_gram_data
          }
        }else if((nrow(penta_grams)>0)==TRUE){
          n_gram_data =  paste(segmented_words[1],penta_grams[1,1], penta_grams[1,2], penta_grams[1,3], penta_grams[1,4], penta_grams[1,5], penta_grams[1,6])
          n_gram_data = paste(segmented_words[1],segmented_words[2], n_gram_data)
          n_gram_data
        }else{
          n_gram_data <- paste("Error number #.")
          n_gram_data
        }
      }else if((nrow(hexa_grams)>0)==TRUE){
        n_gram_data <- paste(hexa_grams[1,1], hexa_grams[1,2], hexa_grams[1,3], hexa_grams[1,4], hexa_grams[1,5], hexa_grams[1,6])
        n_gram_data = paste(segmented_words[1], segmented_words[2], n_gram_data)
        n_gram_data
      }else{
        n_gram_data <- paste("Error number #.")
        n_gram_data
      }
    }else if((nrow(hepta_grams)>0)==TRUE){
      n_gram_data <- paste(hepta_grams[1,1], hepta_grams[1,2], hepta_grams[1,3], hepta_grams[1,4], hepta_grams[1,5], hepta_grams[1,6], hepta_grams[1,7])
      n_gram_data = paste(segmented_words[1], n_gram_data)
    }else{
      n_gram_data <- paste("Error number #.")
      n_gram_data
      
    }
    }else if((nrow(octo_grams)>0)==TRUE){
      n_gram_data <- paste(octo_grams[1,1], octo_grams[1,2], octo_grams[1,3], octo_grams[1,4], octo_grams[1,5], octo_grams[1,6], octo_grams[1,7], octo_grams[1,8])
      n_gram_data
      
    }else{
      n_gram_data <- paste("Error number #.")
      n_gram_data
      
    }  
    
  }else if((countWords(input$words_in)==8)==TRUE){
    nona_grams <- nonaGrams(input$data_source, input$words_in[1])
    
    if((nrow(nona_grams)==0)==TRUE){
      segmented_words = segmentWords(input$words_in)
      to_drive_octo_grams = paste(unlist(segmented_words[2]), unlist(segmented_words[3]), unlist(segmented_words[4]), unlist(segmented_words[5]), unlist(segmented_words[6])
                                   , unlist(segmented_words[7],  unlist(segmented_words[8])))
    octo_grams <- octoGrams(input$data_source, to_drive_octo_grams)
    segmented_words = segmentWords(input$words_in)
    if((nrow(octo_grams)==0)==TRUE){
      to_drive_hepta_grams = paste(unlist(segmented_words[2]), unlist(segmented_words[3]), unlist(segmented_words[4]), unlist(segmented_words[5]), unlist(segmented_words[6])
                                   , unlist(segmented_words[7],  unlist(segmented_words[8])))
      hepta_grams <- heptaGrams(input$data_source, to_drive_hepta_grams)
      if((nrow(hepta_grams)==0)==TRUE){
        to_drive_hexagrams = paste(unlist(segmented_words[3]),unlist(segmented_words[4]), unlist(segmented_words[5]), unlist(segmented_words[6]), unlist(segmented_words[7])
                                   , unlist(segmented_words[8]))
        hexa_grams <- hexaGrams(input$data_source, to_drive_hexagrams)
        segmented_words = segmentWords(input$words_in)
        if((nrow(hexa_grams)==0)==TRUE){
          to_drive_pentagrams = paste(unlist(segmented_words[5]), unlist(segmented_words[6]), unlist(segmented_words[7])
                                      , unlist(segmented_words[8]))
          penta_grams <- pentaGrams(input$data_source, to_drive_pentagrams)
          if((nrow(penta_grams)==0)==TRUE){
            to_drive_quadgrams = paste(unlist(segmented_words[6]), unlist(segmented_words[7]), unlist(segmented_words[8]))
            quad_grams <- quadGrams(input$data_source, to_drive_quadgrams)
            if((nrow(quad_grams)==0)==TRUE){
              to_drive_trigrams = paste(unlist(segmented_words[7]), unlist(segmented_words[8]))
              tri_grams = triGrams(input$data_source, to_drive_trigrams)
              if((nrow(tri_grams)==0)==TRUE){
                to_drive_bigrams = unlist(segmented_words[8])
                bi_grams = biGrams(input$data_source, to_drive_bigrams)
                if((nrow(bi_grams)==0)==TRUE){
                  first = as.character(segmented_words[1])
                  second = as.character(segmented_words[2])
                  third = as.character(segmented_words[3])
                  fourth = as.character(segmented_words[4])
                  fifth = as.character(segmented_words[5])
                  sixth = as.character(segmented_words[6])
                  seventh = as.character(segmented_words[7])
                  eighth = as.character(segmented_words[8])
                  ninth = "is"
                  n_gram_data = paste(first, second, third, fourth, fifth, sixth, seventh)
                }else if((nrow(bi_grams)>0)==TRUE){
                  n_gram_data <- paste(bi_grams[1,1],bi_grams[1,2])
                  segmented_words_2 = segmentWords(n_gram_data)
                  first = as.character(segmented_words[1])
                  second = as.character(segmented_words[2])
                  third = as.character(segmented_words[3])
                  fourth = as.character(segmented_words[4])
                  fifth = as.character(segmented_words[5])
                  sixth = as.character(segmented_words[6])
                  seventh = as.character(segmented_words[7])
                  eighth = as.character(segmented_words_2[1])
                  ninth = as.character(segmented_words_2[2])
                  n_gram_data = paste(first, second, third, fourth, fifth, sixth, seventh, eighth, ninth)
                  
                }else{
                  n_gram_data <- paste("Error number #.")
                  n_gram_data
                }
              }else if((nrow(tri_grams)>0)==TRUE){
                n_gram_data <- paste(tri_grams[1,1],tri_grams[1,2], tri_grams[1,3])
                n_gram_data = paste(segmented_words[1], segmented_words[2], segmented_words[3], segmented_words[4], segmented_words[5], segmented_words[6], n_gram_data)
                n_gram_data
                
              }else{
                n_gram_data <- paste("Error number #.")
                n_gram_data
              }
            }else if((nrow(quad_grams)>0)==TRUE){ 
              n_gram_data <- paste(quad_grams[1,1], quad_grams[1,2], quad_grams[1,3], quad_grams[1,4])
              n_gram_data = paste(segmented_words[1],segmented_words[2], segmented_words[3], segmented_words[4], segmented_words[5],n_gram_data)
              n_gram_data
            }else{
              n_gram_data <- paste("Error number #.")
              n_gram_data
            }
          }else if((nrow(penta_grams)>0)==TRUE){
            n_gram_data <- paste(penta_grams[1,1], penta_grams[1,2], penta_grams[1,3], penta_grams[1,4], penta_grams[1,5])
            n_gram_data = paste(segmented_words[1],segmented_words[2], segmented_words[3], segmented_words[4], n_gram_data)
            n_gram_data
          }else{
            n_gram_data <- paste("Error number #.")
            n_gram_data
          }
        }else if((nrow(hexa_grams)>0)==TRUE){
          n_gram_data =  paste(hexa_grams[1,1], hexa_grams[1,2], hexa_grams[1,3], hexa_grams[1,4], hexa_grams[1,5], hexa_grams[1,6])
          n_gram_data = paste(segmented_words[1],segmented_words[2], segmented_words[3],n_gram_data)
          n_gram_data
        }else{
          n_gram_data <- paste("Error number #.")
          n_gram_data
        }
      }else if((nrow(hepta_grams)>0)==TRUE){
        n_gram_data <- paste(hepta_grams[1,1], hepta_grams[1,2], hepta_grams[1,3], hepta_grams[1,4], hepta_grams[1,5], hepta_grams[1,6], hepta_grams[1,7])
        n_gram_data = paste(segmented_words[1], segmented_words[2], n_gram_data)
        n_gram_data
      }else{
        n_gram_data <- paste("Error number #.")
        n_gram_data
        
      }
    }else if((nrow(octo_grams)>0)==TRUE){
      n_gram_data <- paste(octo_grams[1,1], octo_grams[1,2], octo_grams[1,3], octo_grams[1,4], octo_grams[1,5], octo_grams[1,6], octo_grams[1,7], octo_grams[1,8])
      n_gram_data = paste(segmented_words[1], n_gram_data)
      
    }else{
      n_gram_data <- paste("Error number #.")
      n_gram_data
    }
    }else if((nrow(nona_grams)>0)==TRUE){
      n_gram_data <- paste(nona_grams[1,1], nona_grams[1,2], nona_grams[1,3], nona_grams[1,4], nona_grams[1,5], nona_grams[1,6], nona_grams[1,7], nona_grams[1,8], nona_grams[1,9])
      n_gram_data
    }else{
      n_gram_data <- paste("Error number #.")
      n_gram_data
    }
  }      ##end of renderText function below
    
}
)
  
  #shiny server function
  
}
)
