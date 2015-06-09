#server.R
library(shiny)
library(plyr)
library(doBy)
library(data.table)
library(shinyapps)

shinyServer(function(input, output) {
  
  ###biGrams
  
  
  
  bData = reactive({
    
    if(input$data_source=="blogs"){
      twoGrams_DF <- read.table("b2.txt",sep=",",quote="")
    }else if(input$data_source=="news"){
      twoGrams_DF <- read.table("n2.txt",sep=",",quote="")
    }else{
      twoGrams_DF <- read.table("t2.txt",sep=",",quote="")
    }
    

    twoGrams_DF <- setnames(twoGrams_DF, c("V1", "V2", "V3"), c("first", "second","count"))
    twoGrams_DF$first<-as.character(twoGrams_DF$first)
    twoGrams_DF$second<-as.character(twoGrams_DF$second)
    #twoGrams_DF$count<-as.numeric(twoGrams_DF$count)
    twoGrams_DF<- subset(twoGrams_DF, select=c(first,second,count)) 
    
    bdf <- twoGrams_DF[which(twoGrams_DF$first==input$bword),]
    bdf<-orderBy(~-count, data=bdf)
    bsum <- sum(bdf$count)
    bdf$frequency<-bdf$count / bsum
    #print(bsum)
    bdf <- bdf[1:20,]
    return(list(bdf=bdf))
    
  })
  
  output$biGrams <- renderTable({
    if (is.null(bData())) {return()}
    print(bData()$bdf)
  }, 'include.rownames' = FALSE
  , 'include.colnames' = TRUE
  , 'sanitize.text.function' = function(x){x}
  , digits = 4)
  
  ###triGrams
  
tData = reactive ({
  
  if(input$data_source=="blogs"){
    threeGrams_DF <- read.table("b3.txt",sep=",",quote="")
  }else if(input$data_source=="news"){
    threeGrams_DF <- read.table("n3.txt",sep=",",quote="")
  }else{
    threeGrams_DF <- read.table("t3.txt",sep=",",quote="")
  }
  
  threeGrams_DF <- setnames(threeGrams_DF, c("V1", "V2", "V3", "V4"), c("first", "second", "third", "count"))
  threeGrams_DF$first<-as.character(threeGrams_DF$first)
  threeGrams_DF$second<-as.character(threeGrams_DF$second)
  threeGrams_DF$third<-as.character(threeGrams_DF$third)
  threeGrams_DF<- subset(threeGrams_DF, select=c(first,second,third,count)) 
  
  #print(input$twords)
  triWords <- strsplit(input$twords," ")
  triWords <- unlist(triWords)
  #print(triWords)
  #print(triWords[1])
  #print(triWords[2])
  tdf <- threeGrams_DF[which(threeGrams_DF$first==triWords[1] & threeGrams_DF$second==triWords[2]),]
  tdf<-orderBy(~-count, data=tdf)
  tsum <- sum(tdf$count)
  tdf$frequency<-tdf$count / tsum
  tdf <- tdf[1:20,]
  return(list(tdf=tdf))
})

output$triGrams <- renderTable({
  if (is.null(tData())) {return()}
  print(tData()$tdf)
}, 'include.rownames' = FALSE
, 'include.colnames' = TRUE
, 'sanitize.text.function' = function(x){x}
, digits = 4)
 
qData = reactive ({
  
  if(input$data_source=="blogs"){
    fourGrams_DF <- read.table("b4.txt",sep=",",quote="")
  }else if(input$data_source=="news"){
    fourGrams_DF <- read.table("n4.txt",sep=",",quote="")
  }else{
    fourGrams_DF <- read.table("t4.txt",sep=",",quote="")
  }

  fourGrams_DF <- setnames(fourGrams_DF, c("V1", "V2", "V3", "V4", "V5"), c("first", "second", "third", "fourth", "count"))
  fourGrams_DF$first<-as.character(fourGrams_DF$first)
  fourGrams_DF$second<-as.character(fourGrams_DF$second)
  fourGrams_DF$third<-as.character(fourGrams_DF$third)
  fourGrams_DF$fourth<-as.character(fourGrams_DF$fourth)
  #fourGrams_DF$count<-as.numeric(fourGrams_DF$count)
  fourGrams_DF<- subset(fourGrams_DF, select=c(first,second,third,fourth,count)) 
  
  print(input$qwords)
  quadWords <- strsplit(input$qwords," ")
  quadWords <- unlist(quadWords)
  print(quadWords[1])
  print(quadWords[2])
  print(quadWords[3])
  qdf <- fourGrams_DF[which(fourGrams_DF$first==quadWords[1] & fourGrams_DF$second==quadWords[2] & fourGrams_DF$third==quadWords[3]),]
  qdf<-orderBy(~-count, data=qdf)
  qsum <- sum(qdf$count)
  qdf$frequency<-qdf$count / qsum
  qdf <- qdf[1:20,]
  return(list(qdf=qdf))
})

output$quadGrams <- renderTable({
  if (is.null(qData())) {return()}
  print(qData()$qdf)
}, 'include.rownames' = FALSE
, 'include.colnames' = TRUE
, 'sanitize.text.function' = function(x){x}
, digits = 4)

pData = reactive ({
  
   if(input$data_source=="blogs"){
    fiveGrams_DF <- read.table("b5.txt",sep=",",quote="")
  }else if(input$data_source=="news"){
   fiveGrams_DF <- read.table("n5.txt",sep=",",quote="")
  }else{
    fiveGrams_DF <- read.table("t5.txt",sep=",",quote="")
  }
  
  #fiveGrams_DF <- read.table("t5.txt",sep=",",quote="")
  fiveGrams_DF <- setnames(fiveGrams_DF, c("V1", "V2", "V3", "V4", "V5", "V6"), c("first", "second", "third", "fourth", "fifth", "count"))
  fiveGrams_DF$first<-as.character(fiveGrams_DF$first)
  fiveGrams_DF$second<-as.character(fiveGrams_DF$second)
  fiveGrams_DF$third<-as.character(fiveGrams_DF$third)
  fiveGrams_DF$fourth<-as.character(fiveGrams_DF$fourth)
  fiveGrams_DF$fifth<-as.character(fiveGrams_DF$fifth)
  #fiveGrams_DF$count<-as.numeric(fiveGrams_DF$count)
  fiveGrams_DF<- subset(fiveGrams_DF, select=c(first,second,third,fourth,fifth,count)) 
  
  print(input$pwords)
  pentaWords <- strsplit(input$pwords," ")
  pentaWords <- unlist(pentaWords)
  print(pentaWords[1])
  print(pentaWords[2])
  print(pentaWords[3])
  print(pentaWords[4])
  pdf <- fiveGrams_DF[which(fiveGrams_DF$first==pentaWords[1] & fiveGrams_DF$second==pentaWords[2] & fiveGrams_DF$third==pentaWords[3] & fiveGrams_DF$fourth==pentaWords[4]),]
  pdf<-orderBy(~-count, data=pdf)
  psum <- sum(pdf$count)
  pdf$frequency<-pdf$count / psum
  pdf <- pdf[1:20,]
  return(list(pdf=pdf))
})

output$pentaGrams <- renderTable({
  if (is.null(pData())) {return()}
  print(pData()$pdf)
}, 'include.rownames' = FALSE
, 'include.colnames' = TRUE
, 'sanitize.text.function' = function(x){x}
, digits = 4)

  
})

  


