# 2014年下半年-课程设计

rm(list = ls())
setwd("O:/数据分析R语言/作业/词云")
library(rJava)
library(Rwordseg) # 依赖于rJava
library(RColorBrewer)
library(wordcloud) # 依赖于RColorBrewer

# 清洗数据 --------------------------------------------------------------------

clearData <- function(data){
  tmpData <- array()
  finalData <- list()
  for(num in 1:5){
    count <- 1
    for(i in 1:length(as.character(data[[num]]))){
      if(nchar(as.character(data[[num]][i]))!=0){
        tmpData[count] <- as.character(data[[num]][i])
        count <- count + 1
      }  
    }
    finalData[[num]] <- tmpData
    tmpData <- array()
  }
  return(finalData)
}

# 分词 ----------------------------------------------------------------------

wordSeg <- function(data, choice){
  #插入词频较大的北京邮电大学/北京交通大学
  main <- c("BUPT", "BJTU")
  filePath <- c("BUPT.txt", "BJTU.txt")
  words <- unlist(lapply(X = data, FUN = segmentCN)) # 分词
  tmpData <- array()
  for(i in 1:length(words)){
    if(nchar(words[i]) != 1){
      tmpData[i] <- words[i]
      cat(words[i], file=filePath[choice], sep="\n", append = TRUE) # 把分词写入txt文件
    }
    else{
      tmpData[i] <- main[choice] # 把字符为1的分词全部置为北邮/北交
      cat(main[choice], file=filePath[choice], sep="\n", append = TRUE)
    }
  }
  tab <- table(tmpData) # 重建表
  word = sort(tab, decreasing = TRUE) # 降序排列
  return(word)
}

# 词云 ----------------------------------------------------------------------

wordCloud <- function(data, choice){
  main <- c("BUPTWOrdCloud.png", "BJTUWOrdCloud.png")
  png(main[choice], width=12, height=8, units="in", res=300) # 保存文件
  wordcloud(names(data),as.integer(data), random.order=FALSE, colors=brewer.pal(8, "Dark2"))
  dev.off()
}

# 北京邮电大学 ------------------------------------------------------------------

BUPT <- read.csv("北京邮电大学2013-2009年获奖科技成果.csv") 
BUPTData <- list()
BUPTData <- clearData(BUPT)
BUPTWord <- wordSeg(BUPTData, 1)
wordCloud(BUPTWord[1:100], 1)

# 北京交通大学 ------------------------------------------------------------------

BJTU <- read.csv("北京交通大学电气工程学院2012-2008年科研项目一览.csv") 
BJTUData <- list()
BJTUData <- clearData(BJTU)
BJTUWord <- wordSeg(BJTUData, 2)
wordCloud(BJTUWord[1:100], 2)

