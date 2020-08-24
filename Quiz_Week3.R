# Week 3 Getting and Cleaning Data Quiz
# Q1
# Solutions

getwd()
setwd("./data")

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl, destfile = "quiz3data.csv")
mydata <- read.csv("quiz3data.csv")

logic <- mydata$ACR == 3 & mydata$AGS == 6
head(which(logic), n=3)

#Q2

install.packages("jpeg")
library(jpeg)

fileUrl2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
download.file(fileUrl2, destfile = "quiz3data2.jpg", mode = "wb")
jpgdata <- readJPEG("quiz3data2.jpg", native = TRUE)
quantile(jpgdata, probs = c(0.3,0.8))

#Q3

install.packages("data.table")
library(data.table)
install.packages("dplyr")
library(dplyr)
install.packages("dtplyr")
library(dtplyr)

fileUrl3 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
fileUrl4 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"

gdp <- data.table::fread(fileUrl3, skip = 5, nrows = 190, select = c(1,2,4,5),
                         col.names = c("CountryCode", "Rank", 
                                       "Economy", "Total"))

edu <- data.table::fread(fileUrl4)

merged <- merge(gdp, edu, by = "CountryCode")

dim(merged)

Q3_merged <- merged %>% arrange(desc(Rank))
Q3_merged
nrow(Q3_merged)
Q3_merged$Economy[13]
paste(nrow(Q3_merged), "matches, 13th country is", Q3_merged$Economy[13])

# Q4

Q3_merged %>% group_by(`Income Group`) %>%
  filter("High income: OECD" %in% `Income Group` | "High income: nonOECD" %in% `Income Group`) %>%
  summarize(Average = mean(Rank, na.rm = T)) %>%
  arrange(desc(`Income Group`))%>%
  print 

# Q5

Q3_merged$RankGroups <- cut(Q3_merged$Rank, breaks = 5)
vs <- table(Q3_merged$RankGroups, Q3_merged$`Income Group`)
vs

vs[1, "Lower middle income"]





