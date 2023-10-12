install.packages('UsingR')
library('UsingR')
install.packages('RISmed')
library('RISmed')

search_query <- EUtilsSummary("e-prescription AND 2013[pdat]", retmax = 50)

#For ID:
QueryId(search_query)

#For title:
head(ArticleTitle(EUtilsGet(search_query)),20)

#For abstract:
tail(AbstractText(EUtilsGet(search_query)),4)

#Section B

#1: Read icd10.csv
icd10 <- read.csv2("/cloud/project/icd10.csv")
View(icd10)

#2: Print three codes:
subset_data1 <- icd10[grep("^S37", icd10$code), ]
A <- head(subset_data1$name, 1)
print(head(subset_data1$name, 1))

subset_data2 <- icd10[grep("^D37", icd10$code), ]
print(head(subset_data2$name, 1))
B <- head(subset_data2$name, 1)

subset_data3 <- icd10[grep("^A30", icd10$code), ]
print(head(subset_data3$name, 1))
C <- head(subset_data3$name, 1)

#3: # Retrieve the count of articles related to the search query from 2019 to 2023
search_A <- EUtilsSummary("cat(A) AND 2019:2023[pdat]")
resultsA <- EUtilsGet(search_A)
resultsA

search_B <- EUtilsSummary("cat(B) AND 2019:2023[pdat]")
resultsB <- EUtilsGet(search_A)
resultsB

search_C <- EUtilsSummary("cat(C) AND 2019:2023[pdat]")
resultsC <- EUtilsGet(search_C)
resultsC

results <- (resultsA + resultsB + resultsC)
results

#4: Barplot that displays with different colour the number of articles per year for all 3 diseases combined for each of the last 3 years

query_2021 <- EUtilsSummary(paste0("(\"", A, "\" OR \"", B, "\" OR \"", C, "\")[Title/Abstract] AND 2021[pdat]"))
query_2022 <- EUtilsSummary(paste0("(\"", A, "\" OR \"", B, "\" OR \"", C, "\")[Title/Abstract] AND 2022[pdat]"))
query_2023 <- EUtilsSummary(paste0("(\"", A, "\" OR \"", B, "\" OR \"", C, "\")[Title/Abstract] AND 2023[pdat]"))

x <- c(QueryCount(query_2021), QueryCount(query_2022),QueryCount(query_2023))
barplot(x,names.arg=c("2021","2022","2023"), col = c("green", "brown", "yellow"))

#5:Treemap for the number of articles for the last 2 years and for the 3 diseases. 
#The treemap will consist of 3 groups (1 for each disease) and each group will consist of 2 subgroups (1 for each year).

install.packages("treemap")
library(treemap)

deseaseA_2022 <- EUtilsSummary(paste0("\"", A, "\"[Title/Abstract] AND 2022[pdat])"))
deseaseA_2023 <- EUtilsSummary(paste0("\"", A, "\"[Title/Abstract] AND 2023[pdat])"))

deseaseB_2022 <- EUtilsSummary(paste0("\"", B, "\"[Title/Abstract] AND 2022[pdat])"))
deseaseB_2023 <- EUtilsSummary(paste0("\"", B, "\"[Title/Abstract] AND 2023[pdat])"))

deseaseC_2022 <- EUtilsSummary(paste0("\"", C, "\"[Title/Abstract] AND 2022[pdat])"))
deseaseC_2023 <- EUtilsSummary(paste0("\"", C, "\"[Title/Abstract] AND 2023[pdat])"))


df <- data.frame(
  Disease = rep(c(A,B,C), each = 2),
  Year = rep(2022:2023, times = 3),
  Articles = c(QueryCount(deseaseA_2022), QueryCount(deseaseA_2023), QueryCount(deseaseB_2022), QueryCount(deseaseB_2023), QueryCount(deseaseC_2022), QueryCount(deseaseC_2023))
)

treemap(df, index = c("Disease", "Year"), vSize = "Articles", type = "value", palette = "Greens", title = "Treemap for Question 5")

