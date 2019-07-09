library(readxl)
library(summarytools)


housing <- read_excel("rdata/Housing.xlsx")
view(dfsummary(housing))
