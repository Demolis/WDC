library(gdata)
data_PPD = read.xls (xls = "Data.xlsx", sheet = 2, blank.lines.skip = TRUE, check.names = FALSE)
data_PPD_1995_2015 <- data_PPD[,-1]
rownames(data_PPD_1995_2015) <- data_PPD[,1]
data_PPD_1995_2015 <- data_PPD_1995_2015[,36:56]
data_PPD_1995_2015
colMeans(is.na(data_PPD_1995_2015)) #percentage of NAs in a data.frame
colMeans(!is.na(data_PPD_1995_2015))
data_row_PPD = dcast(melt(data_PPD_1995_2015), variable ~ rownames(data_PPD_1995_2015))
ibrary(plyr)
data_row_PPD <- rename(data_row_PPD, c("variable"="Year"))
data_row_PPD
colMeans(is.na(data_row_PPD))
colMeans(!is.na(data_row_PPD))