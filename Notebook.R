library(gdata)
data_PPD = read.xls (xls = "Data.xlsx", sheet = 2, blank.lines.skip = TRUE, check.names = FALSE)
data_PPD_1995_2015 = data_PPD[,37:57]
data_PPD_1995_2015
colMeans(is.na(data_PPD_1995_2015)) #percentage of NAs in a data.frame
colMeans(!is.na(data_PPD_1995_2015))