library(gdata)
library(reshape2)
library(plyr)

#n - номер листа в таблице .xlsx
#p - это процент данных, который должен быть по одному показателю.
#xls - название файла, в котором хранятся данные

data_set<- function(n, p, xls){
       data_ind_code = read.xls (xls, sheet = n, blank.lines.skip = TRUE, check.names = FALSE)
       data_ind_code_years <- data_ind_code[,-1]
       #data for function from 1995 till last year
       rownames(data_ind_code_years) <- data_ind_code[,1]
       data_ind_code_years <- data_ind_code_years[,36:53]
       data_row_ind_code = dcast(melt(data_ind_code_years), variable ~ rownames(data_ind_code_years))
       data_row_ind_code <- rename(data_row_ind_code, c("variable"="Year"))
       del_data_na <- data_row_ind_code[, which(colMeans(!is.na(data_row_ind_code)) >= p)]
       #del_data_na - результат (набор данных)
       print(del_data_na)
}

data_set(2, 0.5, "Data.xlsx")
