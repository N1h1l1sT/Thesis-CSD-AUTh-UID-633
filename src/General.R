########################
#### Initialisation ####
########################

# Installing required Libraries
install.packages(c("rpart.plot", "rattle")) #Only needed the first time the program runs!

#Loading required Libraries
library(rpart) #No installation needed, is already installed
library(rpart.plot)
library(rattle)
library(RevoTreeView) #No installation needed, is already installed

#Determines how many blocks/chunks will be. For a dataset with 25,000 rows, there will only be 1 chunk, whilst for one with 133,098 like ours, where will be Ceil(133,098/25,000)=6 chunks
RowsPerRead = 25000
strXDF = "H:/[XDF]/"
#strDesktop = file.path(Sys.getenv("USERPROFILE"), "Desktop", fsep = "\\")
sqlConnString <- "driver={SQL Server};server=GIANNISM-PC;database=YLIKA_KOSTOL;trusted_connection=true"

# dim(Classification_DS) - its dimensions
#Check out the rxImport function for an efficient and flexible way to bring data stored in a variety of data formats (e.g., text, SQL Server, ODBC, SAS, SPSS, Teradata) into a data frame in memory or an .xdf file.


#############################
#### SQL Table: Mel_Ylika ###
#############################
Mel_Ylika_DS <- rxImport(inData = RxOdbcData(sqlQuery = "SELECT * FROM v1Mel_Ylika", connectionString = sqlConnString, rowsPerRead = RowsPerRead),
                         outFile = paste(strXDF, "Mel_Ylika_DS.xdf", sep = ""),
                         stringsAsFactors = TRUE,
                         overwrite = TRUE
)
# Mel_Ylika_DS <- RxXdfData(file = paste(strXDF, "Mel_Ylika_DS.xdf", sep = ""))

rxGetInfo(paste(strXDF, "Mel_Ylika_DS.xdf", sep = ""), getVarInfo = TRUE, numRows = 5)
rxSummary(~., data = paste(strXDF, "Mel_Ylika_DS.xdf", sep = ""))$sDataFrame

file.remove(paste(strXDF, "Mel_Ylika_DS.xdf", sep = ""))
remove(Mel_Ylika_DS)


###########################################
#### SQL Table: Mel_Kostol_Analys_Ylika ###
###########################################
Mel_Kostol_Analys_Ylika_DS <- rxImport(inData = RxOdbcData(sqlQuery = "SELECT * FROM v2Mel_Kostol_Analys_Ylika", connectionString = sqlConnString, rowsPerRead = RowsPerRead),
                                       outFile = paste(strXDF, "Mel_Kostol_Analys_Ylika_DS.xdf", sep = ""),
                                       stringsAsFactors = TRUE,
                                       overwrite = TRUE
)
# Mel_Kostol_Analys_Ylika_DS <- RxXdfData(file = paste(strXDF, "Mel_Kostol_Analys_Ylika_DS.xdf", sep = ""))

rxGetInfo(paste(strXDF, "Mel_Kostol_Analys_Ylika_DS.xdf", sep = ""), getVarInfo = TRUE, numRows = 5)
rxSummary(~., data = paste(strXDF, "Mel_Kostol_Analys_Ylika_DS.xdf", sep = ""))$sDataFrame

file.remove(paste(strXDF, "Mel_Kostol_Analys_Ylika_DS.xdf", sep = ""))
remove(Mel_Kostol_Analys_Ylika_DS)


#########################################
#### SQL Table: vMel_Kostol_Variantes ###
#########################################
Mel_Kostol_Variantes_DS <- rxImport(inData = RxOdbcData(sqlQuery = "SELECT * FROM v3Mel_Kostol_Variantes", connectionString = sqlConnString, rowsPerRead = RowsPerRead),
                                    outFile = paste(strXDF, "Mel_Kostol_Variantes_DS.xdf", sep = ""),
                                    stringsAsFactors = TRUE,
                                    rowsPerRead = RowsPerRead,
                                    overwrite = TRUE
)
# Mel_Kostol_Variantes_DS <- RxXdfData(file = paste(strXDF, "Mel_Kostol_Variantes_DS.xdf", sep = ""))

rxGetInfo(paste(strXDF, "Mel_Kostol_Variantes_DS", sep = ""), getVarInfo = TRUE, numRows = 5)
rxSummary(~., data = paste(strXDF, "Mel_Kostol_Variantes_DS", sep = ""))$sDataFrame

file.remove(paste(strXDF, "Mel_Kostol_Variantes_DS.xdf", sep = ""))
remove(Mel_Kostol_Variantes_DS)


#########################
#### SQL Table: vErga ###
#########################
Erga_DS <- rxImport(inData = RxOdbcData(sqlQuery = "SELECT * FROM v4Erga", connectionString = sqlConnString, rowsPerRead = RowsPerRead),
                    outFile = paste(strXDF, "Erga_DS.xdf", sep = ""),
                    stringsAsFactors = TRUE,
                    rowsPerRead = RowsPerRead,
                    overwrite = TRUE
)
# Erga_DS <- RxXdfData(file = paste(strXDF, "Erga_DS.xdf", sep = ""))

rxGetInfo(paste(strXDF, "Erga_DS.xdf", sep = ""), getVarInfo = TRUE, numRows = 5)
rxSummary(~., data = paste(strXDF, "Erga_DS.xdf", sep = ""))$sDataFrame

file.remove(paste(strXDF, "Erga_DS.xdf", sep = ""))
remove(Erga_DS)


#######################
#### SQL Table: AIO ###
#######################
AIO_DS <- rxImport(inData = RxOdbcData(sqlQuery = "SELECT * FROM vAIO", connectionString = sqlConnString, rowsPerRead = RowsPerRead),
                   outFile = paste(strXDF, "AIO_DS.xdf", sep = ""),
                   stringsAsFactors = TRUE,
                   rowsPerRead = RowsPerRead,
                   overwrite = TRUE
)
# AIO_DS <- RxXdfData(file = paste(strXDF, "AIO_DS.xdf", sep = ""))

rxGetInfo(paste(strXDF, "AIO_DS.xdf", sep = ""), getVarInfo = TRUE, numRows = 5)
rxSummary(~., data = paste(strXDF, "AIO_DS.xdf", sep = ""))$sDataFrame

file.remove(paste(strXDF, "AIO_DS.xdf", sep = ""))
remove(AIO_DS)
