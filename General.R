########################
#### Initialisation ####
########################

# Only needed the first time!
# install.packages("RODBC")
library(RODBC)

true = TRUE
True = TRUE
false = FALSE
False = FALSE
sqlRowsPerRead = 5000
strDesktop = file.path(Sys.getenv("USERPROFILE"),"Desktop",fsep="\\")
strXDF = "H:/[XDF]/"
sqlConnString <- "driver={SQL Server};server=GIANNISM-PC;database=YLIKA_KOSTOL;trusted_connection=true"

dbhandle <- odbcDriverConnect(sqlConnString)
#Mel_Ylika_Table <- sqlQuery(dbhandle, 'select * from vMel_Ylika')

#############################
#### SQL Table: Mel_Ylika ###
#############################

# Mel_Ylika_DS <- rxImport(inData = sqlFetch(dbhandle, 'vMel_Ylika', colnames=FALSE, rows_at_time=1000),
#                          outFile = paste(strXDF, "Mel_Ylika_DS.xdf", sep = ""),
#                          stringsAsFactors = TRUE,
#                          overwrite = TRUE)
Mel_Ylika_DS <- rxImport(inData = paste(strXDF, "Mel_Ylika_DS.xdf", sep = ""))

#PromitheftisAbbr <- c("DEH", "NonDEH")
  ccColInfo <- list(
    Promitheftis = list(type = "factor",
                        levels = c("0", "-1"),
                        newLevels = c("DEH", "NonDEH")),
    Epimetrisimo = list(type = "factor",
                        levels = c("0", "-1"),
                        newLevels = c("False", "True"))
  )

sql_Mel_Ylika_DS <- RxSqlServerData(connectionString = sqlConnString,
                                    table = "[Mel_Ylika]", colInfo = ccColInfo,
                                    rowsPerRead = sqlRowsPerRead)
rxGetVarInfo(sql_Mel_Ylika_DS)
CurSummarySQL <- rxSummary(~., data = sql_Mel_Ylika_DS)
CurSummarySQL$sDataFrame


rxGetVarInfo(Mel_Ylika_DS)
CurSummary <- rxSummary(~., data = Mel_Ylika_DS)
CurSummary$sDataFrame
#CurSummary$categorical #Too big to be printed in the console (results in loss of information at the top)


###########################################
#### SQL Table: Mel_Kostol_Analys_Ylika ###
###########################################

# Mel_Kostol_Analys_Ylika_DS <- rxImport(inData = sqlFetch(dbhandle, 'vMel_Kostol_Analys_Ylika', colnames=FALSE, rows_at_time=1000),
#                         outFile = paste(strXDF, "Mel_Kostol_Analys_Ylika_DS.xdf", sep = ""),
#                         stringsAsFactors = TRUE,
#                         overwrite = TRUE)
Mel_Kostol_Analys_Ylika_DS <- rxImport(inData = paste(strXDF, "Mel_Kostol_Analys_Ylika_DS.xdf", sep = ""))

rxGetVarInfo(Mel_Kostol_Analys_Ylika_DS)
CurSummary <- rxSummary(~., data = Mel_Kostol_Analys_Ylika_DS)
CurSummary$sDataFrame


#########################################
#### SQL Table: vMel_Kostol_Variantes ###
#########################################

# Mel_Kostol_Variantes_DS <- rxImport(inData = sqlFetch(dbhandle, 'vMel_Kostol_Variantes', colnames=FALSE, rows_at_time=1000),
#                         outFile = paste(strXDF, "Mel_Kostol_Variantes_DS.xdf", sep = ""),
#                         stringsAsFactors = TRUE,
#                         overwrite = TRUE)
Mel_Kostol_Variantes_DS <- rxImport(inData = paste(strXDF, "Mel_Kostol_Variantes_DS.xdf", sep = ""))

rxGetVarInfo(Mel_Kostol_Variantes_DS)
CurSummary <- rxSummary(~., data = Mel_Kostol_Variantes_DS)
CurSummary$sDataFrame


#########################
#### SQL Table: vErga ###
#########################

Erga_DS <- rxImport(inData = sqlFetch(dbhandle, 'vErga', colnames=FALSE, rows_at_time=1000),
                        outFile = paste(strXDF, "Erga_DS.xdf", sep = ""),
                        stringsAsFactors = TRUE,
                        overwrite = TRUE)
Erga_DS <- rxImport(inData = paste(strXDF, "Erga_DS.xdf", sep = ""))

rxGetVarInfo(Erga_DS)
CurSummary <- rxSummary(~., data = Erga_DS)
CurSummary$sDataFrame
























#######################
#### SQL Table: AIO ###
#######################
AIO_DS <- rxImport(inData = sqlFetch(dbhandle, 'vAIO', colnames=FALSE, rows_at_time=1000),
                        outFile = paste(strXDF, "AIO_DS.xdf", sep = ""),
                        stringsAsFactors = TRUE,
                        overwrite = TRUE)
AIO_DS <- rxImport(inData = paste(strXDF, "AIO_DS.xdf", sep = ""))

rxGetVarInfo(AIO_DS)
CurSummary <- rxSummary(~., data = AIO_DS)
CurSummary$sDataFrame



