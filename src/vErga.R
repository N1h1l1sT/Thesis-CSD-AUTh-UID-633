#########################
#### SQL Table: vErga ###
#########################
vErgaColClasses <- c(Mel_Kathisterisi_Pelati = "integer",
                     Mel_Kathisterisi_DEH = "integer",
                     Mel_Kathisterisi_Triton = "integer",
                     Meres_Meletis = "integer",
                     Kostos_Ergatikon_Kataskevis = "numeric",
                     Kostos_Ilikon_Kataskevis = "numeric",
                     Kostos_Kataskevis = "numeric",
                     Kostos_Ergolavikon_Epidosis = "numeric",
                     DayOfYearSine = "numeric",
                     DayOfYearCosine = "numeric",
                     DayOfYearCartesX = "numeric",
                     DayOfYearCartesY = "numeric",
                     Kathisterisi_AitisisKataxorisis = "integer",
                     Kathisterisi_Meletis = "integer",
                     Kathisterisi_Anagelias = "integer",
                     Label = "integer",
                     ID_Erga = "factor",
                     TimeSeriesDate = "factor",
                     GrafioEktelesisErgou = "factor",
                     Onoma_Polis = "factor",
                     GeoLocX = "numeric",
                     GeoLocY = "numeric",
                     Katigoria = "factor",
                     Xaraktirismos_Ergou = "factor",
                     Skopos_Ergou = "factor",
                     MelClientDelay = "factor",
                     MelDEHDelay = "factor",
                     MelOthersDelay = "factor",
                     Sinergio_Meletis = "factor",
                     Ektasi_Ergou = "factor",
                     Anagi_YS = "factor",
                     SAP_Typos_Pelati = "factor",
                     SAP_Eidos_Aitimatos = "factor"
                     )

vErgaColInfo <- list(#Xaraktirismos_Ergou = list(type = "factor", levels = c("EKMETALEFSI","EPENDISI"), newLevels = c("EKMETALEFSI", "EPENDISI")), #For some reason it returns 0 rows for this column
                     MelClientDelay = list(type = "factor", levels = c("0","1"), newLevels = c("Not_Delayed", "Delayed")),
                     MelDEHDelay = list(type = "factor", levels = c("0","1"), newLevels = c("Not_Delayed", "Delayed")),
                     MelOthersDelay = list(type = "factor", levels = c("0","1"), newLevels = c("Not_Delayed", "Delayed")),
                     Anagi_YS = list(type = "factor", levels = c("0","1"), newLevels = c("Not_Needed", "Needed"))#,
                     # TimeSeriesDate = list(type = "factor",
                     #                       levels = c("1930 Q1", "1947 Q4", "1998 Q2", "1998 Q3", "1998 Q4", "1999 Q2", "1999 Q4", "2000 Q1", "2000 Q4", "2001 Q1", "2001 Q2", "2001 Q3", "2001 Q4", "2002 Q1", "2002 Q2", "2002 Q3", "2002 Q4", "2003 Q1", "2003 Q2", "2003 Q3", "2003 Q4", "2004 Q1", "2004 Q2", "2004 Q3", "2004 Q4", "2005 Q1", "2005 Q2", "2005 Q3", "2005 Q4", "2006 Q1", "2006 Q2", "2006 Q3", "2006 Q4", "2007 Q1", "2007 Q2", "2007 Q3", "2007 Q4", "2008 Q1", "2008 Q2", "2008 Q3", "2008 Q4", "2009 Q1", "2009 Q2", "2009 Q3", "2009 Q4", "2010 Q1", "2010 Q2", "2010 Q3", "2010 Q4", "2011 Q1", "2011 Q2", "2011 Q3", "2011 Q4", "2012 Q1", "2012 Q2", "2012 Q3", "2012 Q4", "2013 Q1", "2013 Q2", "2013 Q3", "2013 Q4", "2014 Q1", "2014 Q2", "2014 Q3", "2014 Q4", "2015 Q1", "2015 Q2", "2015 Q3", "2015 Q4", "2016 Q1", "2016 Q2"),
                     #                       newLevels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "70")
                     # )
)

vErga_DS <- rxImport(inData = RxOdbcData(sqlQuery = "SELECT * FROM v4Erga", connectionString = sqlConnString, rowsPerRead = RowsPerRead),
                     outFile = paste(strXDF, "vErga_DS.xdf", sep = ""),
                     colClasses = vErgaColClasses,
                     colInfo = vErgaColInfo,
                     stringsAsFactors = TRUE,
                     rowsPerRead = RowsPerRead,
                     overwrite = TRUE
)
# vErga_DS <- rxImport(inData = paste(strXDF, "vErga_DS.xdf", sep = ""),
                     # colClasses = vErgaColClasses,
                     # colInfo = vErgaColInfo
# )
rxGetInfo(vErga_DS, getVarInfo = TRUE)
rxSummary(~., data = vErga_DS)$sDataFrame


