if (("RevoScaleR" %in% loadedNamespaces())) {
#########################
#### SQL Table: vErga ###
#########################
vErgaColClasses <- c(Label = "integer",
                     ID_Erga = "factor",
                     GrafioEktelesisErgou = "factor",
                     Onoma_Polis = "factor",
                     GeoLocX = "numeric",
                     GeoLocY = "numeric",
                     Katigoria = "factor",
                     Mel_Kathisterisi_Pelati = "integer",
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
                     TimeSeriesDate = "character",
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

vErgaColInfo <- list(MelClientDelay = list(type = "factor", levels = c("0","1"), newLevels = c("Not_Delayed", "Delayed")),
                     MelDEHDelay = list(type = "factor", levels = c("0","1"), newLevels = c("Not_Delayed", "Delayed")),
                     MelOthersDelay = list(type = "factor", levels = c("0","1"), newLevels = c("Not_Delayed", "Delayed")),
                     Anagi_YS = list(type = "factor", levels = c("0","1"), newLevels = c("Not_Needed", "Needed")),
                     Ektasi_Ergou = list(type = "factor", levels = c("1", "2"), newLevels = c("Small", "Large"))
)

NewvErgaVarInfo <- list(
  Label = list(description = "The Label for the Supervised learning (Dependent Variable); Whether a project was approved or rejected."),
  ID_Erga = list(description = "Primary identification of each project on the SQL Server."),
  GrafioEktelesisErgou = list(description = "General understanding of the geographical area the project took place on."),
  Katigoria = list(description = "Generalised project categories."),
  Meres_Meletis = list(description = "Number of days the ‘study’ part of the project lasted (HEDNO-delay inclusive)."),
  Sinergio_Meletis = list(description = "The name of the study workshop used."),
  Kostos_Ergatikon_Kataskevis = list(description = "The amount of money that construction workers cost the company."),
  Kostos_Ilikon_Kataskevis = list(description = "The cost of the materials to be used for the project."),
  Kostos_Kataskevis = list(description = "The amount of money the construction costs to the company (inclusive of [Kostos_Ylikon_Kataskevis] and [Kostos_Ergatikon_Kataskevis])."),
  Kostos_Ergolavikon_Epidosis = list(description = "The cost of service contractors."),
  Ektasi_Ergou = list(description = "The project’s scale (small or big)."),
  Anagi_YS = list(description = "Whether or not there’s a need for a substation."),
  SAP_Typos_Pelati = list(description = "5 codes for 5 delineating types of customers."),
  SAP_Eidos_Aitimatos = list(description = "41 codes for 41 delineating types of requests."),
  Onoma_Polis = list(description = "The city that a project takes place on."),
  Xaraktirismos_Ergou = list(description = "Separates projects into categories: investment, utilisation."),
  Skopos_Ergou = list(description = "Separates projects into the 4 Investment sub-categories and the 12 Utilisation sub-categories."),
  TimeSeriesDate = list(description = "Separates the projects into quarters of their respective years."),
  Mel_Kathisterisi_Pelati = list(description = "Number of days the study was held back due to the client."),
  MelClientDelay = list(description = "Binary variable for whether or not there was a delay in the ‘study’ due to the customer/client."),
  Mel_Kathisterisi_DEH = list(description = "Number of days the study was held back due to the organisation itself (HEDNO)."),
  MelDEHDelay = list(description = "Binary variable for whether or not there was a delay in the ‘study’ due to the organisation itself (HEDNO)."),
  Mel_Kathisterisi_Triton = list(description = "Number of days the study was held back due to other factors."),
  MelOthersDelay = list(description = "Binary variable for whether or not there was a delay in the ‘study’ due to other external factors."),
  GeoLocX = list(description = "The geographical Longitude of the corresponding field in [Onoma_Polis]."),
  GeoLocY = list(description = "The geographical Latitude of the corresponding field in [Onoma_Polis]."),
  Kathisterisi_AitisisKataxorisis = list(description = "The delay in days between the date of the application for the project and the day of its registration."),
  Kathisterisi_Meletis = list(description = "The delay in days between the date of the project’s registration and the date the its study part finished."),
  Kathisterisi_Anagelias = list(description = "The delay in days between the date the project’s study part finished and the date the letter informing the customer of its price is sent."),
  DayOfYearSine = list(description = "Gives very close values for days in a year that are close to each other."),
  DayOfYearCosine = list(description = "Gives very close values for days in a year that are close to each other."),
  DayOfYearCartesX = list(description = "Gives very close values for days in a year and years themselves that are close to each other."),
  DayOfYearCartesY = list(description = "Gives very close values for days in a year and years themselves that are close to each other.")
)


rxDataStep(inData = RxOdbcData(sqlQuery = "SELECT * FROM v4Erga", connectionString = sqlConnString, rowsPerRead = RowsPerRead),
           outFile = paste(strXDF, "tmp.xdf", sep = ""),
           colClasses = vErgaColClasses,
           colInfo = vErgaColInfo,
           stringsAsFactors = TRUE,
           rowsPerRead = RowsPerRead,
           overwrite = TRUE
)
rxSetVarInfo(varInfo = NewvErgaVarInfo,
             data = paste(strXDF, "tmp.xdf", sep = "")
)
rxFactors(inData = paste(strXDF, "tmp.xdf", sep = ""),
          outFile = paste(strXDF, "vErga_DS.xdf", sep = ""),
          factorInfo = c("TimeSeriesDate"),
          sortLevels = TRUE,
          overwrite = TRUE
)
vErga_DS <- RxXdfData(file = paste(strXDF, "vErga_DS.xdf", sep = ""))

file.remove(paste(strXDF, "tmp.xdf", sep = ""))

rxGetInfo(vErga_DS, getVarInfo = TRUE, numRows = 0)
rxSummary(~., data = vErga_DS)$sDataFrame


} else {
  "RevoScaleR not found. Please use Microsoft R Server or equivalent."
}
