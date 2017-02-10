## Unsupervised Learning - Clustering ##

##################################
### Forming the Clustering Set ###
##################################


#################
## Iteration 0 ##
#################
#Peaking at the database as it is in the SQL View
rxLinePlot(formula = GeoLocY ~ GeoLocX,
           data = vErga_DS,
           type = "p"
)
#Many entries are outside Greece's rectangle

file.remove(paste(strXDF, "vErga_DS.xdf", sep = ""))
remove(vErga_DS)


#################
## Iteration 1 ##
#################
#Eliminating invalid entries
ClusteringSQLQuery <- "SELECT * FROM [YLIKA_KOSTOL].[dbo].[v4Erga] WHERE (GeoLocX >= 18 AND GeolocX <= 29 AND GeoLocY >= 34 AND GeoLocY <= 42) AND (GeoLocX <> -1 and GeoLocY <> -1)"
Clustering_DS <- rxImport(inData = RxOdbcData(sqlQuery = ClusteringSQLQuery, connectionString = sqlConnString, rowsPerRead = RowsPerRead),
                         outFile = paste(strXDF, "tmp.xdf", sep = ""),
                         colClasses = vErgaColClasses,
                         colInfo = vErgaColInfo,
                         stringsAsFactors = TRUE,
                         overwrite = TRUE
)
rxSetVarInfo(varInfo = NewvErgaVarInfo,
             data = paste(strXDF, "tmp.xdf", sep = "")
)
rxDataStep(inData = paste(strXDF, "tmp.xdf", sep = ""),
           outFile = paste(strXDF, "tmp2.xdf", sep = ""),
           varsToDrop = c("Onoma_Polis"),
           overwrite = TRUE
)
rxFactors(inData = paste(strXDF, "tmp2.xdf", sep = ""),
          outFile = paste(strXDF, "Clustering_DS.xdf", sep = ""),
          factorInfo = c("TimeSeriesDate"), 
          sortLevels = TRUE,
          overwrite = TRUE
)
Clustering_DS <- RxXdfData(paste(strXDF, "Clustering_DS.xdf", sep = ""))


file.remove(paste(strXDF, "tmp.xdf", sep = ""))
file.remove(paste(strXDF, "tmp2.xdf", sep = ""))
remove(NewvErgaVarInfo)
remove(vErgaColClasses)
remove(vErgaColInfo)
remove(ClusteringSQLQuery)

rxGetInfo(Clustering_DS, getVarInfo = TRUE, numRows = 1)
rxSummary(~., data = Clustering_DS)$sDataFrame

#Visualising the invalid-entries-free Locations of the Clustering Dataset
rxLinePlot(GeoLocY ~ GeoLocX, Clustering_DS, type = "p")

############################################
## Finding the optimal number of clusters ##
############################################
unsupervisedLocationData1 <- rxDataStep(inData = Clustering_DS,
                                        varsToKeep = c("GeoLocX", "GeoLocY")
)
WithinGroupsSquaredError <- (nrow(unsupervisedLocationData1) - 1) * sum(apply(unsupervisedLocationData1, 2, var))
for (i in 2:30) {
  WithinGroupsSquaredError[i] <- sum(rxKmeans(formula = formula(~ GeoLocX + GeoLocY),
                                              data = unsupervisedLocationData1, 
                                              numClusters = i, 
                                              algorithm = "lloyd"
                                              )$withinss
                                 )
}

remove(unsupervisedLocationData1)

plot(1:30,
     WithinGroupsSquaredError,
     type = "b",
     xlab = "# of Clusters",
     ylab = "Within Groups Sum of Squares"
)

k <- 1
for (i in 2:30) {
  if (((WithinGroupsSquaredError[i-1] - WithinGroupsSquaredError[i]) / WithinGroupsSquaredError[i]) <= 0.1) {
    break
  }
  else {
    k <- i
  }
}
k

remove(i)
remove(WithinGroupsSquaredError)

#############
## K-Means ##
#############
KMeansModel <- rxKmeans(formula = ~ GeoLocX + GeoLocY, 
                        data = paste(strXDF, "Clustering_DS.xdf", sep = ""), 
                        numClusters = k,
                        outFile = paste(strXDF, "Clustering_DS.xdf", sep = ""),
                        algorithm = "lloyd",
                        blocksPerRead = 1,
                        overwrite = TRUE
)
#K-Means model information
KMeansModel
remove(KMeansModel)

rxLinePlot(GeoLocY ~ GeoLocX,
           groups = .rxCluster,
           data = paste(strXDF, "Clustering_DS.xdf", sep = ""),
           type = "p"
)

