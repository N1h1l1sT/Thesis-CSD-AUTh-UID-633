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


#################
## Iteration 1 ##
#################
#Eliminating invalid entries
ClusteringSQLQuery <- "SELECT * FROM [YLIKA_KOSTOL].[dbo].[v4Erga] WHERE (GeoLocX >= 18 AND GeolocX <= 29 AND GeoLocY >= 34 AND GeoLocY <= 42) AND (GeoLocX <> -1 and GeoLocY <> -1)"
Clustering_DS <- rxImport(inData = RxOdbcData(sqlQuery = ClusteringSQLQuery, connectionString = sqlConnString, rowsPerRead = RowsPerRead),
                         outFile = paste(strXDF, "Clustering_DS.xdf", sep = ""),
                         colClasses = vErgaColClasses,
                         colInfo = vErgaColInfo,
                         stringsAsFactors = TRUE,
                         varsToDrop = c("Onoma_Polis"),
                         overwrite = TRUE
)
remove(ClusteringSQLQuery)
file.remove(paste(strXDF, "vErga_DS.xdf", sep = ""))
remove(vErga_DS)
# This creates it in-memory and is not used
# Clustering_DS <- rxImport(inData = paste(strXDF, "Clustering_DS.xdf", sep = ""),
#                      colClasses = vErgaColClasses,
#                      colInfo = vErgaColInfo)
# rxSummary(~., data = Clustering_DS)$sDataFrame
remove(vErgaColClasses)
remove(vErgaColInfo)

#Visualising the invalid-entries-free Locations of the Clustering Dataset
rxLinePlot(GeoLocY ~ GeoLocX, Clustering_DS, type = "p")

############################################
## Finding the optimal number of clusters ##
############################################
unsupervisedLocationData1 <- rxDataStep(inData = Clustering_DS,
                                        varsToKeep = c("GeoLocX", "GeoLocY")
)
WithinGroupsSquaredError <- (nrow(unsupervisedLocationData1) - 1) * sum(apply(unsupervisedLocationData1, 2, var))
for (i in 2:15) {
  WithinGroupsSquaredError[i] <- sum(rxKmeans(formula = formula(~ GeoLocX + GeoLocY),
                                              data = unsupervisedLocationData1, 
                                              numClusters = i, 
                                              algorithm = "lloyd"
                                              )$withinss
                                 )
}

remove(unsupervisedLocationData1)
remove(i)

plot(1:15,
     WithinGroupsSquaredError,
     type = "b",
     xlab = "# of Clusters",
     ylab = "Within Groups Sum of Squares"
)
remove(WithinGroupsSquaredError)

#############
## K-Means ##
#############
KMeansModel <- rxKmeans(formula = ~ GeoLocX + GeoLocY, 
                        data = paste(strXDF, "Clustering_DS.xdf", sep = ""), 
                        numClusters = 5,
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

