if (("RevoScaleR" %in% loadedNamespaces())) {
  
#How construction cost differs between projects that were cancelled and those who weren't
rxSummary(formula = ~Kostos_Kataskevis:LabelFactorial, data = Classification_DS)


#Getting a better feel for the data by drawing their histograms
rxHistogram(~Label|TimeSeriesDate, data = Classification_DS)



} else {
  "RevoScaleR not found. Please use Microsoft R Server or equivalent."
}