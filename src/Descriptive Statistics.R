
#How construction cost differs between projects that were cancelled and those who weren't
rxSummary(formula = ~Kostos_Kataskevis:Label, data = Classification_DS)


#Getting a better feel for the data by drawing their histograms
rxHistogram(~TimeSeriesDate, data = Classification_DS)

