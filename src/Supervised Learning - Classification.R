if (("RevoScaleR" %in% loadedNamespaces())) {
###########################
### Supervised Learning ###
###########################

################################################
### 0) Forming the Training and Testing Sets ###
################################################
#Creating a selection ratio, dropping no longer needed variables and finalising the classification dataset
rxDataStep(inData = paste(strXDF, "Clustering_DS.xdf", sep = ""),
           outFile = paste(strXDF, "preClassification1_DS.xdf", sep = ""),
           transforms = list(SelectionRatio = as.integer(runif(.rxNumRows,1,11)),
                             LabelFactorial = factor(Label, c(0,1))),
           overwrite=TRUE
)
file.remove(paste(strXDF, "Clustering_DS.xdf", sep = ""))
remove(Clustering_DS)
rxDataStep(inData = paste(strXDF, "preClassification1_DS.xdf", sep = ""),
           outFile = paste(strXDF, "preClassification2_DS.xdf", sep = ""),
           varsToDrop = c("GeoLocX", "GeoLocY"),
           overwrite = TRUE
)
file.remove(paste(strXDF, "preClassification1_DS.xdf", sep = ""))
preClassification2_DS <- RxXdfData(file = paste(strXDF, "preClassification2_DS.xdf", sep = ""))
#Turns out, that if one types the path to the xfd in the inData var, the transformation will not work, but the rest seems to work fine.
#One needs to import the xdf through RxXdfData to a var and give that to the inData for the transformation to work.
#Keep it in mind because many an hour can go wasted otherwise. Talking from experience.
ClassificationColInfo <- list("LabelFactorial" = list(type = "factor", levels = c("0", "1"), newLevels = c("Cancelled", "Approved")))
Classification_DS <- rxImport(inData = preClassification2_DS,
                              outFile = paste(strXDF, "Classification_DS.xdf", sep = ""),
                              colInfo = ClassificationColInfo,
                              overwrite = TRUE
)
Classification_DS <- RxXdfData(paste(strXDF, "Classification_DS", sep = ""))
n_Classification <- rxGetInfo(data = Classification_DS)$numRows

remove(ClassificationColInfo)
file.remove(paste(strXDF, "preClassification2_DS.xdf", sep = ""))
remove(preClassification2_DS)
#Visualising the Class Imbalance
rxHistogram(~LabelFactorial, data = Classification_DS)

rxGetInfo(Classification_DS, getVarInfo = TRUE, numRows = 0)
rxSummary(~., data = Classification_DS)$sDataFrame

##########################
#Forming the Training set#
##########################
rxDataStep(inData = paste(strXDF, "Classification_DS.xdf", sep = ""),
           outFile = paste(strXDF, "Training_DS.xdf", sep = ""),
           rowSelection = SelectionRatio <= 8, #About 80% of the data
           varsToDrop = "SelectionRatio",
           blocksPerRead = 20,
           rowsPerRead = RowsPerRead,
           overwrite = TRUE
)
Training_DS <- RxXdfData(file = paste(strXDF, "Training_DS.xdf", sep = ""))
n_Train <- rxGetInfo(data = Training_DS)$numRows
ActualTrainingPercentage <- n_Train / n_Classification * 100
ActualTrainingPercentage

rxGetInfo(Training_DS, getVarInfo = TRUE, numRows = 0)
rxSummary(~., data = Training_DS)$sDataFrame

######################
#forming the test set#
######################
rxDataStep(inData = paste(strXDF, "Classification_DS.xdf", sep = ""),
           outFile = paste(strXDF, "Test_DS.xdf", sep = ""),
           rowSelection = SelectionRatio > 8, #About 20% of the data
           varsToDrop = "SelectionRatio",
           blocksPerRead = 20,
           rowsPerRead = RowsPerRead,
           overwrite = TRUE
)
Test_DS <- RxXdfData(file = paste(strXDF, "Test_DS.xdf", sep = ""))
n_Test <- rxGetInfo(data = Test_DS)$numRows
ActualTestPercentage <- n_Test / n_Classification * 100
ActualTestPercentage
remove(ActualTestPercentage)
remove(ActualTrainingPercentage)

rxGetInfo(Test_DS, getVarInfo = TRUE, numRows = 0)
rxSummary(~., data = Test_DS)$sDataFrame

####################################################################
### 1) Creating a Classification Model using Logistic Regression ###
####################################################################

## Creating the Model ##

#SAP_Eidos_Aitimatos creates loads of error messages, ergo it is removed
#rxLogit will only classify cases where no variables are missing, and since SAP_Typos_Pelati is mostly missing, it's deemed imperative that it be excluded
system.time(
  #Tweakable:
  #--Dependent Variables
  LogisticRegressionModel <- rxLogit(Label ~ TimeSeriesDate + GrafioEktelesisErgou + Katigoria + Xaraktirismos_Ergou + .rxCluster
                                     + Skopos_Ergou + MelClientDelay + MelDEHDelay + MelOthersDelay + Sinergio_Meletis
                                     + Ektasi_Ergou + Anagi_YS #+ SAP_Typos_Pelati #+ SAP_Eidos_Aitimatos
                                     + Mel_Kathisterisi_Pelati + Mel_Kathisterisi_DEH + Mel_Kathisterisi_Triton + Meres_Meletis
                                     + Kostos_Ergatikon_Kataskevis + Kostos_Ilikon_Kataskevis + Kostos_Kataskevis + Kostos_Ergolavikon_Epidosis
                                     + Kathisterisi_AitisisKataxorisis + Kathisterisi_Meletis + Kathisterisi_Anagelias + DayOfYearSine
                                     + DayOfYearCosine + DayOfYearCartesX + DayOfYearCartesY
                                     , data = paste(strXDF, "Training_DS.xdf", sep = "")
                                     , covCoef = TRUE
                                     # ,maxIterations =
                                     # ,fweights =
                                     # ,pweights =

                                     ,reportProgress = rxGetOption("reportProgress")
                                     ,blocksPerRead = rxGetOption("blocksPerRead")
                                     # ,rowSelection =
                                     # ,variableSelection =
  )
)
# summary(LogisticRegressionModel)
#LogisticRegressionModel$coefficients     #[vector] Model's Coefficients
#LogisticRegressionModel$covCoef          #[vector] variance-covariance matrix for the regression coefficient estimates
#LogisticRegressionModel$condition.number #[value ] estimated reciprocal condition number of final weighted cross-product (X'WX) matrix
#LogisticRegressionModel$aliased          #[vector] TRUE/FALSE of whether columns were dropped or not due to collinearity
#LogisticRegressionModel$coef.std.error   #[vector] standard errors of the coefficients
#LogisticRegressionModel$coef.t.value     #[vector] coefficients divided by their standard errors
#LogisticRegressionModel$coef.p.value     #[vector] p-values for coef.t.values, using the normal distribution (Pr(>|z|))
#LogisticRegressionModel$total.squares    #[value ] Y'Y of raw Y's
#LogisticRegressionModel$df               #[value3] degrees of freedom, a 3-vector (p, n-p, p*), the last being the number of non-aliased coefficients
#LogisticRegressionModel$deviance         #[value ] deviance minus twice the maximized log-likelihood (up to a constant)

# rxGetInfo(paste(strXDF, "Training_DS.xdf", sep = ""), getVarInfo=TRUE, numRows = 3)
# rxSummary(~., data = paste(strXDF, "Training_DS.xdf", sep = ""))$sDataFrame

## Applying the Predictions ##
rxPredict(modelObject = LogisticRegressionModel,
          data = paste(strXDF, "Test_DS.xdf", sep = ""),
          outData = paste(strXDF, "Test_DS.xdf", sep = ""),
          overwrite = TRUE
          ,predVarNames = "LogRe_PredictionReal"
          # ,computeStdErr = TRUE
          # ,stdErrorsVarNames = "LogRe_StdError"
          # ,interval = "confidence"
          # ,intervalVarNames = c("LogRe_LowerConfInterv", "LogRe_UpperConfInterv")
          # ,computeResiduals = TRUE
          # ,residVarNames = "LogRe_Residual"

          ,blocksPerRead = rxGetOption("blocksPerRead")
          ,reportProgress = rxGetOption("reportProgress")
          ,xdfCompressionLevel = rxGetOption("xdfCompressionLevel")
)

rxDataStep(inData = paste(strXDF, "Test_DS.xdf", sep = ""),
           outFile = paste(strXDF, "Test_DS.xdf", sep = ""),
           transforms = list(LogRe_Prediction = as.logical(round(LogRe_PredictionReal))),
           overwrite = TRUE
)

# rxGetInfo(paste(strXDF, "Test_DS.xdf", sep = ""), getVarInfo = TRUE, numRows = 3)
rxSummary(~., data = paste(strXDF, "Test_DS.xdf", sep = ""))$sDataFrame


## Creating and Viewing Statistics & Graphs ##

## Statistics ##
tmp <- rxCube(~ F(Label):F(LogRe_Prediction), data = paste(strXDF, "Test_DS.xdf", sep = ""))
Statistics(tmp, 3, paste(strXDF, "Training_DS.xdf", sep = ""))
remove(tmp)

## Graphs ##
rxRocCurve(actualVarName = "Label",
           predVarName = "LogRe_PredictionReal",
           data = paste(strXDF, "Test_DS.xdf", sep = "")
)

saveRDS(LogisticRegressionModel, "models/LogisticRegressionModel.rds")
remove(LogisticRegressionModel)

tmp <- rxDataStep(inData = Test_DS, varsToKeep = c("ID_Erga", "LogRe_PredictionReal", "LogRe_Prediction"))
write.csv(tmp, file = paste(strXDF, "LogisticRegression_Results.csv", sep = ""), row.names = FALSE)
remove(tmp)


###############################################################
### 2) Creating a Classification Model using Decision Trees ###
###############################################################

## Creating the Model ##
system.time(
  #Tweakable:
  #--Dependent Variables
  #--maxDepth
  #--method [anova/class]
  #//To be left as is on this experiment (though tweakable if needed)\\#
  #--xval: Cross Validation Folds for Pruning, 10 is great (&very time consuming)
  #--cp: 0 is the best (&most time consuming)
  TreeModel <- rxDTree(LabelFactorial ~ TimeSeriesDate + GrafioEktelesisErgou + Katigoria + Xaraktirismos_Ergou + .rxCluster
                       + Skopos_Ergou + MelClientDelay + MelDEHDelay + MelOthersDelay + Sinergio_Meletis
                       + Ektasi_Ergou + Anagi_YS #+ SAP_Typos_Pelati #+ SAP_Eidos_Aitimatos
                       + Mel_Kathisterisi_Pelati + Mel_Kathisterisi_DEH + Mel_Kathisterisi_Triton + Meres_Meletis
                       + Kostos_Ergatikon_Kataskevis + Kostos_Ilikon_Kataskevis + Kostos_Kataskevis + Kostos_Ergolavikon_Epidosis
                       + Kathisterisi_AitisisKataxorisis + Kathisterisi_Meletis + Kathisterisi_Anagelias + DayOfYearSine
                       + DayOfYearCosine + DayOfYearCartesX + DayOfYearCartesY
                       ,data = paste(strXDF, "Training_DS.xdf", sep = "")
                       ,xVal = 10
                       ,maxDepth = 15
                       ,method = "anova"
                       ,maxNumBins = round(min(1001, max(101, sqrt(n_Train))))
                       ,cp = 0
                       ,pruneCp = "auto"
                       #,maxCompete = 0
                       #,useSurrogate =
                       #,maxSurrogate = 0
                       #,surrogateStyle =
                       #,minSplit =
                       #,minBucket =
                       #,fweights =
                       #,pweights =
                       #,cost = c("")
                       #,parms = list(loss = c(0, 3, 1, 0))

                       ,blocksPerRead = rxGetOption("blocksPerRead")
                       ,reportProgress = rxGetOption("reportProgress")
                       ,xdfCompressionLevel = rxGetOption("xdfCompressionLevel")
                       # ,rowSelection =
  )
)
# TreeModel #The Tree model
# TreeModel$variable.importance   #[vector] A numerical value representing how important the variable has been to the model
# TreeModel$frame                 #[vector] var      n     wt          dev       yval   complexity ncompete nsurrogate

# write.csv(TreeModel$cptable, "TreeModel cptable.csv")
printcp(rxAddInheritance(TreeModel)) #Table of optimal prunings based on complexity
plotcp(rxAddInheritance(TreeModel))

#Based on where the decrease rate of Cross Validation Error (xerror) approaches a line,
#or better, for models fit with 2-fold or greater cross-validation, where cross-validation standard error approaches the line in the plot
#TreeModelPruned <- prune(TreeModel, cp=2.2416e-04)

## Applying the Predictions ##
rxPredict(modelObject = TreeModel,
          data = paste(strXDF, "Test_DS.xdf", sep = ""),
          outData = paste(strXDF, "Test_DS.xdf", sep = ""),
          overwrite = TRUE,
          predVarNames = "Tree_PredictionReal"
          # ,computeStdErr = TRUE
          # ,stdErrorsVarNames = "LogRe_StdError"
          # ,interval = "confidence"
          # ,intervalVarNames = c("LogRe_LowerConfInterv", "LogRe_UpperConfInterv")
          # ,computeResiduals = TRUE
          # ,residVarNames = "LogRe_Residual"

          ,blocksPerRead = rxGetOption("blocksPerRead")
          ,reportProgress = rxGetOption("reportProgress")
          ,xdfCompressionLevel = rxGetOption("xdfCompressionLevel")
)
rxDataStep(inData = paste(strXDF, "Test_DS.xdf", sep = ""),
           outFile = paste(strXDF, "Test_DS.xdf", sep = ""),
           transforms = list(Tree_Prediction = as.logical(round(Tree_PredictionReal))),
           overwrite = TRUE
)

#// For method = class \\#
# rxPredict(modelObject = TreeModelClMl5F,
#           data = paste(strXDF, "Test_DS.xdf", sep = ""),
#           outData = paste(strXDF, "Test_DS.xdf", sep = ""),
#           overwrite = TRUE,
#           predVarNames = c("0_prob", "Tree_PredictionRealClMl5F")
# )
# rxDataStep(inData = paste(strXDF, "Test_DS.xdf", sep = ""),
#            outFile = paste(strXDF, "tmp.xdf", sep = ""),
#            transforms = list(Tree_PredictionClMl5F = as.logical(round(Tree_PredictionRealClMl5F))),
#            overwrite = TRUE
# )
# rxDataStep(inData = paste(strXDF, "tmp.xdf", sep = ""),
#            outFile = paste(strXDF, "Test_DS.xdf", sep = ""),
#            varsToDrop = "0_prob",
#            overwrite = TRUE
# )
#\\ For method = class //#

#// For method = Anova \\#
# rxPredict(modelObject = TreeModelAnoMl1000,
#           data = paste(strXDF, "Test_DS.xdf", sep = ""),
#           outData = paste(strXDF, "Test_DS.xdf", sep = ""),
#           overwrite = TRUE,
#           predVarNames = "Tree_PredictionRealAnoMl1000"
# )
# rxDataStep(inData = paste(strXDF, "Test_DS.xdf", sep = ""),
#            outFile = paste(strXDF, "Test_DS.xdf", sep = ""),
#            transforms = list(Tree_PredictionAnoMl1000 = as.logical(round(Tree_PredictionRealAnoMl1000))),
#            overwrite = TRUE
# )
#\\ For method = Anova //#

# rxGetInfo(paste(strXDF, "Test_DS.xdf", sep = ""), getVarInfo = TRUE, numRows = 5)
# rxSummary(~., data = paste(strXDF, "Test_DS.xdf", sep = ""))$sDataFrame


## Creating and Viewing Statistics & Graphs ##

## Statistics ##
tmp <- rxCube(~ F(Label):F(Tree_Prediction), data = paste(strXDF, "Test_DS.xdf", sep = ""))
Statistics(tmp, 3, paste(strXDF, "Test_DS.xdf", sep = ""))
remove(tmp)
## Graphs ##
rxRocCurve(actualVarName = "Label",
           predVarName = "Tree_PredictionReal",
           data = paste(strXDF, "Test_DS.xdf", sep = ""),
           title = "Decision Tree ROC Curve"
)

#Visualise the decision tree
plot(createTreeView(TreeModel))

#USELESS || There are too many variables and each's values are too lengthy for the tree to be visualised properly
#fancyRpartPlot(rxAddInheritance(TreeModel))

#USELESS || TDrawing the Tree Plot
#drawTreeNodes(TreeModel)

#USELESS || rpart plot
#plot(rxAddInheritance(TreeModel))
#text(rxAddInheritance(TreeModel))

saveRDS(TreeModel, "models/TreeModel.rds")
remove(TreeModel)

tmp <- rxDataStep(inData = Test_DS, varsToKeep = c("ID_Erga", "Tree_PredictionReal", "Tree_Prediction"))
write.csv(tmp, file = paste(strXDF, "DecisionTrees_Results", sep = ""), row.names = FALSE)
remove(tmp)


############################################################
### 3) Creating a Classification Model using Naive Bayes ###
############################################################

## Creating the Model ##
# rxNaiveBayes()
system.time(
  #Tweakable:
  #--Dependent Variables
  #--smoothingFactor
  #//To be left as is on this experiment (though tweakable if needed)\\#
  NaiveBayesModel <- rxNaiveBayes(LabelFactorial ~ TimeSeriesDate + GrafioEktelesisErgou + Katigoria + Xaraktirismos_Ergou + .rxCluster
                                  + Skopos_Ergou + MelClientDelay + MelDEHDelay + MelOthersDelay + Sinergio_Meletis
                                  + Ektasi_Ergou + Anagi_YS #+ SAP_Typos_Pelati #+ SAP_Eidos_Aitimatos
                                  + Mel_Kathisterisi_Pelati + Mel_Kathisterisi_DEH + Mel_Kathisterisi_Triton + Meres_Meletis
                                  + Kostos_Ergatikon_Kataskevis + Kostos_Ilikon_Kataskevis + Kostos_Kataskevis + Kostos_Ergolavikon_Epidosis
                                  + Kathisterisi_AitisisKataxorisis + Kathisterisi_Meletis + Kathisterisi_Anagelias + DayOfYearSine
                                  + DayOfYearCosine + DayOfYearCartesX + DayOfYearCartesY
                                  ,data = paste(strXDF, "Training_DS.xdf", sep = "")
                                  ,smoothingFactor = 1
                                  #,fweights =
                                  #,pweights =

                                  ,blocksPerRead = rxGetOption("blocksPerRead")
                                  ,reportProgress = rxGetOption("reportProgress")
                                  ,xdfCompressionLevel = rxGetOption("xdfCompressionLevel")
                                  # ,rowSelection =
  )
)
# NaiveBayesModel #The Naive Bayes model
# NaiveBayesModel$apriori           #[valueC] Proportion of the Label for each of its categories | Cancelled 0.2289043 Approved 0.7710957

## Applying the Predictions ##
rxPredict(modelObject = NaiveBayesModel,
          data = paste(strXDF, "Test_DS.xdf", sep = ""),
          outData = paste(strXDF, "Test_DS.xdf", sep = ""),
          overwrite = TRUE
          ,predVarNames = c("NBCancelledProbabil", "NB_PredictionReal")
          ,type = "prob" #To get probabilities instead of 0/1
          # ,computeStdErr = TRUE
          # ,stdErrorsVarNames = "LogRe_StdError"
          # ,interval = "confidence" #to control whether confidence or prediction (tolerance) intervals are computed at the specified level (confLevel). These are sometimes referred to as narrow and wide intervals, respectively
          # ,intervalVarNames = c("LogRe_LowerConfInterv", "LogRe_UpperConfInterv")
          # ,computeResiduals = TRUE
          # ,residVarNames = "LogRe_Residual"

          ,blocksPerRead = rxGetOption("blocksPerRead")
          ,reportProgress = rxGetOption("reportProgress")
          ,xdfCompressionLevel = rxGetOption("xdfCompressionLevel")
)

rxDataStep(inData = paste(strXDF, "Test_DS.xdf", sep = ""),
           outFile = paste(strXDF, "tmp.xdf", sep = ""),
           transforms = list(NB_Prediction = as.logical(round(NB_PredictionReal))),
           overwrite = TRUE
)
rxDataStep(inData = paste(strXDF, "tmp.xdf", sep = ""),
           outFile = paste(strXDF, "Test_DS.xdf", sep = ""),
           varsToDrop = c("NBCancelledProbabil"),
           overwrite = TRUE
)
file.remove(paste(strXDF, "tmp.xdf", sep = ""))

# rxGetInfo(paste(strXDF, "Test_DS.xdf", sep = ""), getVarInfo = TRUE, numRows = 5)
# rxSummary(~., data = paste(strXDF, "Test_DS.xdf", sep = ""))$sDataFrame           #Naive Bayes has 35 missing values on its output

## Creating and Viewing Statistics & Graphs ##


## Statistics ##
tmp <- rxCube(~ F(LabelFactorial):F(NB_Prediction), data = paste(strXDF, "Test_DS.xdf", sep = ""))
Statistics(tmp, 3, paste(strXDF, "Test_DS.xdf", sep = ""))
remove(tmp)

## Graphs ##
rxRocCurve(actualVarName = "Label",
           predVarName = "NB_PredictionReal",
           data = paste(strXDF, "Test_DS.xdf", sep = ""),
           title = "Naive Bayes ROC Curve"
)

saveRDS(NaiveBayesModel, "models/NaiveBayesModel.rds")
remove(NaiveBayesModel)

tmp <- rxDataStep(inData = Test_DS, varsToKeep = c("ID_Erga", "NB_PredictionReal", "NB_Prediction"))
write.csv(tmp, file = paste(strXDF, "NaiveBayes_Results", sep = ""), row.names = FALSE)
remove(tmp)


##############################################################
### 4) Creating a Classification Model using Random Forest ###
##############################################################

## Creating the Model ##
system.time(
  #Tweakable:
  #--Dependent Variables
  #--cp
  #--nTree
  #--mTry #sqrt(num of vars) for classification / (num of vars)/3 for regression
  #--maxDepth
  #--method [anova/class]
  #--replace
  #//To be left as is on this experiment (though tweakable if needed)\\#
  RandForestModel <- rxDForest(Label ~ TimeSeriesDate + GrafioEktelesisErgou + Katigoria + Xaraktirismos_Ergou + .rxCluster
                               + Skopos_Ergou + MelClientDelay + MelDEHDelay + MelOthersDelay + Sinergio_Meletis
                               + Ektasi_Ergou + Anagi_YS #+ SAP_Typos_Pelati #+ SAP_Eidos_Aitimatos
                               + Mel_Kathisterisi_Pelati + Mel_Kathisterisi_DEH + Mel_Kathisterisi_Triton + Meres_Meletis
                               + Kostos_Ergatikon_Kataskevis + Kostos_Ilikon_Kataskevis + Kostos_Kataskevis + Kostos_Ergolavikon_Epidosis
                               + Kathisterisi_AitisisKataxorisis + Kathisterisi_Meletis + Kathisterisi_Anagelias + DayOfYearSine
                               + DayOfYearCosine + DayOfYearCartesX + DayOfYearCartesY
                               , data = paste(strXDF, "Training_DS.xdf", sep = "")
                               ,cp = 0
                               ,nTree = 1000
                               ,mTry = 3
                               ,maxDepth = 15
                               ,method = "anova"
                               ,importance = TRUE
                               ,maxNumBins = round(min(1001, max(101, sqrt(n_Train))))
                               # ,findSplitsInParallel = TRUE
                               # ,replace =
                               # ,strata =
                               # ,cost = c("")
                               # ,minSplit =
                               # ,minBucket =
                               # ,maxCompete = 0
                               # ,useSurrogate =
                               # ,maxSurrogate = 0
                               # ,surrogateStyle =
                               # ,fweights =
                               # ,pweights =
                               # ,parms = list(loss = c(0, 3, 1, 0))

                               ,blocksPerRead = rxGetOption("blocksPerRead")
                               ,reportProgress = rxGetOption("reportProgress")
                               ,xdfCompressionLevel = rxGetOption("xdfCompressionLevel")
                               # ,rowSelection =
  )
)
# RandForestModel
# RandForestModel$oob.err

# Applying the Predictions ##
rxPredict(modelObject = RandForestModel,
          data = paste(strXDF, "Test_DS.xdf", sep = ""),
          outData = paste(strXDF, "Test_DS.xdf", sep = ""),
          overwrite = TRUE,
          predVarNames = "RF_PredictionReal"
)
rxDataStep(inData = paste(strXDF, "Test_DS.xdf", sep = ""),
           outFile = paste(strXDF, "Test_DS.xdf", sep = ""),
           transforms = list(RF_Prediction = as.logical(round(RF_PredictionReal))),
           overwrite = TRUE
)

#// For method = class \\#
# rxPredict(modelObject = RandForestModel,
#           data = paste(strXDF, "Test_DS.xdf", sep = ""),
#           outData = paste(strXDF, "Test_DS.xdf", sep = ""),
#           overwrite = TRUE
#           # ,predVarNames = c("0_prob", "RandForestModelReal")
# )
# rxDataStep(inData = paste(strXDF, "Test_DS.xdf", sep = ""),
#            outFile = paste(strXDF, "tmp.xdf", sep = ""),
#            transforms = list(RandForestModel = as.logical(round(RandForestModelReal))),
#            overwrite = TRUE
# )
# rxDataStep(inData = paste(strXDF, "tmp.xdf", sep = ""),
#            outFile = paste(strXDF, "Test_DS.xdf", sep = ""),
#            varsToDrop = "0_prob",
#            overwrite = TRUE
# )
#\\ For method = class //#

#// For method = Anova \\#
# rxPredict(modelObject = TreeModelAnoMl1000,
#           data = paste(strXDF, "Test_DS.xdf", sep = ""),
#           outData = paste(strXDF, "Test_DS.xdf", sep = ""),
#           overwrite = TRUE,
#           predVarNames = "Tree_PredictionRealAnoMl1000"
# )
# rxDataStep(inData = paste(strXDF, "Test_DS.xdf", sep = ""),
#            outFile = paste(strXDF, "Test_DS.xdf", sep = ""),
#            transforms = list(Tree_PredictionAnoMl1000 = as.logical(round(Tree_PredictionRealAnoMl1000))),
#            overwrite = TRUE
# )
#\\ For method = Anova //#

# rxGetInfo(paste(strXDF, "Test_DS.xdf", sep = ""), getVarInfo = TRUE, numRows = 5)
# rxSummary(~., data = paste(strXDF, "Test_DS.xdf", sep = ""))$sDataFrame


## Creating and Viewing Statistics & Graphs ##

## Statistics ##
tmp <- rxCube(~ F(Label):F(RF_Prediction), data = paste(strXDF, "Test_DS.xdf", sep = ""))
Statistics(tmp, 3, Test_DS)
remove(tmp)

## Graphs ##
plot(RandForestModel)           #How the Out-Of-Bags error decreases with number of trees
rxVarImpPlot(RandForestModel)   #Importance of Variables for the model

rxRocCurve(actualVarName = "Label",
           predVarName = "RF_PredictionReal",
           data = paste(strXDF, "Test_DS.xdf", sep = ""),
           chanceGridLine = TRUE
)

saveRDS(RandForestModel, "models/RandForestModel.rds")
remove(RandForestModel)

tmp <- rxDataStep(inData = Test_DS, varsToKeep = c("ID_Erga", "RF_PredictionReal", "RF_Prediction"))
write.csv(tmp, file = paste(strXDF, "RandomForest_Results", sep = ""), row.names = FALSE)
remove(tmp)


#############################################################################
### 5) Creating a Classification Model using Stochastic Gradient Boosting ###
#############################################################################

## Creating the Model ##
system.time(
  #Tweakable:
  #--Dependent Variables
  #--cp
  #--nTree
  #--mTry
  #--learningRate
  #--lossFunction [gaussian|Regr/bernoulli|Class/multinomial|MultiClass]
  #--replace
  #//To be left as is on this experiment (though tweakable if needed)\\#
  StochGBModel <- rxBTrees(Label ~ TimeSeriesDate + GrafioEktelesisErgou + Katigoria + Xaraktirismos_Ergou + .rxCluster
                         + Skopos_Ergou + MelClientDelay + MelDEHDelay + MelOthersDelay + Sinergio_Meletis
                         + Ektasi_Ergou + Anagi_YS #+ SAP_Typos_Pelati #+ SAP_Eidos_Aitimatos
                         + Mel_Kathisterisi_Pelati + Mel_Kathisterisi_DEH + Mel_Kathisterisi_Triton + Meres_Meletis
                         + Kostos_Ergatikon_Kataskevis + Kostos_Ilikon_Kataskevis + Kostos_Kataskevis + Kostos_Ergolavikon_Epidosis
                         + Kathisterisi_AitisisKataxorisis + Kathisterisi_Meletis + Kathisterisi_Anagelias + DayOfYearSine
                         + DayOfYearCosine + DayOfYearCartesX + DayOfYearCartesY
                         , data = paste(strXDF, "Training_DS.xdf", sep = "")

                         ,cp = 0
                         ,nTree = 1000
                         ,mTry = 3
                         ,maxDepth = 15
                         ,lossFunction = "bernoulli"
                         ,importance = TRUE
                         ,maxNumBins = round(min(1001, max(101, sqrt(n_Train))))
                         # ,learningRate = 0.1
                         # ,findSplitsInParallel = TRUE
                         # ,replace =
                         # ,strata =
                         # ,cost = c("")
                         # ,minSplit =
                         # ,minBucket =
                         # ,maxCompete = 0
                         # ,useSurrogate =
                         # ,maxSurrogate = 0
                         # ,surrogateStyle =
                         # ,fweights =
                         # ,pweights =

                         ,blocksPerRead = rxGetOption("blocksPerRead")
                         ,reportProgress = rxGetOption("reportProgress")
                         ,xdfCompressionLevel = rxGetOption("xdfCompressionLevel")
                         # ,rowSelection =
  )
)
# StochGBModel #The Stochastic Gradient Boosted Decision Trees model
# StochGBModel$oob.err

# Applying the Predictions ##
rxPredict(modelObject = StochGBModel,
          data = paste(strXDF, "Test_DS.xdf", sep = ""),
          outData = paste(strXDF, "Test_DS.xdf", sep = ""),
          overwrite = TRUE,
          predVarNames = "StochGB_PredictionReal"
)
rxDataStep(inData = paste(strXDF, "Test_DS.xdf", sep = ""),
           outFile = paste(strXDF, "Test_DS.xdf", sep = ""),
           transforms = list(StochGB_Prediction = as.logical(round(StochGB_PredictionReal))),
           overwrite = TRUE
)

# rxGetInfo(paste(strXDF, "Test_DS.xdf", sep = ""), getVarInfo = TRUE, numRows = 5)
# rxSummary(~., data = paste(strXDF, "Test_DS.xdf", sep = ""))$sDataFrame

## Statistics ##
tmp <- rxCube(~ F(Label):F(StochGB_Prediction), data = paste(strXDF, "Test_DS.xdf", sep = ""))
Statistics(tmp, 3, Test_DS)
remove(tmp)

## Graphs ##
plot(StochGBModel)           #How the Out-Of-Bags error decreases with number of trees
rxVarImpPlot(StochGBModel)   #Importance of Variables for the model

rxRocCurve(actualVarName = "Label",
           predVarName = "StochGB_PredictionReal",
           data = paste(strXDF, "Test_DS.xdf", sep = ""),
           chanceGridLine = TRUE
)

saveRDS(StochGBModel, "models/StochGBModel.rds")
remove(StochGBModel)

tmp <- rxDataStep(inData = Test_DS, varsToKeep = c("ID_Erga", "StochGB_PredictionReal", "StochGB_Prediction"))
write.csv(tmp, file = paste(strXDF, "StochasticGradientBoosting_Results", sep = ""), row.names = FALSE)
remove(tmp)


##################################################################################
### 6) Creating a Classification Model using Stochastic Dual Coordinate Ascent ###
##################################################################################

## Creating the Model ##
system.time(
  #Tweakable:
  #-- Variables
  #--lossFunction [logLoss/hingeLoss/smoothHingeLoss]
  #--l2Weight
  #--l1Weight
  #--maxIterations
  #--shuffle
  #--type [binary/regression]
  #//To be left as is on this experiment (though tweakable if needed)\\#
  SDCAModel <- rxFastLinear(Label ~ TimeSeriesDate + GrafioEktelesisErgou + Katigoria + Xaraktirismos_Ergou + .rxCluster
                            + Skopos_Ergou + MelClientDelay + MelDEHDelay + MelOthersDelay + Sinergio_Meletis
                            + Ektasi_Ergou + Anagi_YS #+ SAP_Typos_Pelati #+ SAP_Eidos_Aitimatos
                            + Mel_Kathisterisi_Pelati + Mel_Kathisterisi_DEH + Mel_Kathisterisi_Triton + Meres_Meletis
                            + Kostos_Ergatikon_Kataskevis + Kostos_Ilikon_Kataskevis + Kostos_Kataskevis + Kostos_Ergolavikon_Epidosis
                            + Kathisterisi_AitisisKataxorisis + Kathisterisi_Meletis + Kathisterisi_Anagelias + DayOfYearSine
                            + DayOfYearCosine + DayOfYearCartesX + DayOfYearCartesY
                            , data = paste(strXDF, "Training_DS.xdf", sep = "")
                            ,type = "binary"
                            ,convergenceTolerance = 0.00001
                            ,normalize = "auto"
                            # ,maxIterations = 25
                            # ,lossFunction =
                            # ,l2Weight =
                            # ,l1Weight =
                            # ,shuffle = FALSE

                            ,blocksPerRead = rxGetOption("blocksPerRead")
                            ,reportProgress = rxGetOption("reportProgress")
                            # ,rowSelection =
  )
)
# summary(SDCAModel) #The Stochastic Dual Coordinate Ascend Model
# SDCAModel$coefficients


# Applying the Predictions ##
rxPredict(modelObject = SDCAModel,
          data = paste(strXDF, "Test_DS.xdf", sep = ""),
          outData = paste(strXDF, "tmp.xdf", sep = ""),
          overwrite = TRUE
)
if (type == "binary") {
  tmpVarInfo <- list(
    PredictedLabel = list(newName = "SDCA_Prediction"),
    Probability.1 = list(newName = "SDCA_PredictionReal")
  )
  rxSetVarInfo(varInfo = tmpVarInfo,
               data = paste(strXDF, "tmp.xdf", sep = "")
  )
  rxDataStep(inData = paste(strXDF, "tmp.xdf", sep = ""),
             outFile = paste(strXDF, "tmp2.xdf", sep = ""),
             varsToKeep = c("SDCA_PredictionReal", "SDCA_Prediction"),
             overwrite = TRUE
  )
} else {
  tmpVarInfo <- list(
    Score = list(newName = "SDCA_PredictionReal")
  )
  rxSetVarInfo(varInfo = tmpVarInfo,
               data = paste(strXDF, "tmp.xdf", sep = "")
  )
  rxDataStep(inData = paste(strXDF, "tmp.xdf", sep = ""),
             outFile = paste(strXDF, "tmp2.xdf", sep = ""),
             transforms = list(SDCA_Prediction = ifelse(SDCA_PredictionReal >= 0.5, 1, 0)),
             overwrite = TRUE
  )
}

CurVarNamesTest <- rxSummary(~., data = paste(strXDF, "Test_DS.xdf", sep = ""))$sDataFrame[[1]]
if ("SDCA_PredictionReal" %in% (CurVarNamesTest)) {
  rxDataStep(inData = paste(strXDF, "Test_DS.xdf", sep = ""),
             outFile = paste(strXDF, "tmp3.xdf", sep = ""),
             varsToDrop = c("SDCA_PredictionReal", "SDCA_Prediction"),
             overwrite = TRUE
  )
  Test_DS <- rxImport(inData = paste(strXDF, "tmp3.xdf", sep = ""),
                      outFile = paste(strXDF, "Test_DS.xdf", sep = ""),
                      rowsPerRead = RowsPerRead,
                      overwrite = TRUE
  )
  file.remove(paste(strXDF, "tmp3.xdf", sep = ""))
}

remove(CurVarNamesTest)
rxMerge(inData1 = paste(strXDF, "Test_DS.xdf", sep = ""),
        inData2 = paste(strXDF, "tmp2.xdf", sep = ""),
        outFile = paste(strXDF, "tmp.xdf", sep = ""),
        type = "oneToOne",
        overwrite = TRUE
)
Test_DS <- rxImport(inData = paste(strXDF, "tmp.xdf", sep = ""),
                    outFile = paste(strXDF, "Test_DS.xdf", sep = ""),
                    rowsPerRead = RowsPerRead,
                    overwrite = TRUE
)
remove(tmpVarInfo)
file.remove(paste(strXDF, "tmp.xdf", sep = ""))
file.remove(paste(strXDF, "tmp2.xdf", sep = ""))


# rxGetInfo(paste(strXDF, "Test_DS.xdf", sep = ""), getVarInfo = TRUE, numRows = 0)
# rxSummary(~., data = paste(strXDF, "Test_DS.xdf", sep = ""))$sDataFrame

## Statistics ##
tmp <- rxCube(~ F(Label):F(SDCA_Prediction), data = paste(strXDF, "Test_DS.xdf", sep = ""))
Statistics(tmp, 3, Test_DS)
remove(tmp)

## Graphs ##
rxRocCurve(actualVarName = "Label",
           predVarName = "SDCA_PredictionReal",
           data = paste(strXDF, "Test_DS.xdf", sep = ""),
           chanceGridLine = TRUE
)

saveRDS(SDCAModel, "models/SDCAModel.rds")
remove(SDCAModel)

tmp <- rxDataStep(inData = Test_DS, varsToKeep = c("ID_Erga", "SDCA_PredictionReal", "SDCA_Prediction"))
write.csv(tmp, file = paste(strXDF, "StochasticDualCoordinateAscent_Results", sep = ""), row.names = FALSE)
remove(tmp)


#######################################################################
### 7) Creating a Classification Model using Boosted Decision Trees ###
#######################################################################

## Creating the Model ##
system.time(
  #Tweakable:
  #--Variables
  #--numTrees
  #--numLeaves
  #--gainConfLevel
  #//To be left as is on this experiment (though tweakable if needed)\\#
  BDTModel <- rxFastTrees(Label ~ TimeSeriesDate + GrafioEktelesisErgou + Katigoria + Xaraktirismos_Ergou + .rxCluster
                            + Skopos_Ergou + MelClientDelay + MelDEHDelay + MelOthersDelay + Sinergio_Meletis
                            + Ektasi_Ergou + Anagi_YS #+ SAP_Typos_Pelati #+ SAP_Eidos_Aitimatos
                            + Mel_Kathisterisi_Pelati + Mel_Kathisterisi_DEH + Mel_Kathisterisi_Triton + Meres_Meletis
                            + Kostos_Ergatikon_Kataskevis + Kostos_Ilikon_Kataskevis + Kostos_Kataskevis + Kostos_Ergolavikon_Epidosis
                            + Kathisterisi_AitisisKataxorisis + Kathisterisi_Meletis + Kathisterisi_Anagelias + DayOfYearSine
                            + DayOfYearCosine + DayOfYearCartesX + DayOfYearCartesY
                            , data = paste(strXDF, "Training_DS.xdf", sep = "")
                            ,type = "binary"
                            ,numTrees = 1000
                            ,numLeaves = 25
                            ,numBins = round(min(1001, max(101, sqrt(n_Train))))
                            ,gainConfLevel = 0
                            ,unbalancedSets = TRUE
                            #,learningRate =
                            #,minSplit = 10
                            #,exampleFraction = 0.7
                            #,featureFraction = 1
                            #,splitFraction = 1
                            #,firstUsePenalty = 0
                            #,trainThreads = 8

                            ,blocksPerRead = rxGetOption("blocksPerRead")
                            ,reportProgress = rxGetOption("reportProgress")
                            # ,rowSelection =
  )
)
# summary(BDTModel) #The Boosted Decision Tree Model

# Applying the Predictions ##
rxPredict(modelObject = BDTModel,
          data = paste(strXDF, "Test_DS.xdf", sep = ""),
          outData = paste(strXDF, "tmp.xdf", sep = ""),
          overwrite = TRUE
)
tmpVarInfo <- list(
  PredictedLabel = list(newName = "BDT_Prediction"),
  Probability.1 = list(newName = "BDT_PredictionReal")
)
rxSetVarInfo(varInfo = tmpVarInfo,
             data = paste(strXDF, "tmp.xdf", sep = "")
)
rxDataStep(inData = paste(strXDF, "tmp.xdf", sep = ""),
           outFile = paste(strXDF, "tmp2.xdf", sep = ""),
           varsToKeep = c("BDT_PredictionReal", "BDT_Prediction"),
           overwrite = TRUE
)

CurVarNamesTest <- rxSummary(~., data = paste(strXDF, "Test_DS.xdf", sep = ""))$sDataFrame[[1]]
if ("BDT_PredictionReal" %in% (CurVarNamesTest)) {
  rxDataStep(inData = paste(strXDF, "Test_DS.xdf", sep = ""),
             outFile = paste(strXDF, "tmp3.xdf", sep = ""),
             varsToDrop = c("BDT_PredictionReal", "BDT_Prediction"),
             overwrite = TRUE
  )
  Test_DS <- rxImport(inData = paste(strXDF, "tmp3.xdf", sep = ""),
                      outFile = paste(strXDF, "Test_DS.xdf", sep = ""),
                      rowsPerRead = RowsPerRead,
                      overwrite = TRUE
  )
  file.remove(paste(strXDF, "tmp3.xdf", sep = ""))
}

remove(CurVarNamesTest)
rxMerge(inData1 = paste(strXDF, "Test_DS.xdf", sep = ""),
        inData2 = paste(strXDF, "tmp2.xdf", sep = ""),
        outFile = paste(strXDF, "tmp.xdf", sep = ""),
        type = "oneToOne",
        overwrite = TRUE
)
Test_DS <- rxImport(inData = paste(strXDF, "tmp.xdf", sep = ""),
                    outFile = paste(strXDF, "Test_DS.xdf", sep = ""),
                    rowsPerRead = RowsPerRead,
                    overwrite = TRUE
)
remove(tmpVarInfo)
file.remove(paste(strXDF, "tmp.xdf", sep = ""))
file.remove(paste(strXDF, "tmp2.xdf", sep = ""))


# rxGetInfo(paste(strXDF, "Test_DS.xdf", sep = ""), getVarInfo = TRUE, numRows = 0)
# rxSummary(~., data = paste(strXDF, "Test_DS.xdf", sep = ""))$sDataFrame

## Statistics ##
tmp <- rxCube(~ F(Label):F(BDT_Prediction), data = paste(strXDF, "Test_DS.xdf", sep = ""))
Statistics(tmp, 3, Test_DS)
remove(tmp)

## Graphs ##
rxRocCurve(actualVarName = "Label",
           predVarName = "BDT_PredictionReal",
           data = paste(strXDF, "Test_DS.xdf", sep = ""),
           chanceGridLine = TRUE
)

saveRDS(BDTModel, "models/BDTModel.rds")
remove(BDTModel)

tmp <- rxDataStep(inData = Test_DS, varsToKeep = c("ID_Erga", "BDT_PredictionReal", "BDT_Prediction"))
write.csv(tmp, file = paste(strXDF, "BoostedDecisionTrees_Results", sep = ""), row.names = FALSE)
remove(tmp)



###########################################################################
### 8) Creating a Classification Model using Ensemble of Decision Trees ###
###########################################################################

## Creating the Model ##
system.time(
  #Tweakable:
  #--Variables
  #--numTrees
  #--numLeaves
  #//To be left as is on this experiment (though tweakable if needed)\\#
  #--type [binary/regression]
  EoDTModel <- rxFastForest(Label ~ TimeSeriesDate + GrafioEktelesisErgou + Katigoria + Xaraktirismos_Ergou + .rxCluster
                            + Skopos_Ergou + MelClientDelay + MelDEHDelay + MelOthersDelay + Sinergio_Meletis
                            + Ektasi_Ergou + Anagi_YS #+ SAP_Typos_Pelati #+ SAP_Eidos_Aitimatos
                            + Mel_Kathisterisi_Pelati + Mel_Kathisterisi_DEH + Mel_Kathisterisi_Triton + Meres_Meletis
                            + Kostos_Ergatikon_Kataskevis + Kostos_Ilikon_Kataskevis + Kostos_Kataskevis + Kostos_Ergolavikon_Epidosis
                            + Kathisterisi_AitisisKataxorisis + Kathisterisi_Meletis + Kathisterisi_Anagelias + DayOfYearSine
                            + DayOfYearCosine + DayOfYearCartesX + DayOfYearCartesY
                            , data = paste(strXDF, "Training_DS.xdf", sep = "")
                            ,type = "binary"
                            ,numTrees = 1000
                            ,numLeaves = 25
                            ,numBins = round(min(1001, max(101, sqrt(n_Train))))
                            ,gainConfLevel = 0
                            #,minSplit = 10
                            #,exampleFraction = 0.7
                            #,featureFraction = 0.7
                            #,splitFraction = 0.7
                            #,firstUsePenalty = 0
                            #,trainThreads = 8

                            ,blocksPerRead = rxGetOption("blocksPerRead")
                            ,reportProgress = rxGetOption("reportProgress")
                            # ,rowSelection = #name of a logical variable in the data set (in quotes) or a logical expression using variables in the data set to specify row selection.
  )
)
# summary(EoDTModel) #The Stochastic Dual Coordinate Ascend Model

# Applying the Predictions ##
rxPredict(modelObject = EoDTModel,
          data = paste(strXDF, "Test_DS.xdf", sep = ""),
          outData = paste(strXDF, "tmp.xdf", sep = ""),
          overwrite = TRUE
)
tmpVarInfo <- list(
  PredictedLabel = list(newName = "EoDT_Prediction"),
  Probability.1 = list(newName = "EoDT_PredictionReal")
)
rxSetVarInfo(varInfo = tmpVarInfo,
             data = paste(strXDF, "tmp.xdf", sep = "")
)
rxDataStep(inData = paste(strXDF, "tmp.xdf", sep = ""),
           outFile = paste(strXDF, "tmp2.xdf", sep = ""),
           varsToKeep = c("EoDT_PredictionReal", "EoDT_Prediction"),
           overwrite = TRUE
)

CurVarNamesTest <- rxSummary(~., data = paste(strXDF, "Test_DS.xdf", sep = ""))$sDataFrame[[1]]
if ("EoDT_PredictionReal" %in% (CurVarNamesTest)) {
  rxDataStep(inData = paste(strXDF, "Test_DS.xdf", sep = ""),
             outFile = paste(strXDF, "tmp3.xdf", sep = ""),
             varsToDrop = c("EoDT_PredictionReal", "EoDT_Prediction"),
             overwrite = TRUE
  )
  Test_DS <- rxImport(inData = paste(strXDF, "tmp3.xdf", sep = ""),
                      outFile = paste(strXDF, "Test_DS.xdf", sep = ""),
                      rowsPerRead = RowsPerRead,
                      overwrite = TRUE
  )
  file.remove(paste(strXDF, "tmp3.xdf", sep = ""))
}

remove(CurVarNamesTest)
rxMerge(inData1 = paste(strXDF, "Test_DS.xdf", sep = ""),
        inData2 = paste(strXDF, "tmp2.xdf", sep = ""),
        outFile = paste(strXDF, "tmp.xdf", sep = ""),
        type = "oneToOne",
        overwrite = TRUE
)
Test_DS <- rxImport(inData = paste(strXDF, "tmp.xdf", sep = ""),
                    outFile = paste(strXDF, "Test_DS.xdf", sep = ""),
                    rowsPerRead = RowsPerRead,
                    overwrite = TRUE
)
remove(tmpVarInfo)
file.remove(paste(strXDF, "tmp.xdf", sep = ""))
file.remove(paste(strXDF, "tmp2.xdf", sep = ""))


# rxGetInfo(paste(strXDF, "Test_DS.xdf", sep = ""), getVarInfo = TRUE, numRows = 0)
# rxSummary(~., data = paste(strXDF, "Test_DS.xdf", sep = ""))$sDataFrame

## Statistics ##
tmp <- rxCube(~ F(Label):F(EoDT_Prediction), data = paste(strXDF, "Test_DS.xdf", sep = ""))
Statistics(tmp, 3, Test_DS)
remove(tmp)

## Graphs ##
rxRocCurve(actualVarName = "Label",
           predVarName = "EoDT_PredictionReal",
           data = paste(strXDF, "Test_DS.xdf", sep = ""),
           chanceGridLine = TRUE
)

saveRDS(EoDTModel, "models/EoDTModel.rds")
remove(EoDTModel)

tmp <- rxDataStep(inData = Test_DS, varsToKeep = c("ID_Erga", "EoDT_PredictionReal", "EoDT_Prediction"))
write.csv(tmp, file = paste(strXDF, "EnsembleofDecisionTrees_Results", sep = ""), row.names = FALSE)
remove(tmp)


################################################################
### 9) Creating a Classification Model using Neural Networks ###
################################################################

## Creating the Model ##
system.time(
  #Tweakable:
  #--Variables
  #--acceleration [sse/gpu]
  #--miniBatchSize [1-256]
  #--numHiddenNodes
  #--numIterations
  #--optimizer
  #//To be left as is on this experiment (though tweakable if needed)\\#
  #--normalize [auto/no/yes/warn]
  NNModel <- rxNeuralNet(Label ~ TimeSeriesDate + GrafioEktelesisErgou + Katigoria + Xaraktirismos_Ergou + .rxCluster
                         + Skopos_Ergou + MelClientDelay + MelDEHDelay + MelOthersDelay + Sinergio_Meletis
                         + Ektasi_Ergou + Anagi_YS #+ SAP_Typos_Pelati #+ SAP_Eidos_Aitimatos
                         + Mel_Kathisterisi_Pelati + Mel_Kathisterisi_DEH + Mel_Kathisterisi_Triton + Meres_Meletis
                         + Kostos_Ergatikon_Kataskevis + Kostos_Ilikon_Kataskevis + Kostos_Kataskevis + Kostos_Ergolavikon_Epidosis
                         + Kathisterisi_AitisisKataxorisis + Kathisterisi_Meletis + Kathisterisi_Anagelias + DayOfYearSine
                         + DayOfYearCosine + DayOfYearCartesX + DayOfYearCartesY
                         , data = paste(strXDF, "Training_DS.xdf", sep = "")
                         ,type = "binary"
                         ,numHiddenNodes = 500
                         ,numIterations = 500
                         ,acceleration = "sse"
                         ,optimizer = sgd()
                         ,miniBatchSize = 1
                         ,normalize = "auto"
                         # ,netDefinition =
                         # ,initWtsDiameter = 0.1
                         # ,maxNorm =

                         ,blocksPerRead = rxGetOption("blocksPerRead")
                         ,reportProgress = rxGetOption("reportProgress")
                         # ,rowSelection =
  )
)
# summary(NNModel) #The Neural Networks Model

# Applying the Predictions ##
rxPredict(modelObject = NNModel,
          data = paste(strXDF, "Test_DS.xdf", sep = ""),
          outData = paste(strXDF, "tmp.xdf", sep = ""),
          overwrite = TRUE
)
tmpVarInfo <- list(
  PredictedLabel = list(newName = "NN_Prediction"),
  Probability.1 = list(newName = "NN_PredictionReal")
)
rxSetVarInfo(varInfo = tmpVarInfo,
             data = paste(strXDF, "tmp.xdf", sep = "")
)
rxDataStep(inData = paste(strXDF, "tmp.xdf", sep = ""),
           outFile = paste(strXDF, "tmp2.xdf", sep = ""),
           varsToKeep = c("NN_PredictionReal", "NN_Prediction"),
           overwrite = TRUE
)

CurVarNamesTest <- rxSummary(~., data = paste(strXDF, "Test_DS.xdf", sep = ""))$sDataFrame[[1]]
if ("NN_PredictionReal" %in% (CurVarNamesTest)) {
  rxDataStep(inData = paste(strXDF, "Test_DS.xdf", sep = ""),
             outFile = paste(strXDF, "tmp3.xdf", sep = ""),
             varsToDrop = c("NN_PredictionReal", "NN_Prediction"),
             overwrite = TRUE
  )
  Test_DS <- rxImport(inData = paste(strXDF, "tmp3.xdf", sep = ""),
                      outFile = paste(strXDF, "Test_DS.xdf", sep = ""),
                      rowsPerRead = RowsPerRead,
                      overwrite = TRUE
  )
  file.remove(paste(strXDF, "tmp3.xdf", sep = ""))
}

remove(CurVarNamesTest)
rxMerge(inData1 = paste(strXDF, "Test_DS.xdf", sep = ""),
        inData2 = paste(strXDF, "tmp2.xdf", sep = ""),
        outFile = paste(strXDF, "tmp.xdf", sep = ""),
        type = "oneToOne",
        overwrite = TRUE
)
Test_DS <- rxImport(inData = paste(strXDF, "tmp.xdf", sep = ""),
                    outFile = paste(strXDF, "Test_DS.xdf", sep = ""),
                    rowsPerRead = RowsPerRead,
                    overwrite = TRUE
)
remove(tmpVarInfo)
file.remove(paste(strXDF, "tmp.xdf", sep = ""))
file.remove(paste(strXDF, "tmp2.xdf", sep = ""))


# rxGetInfo(paste(strXDF, "Test_DS.xdf", sep = ""), getVarInfo = TRUE, numRows = 0)
# rxSummary(~., data = paste(strXDF, "Test_DS.xdf", sep = ""))$sDataFrame

## Statistics ##
tmp <- rxCube(~ F(Label):F(NN_Prediction), data = paste(strXDF, "Test_DS.xdf", sep = ""))
Statistics(tmp, 3, Test_DS)
remove(tmp)

## Graphs ##
rxRocCurve(actualVarName = "Label",
           predVarName = "NN_PredictionReal",
           data = paste(strXDF, "Test_DS.xdf", sep = ""),
           chanceGridLine = TRUE,
           title = "Neural Networks"
)

saveRDS(NNModel, "models/NNModel.rds")
remove(NNModel)

tmp <- rxDataStep(inData = Test_DS, varsToKeep = c("ID_Erga", "NN_PredictionReal", "NN_Prediction"))
write.csv(tmp, file = paste(strXDF, "NeuralNetworks_Results", sep = ""), row.names = FALSE)
remove(tmp)


##########################################################################
### 10) Creating a Classification Model using Fast Logistic Regression ###
##########################################################################

## Creating the Model ##
system.time(
  #Tweakable:
  #--Variables
  #--
  #--
  #--
  #//To be left as is on this experiment (though tweakable if needed)\\#
  #--normalize [auto/no/yes/warn]
  MLLRModel <- rxLogisticRegression(Label ~ TimeSeriesDate + GrafioEktelesisErgou + Katigoria + Xaraktirismos_Ergou + .rxCluster
                                    + Skopos_Ergou + MelClientDelay + MelDEHDelay + MelOthersDelay + Sinergio_Meletis
                                    + Ektasi_Ergou + Anagi_YS #+ SAP_Typos_Pelati #+ SAP_Eidos_Aitimatos
                                    + Mel_Kathisterisi_Pelati + Mel_Kathisterisi_DEH + Mel_Kathisterisi_Triton + Meres_Meletis
                                    + Kostos_Ergatikon_Kataskevis + Kostos_Ilikon_Kataskevis + Kostos_Kataskevis + Kostos_Ergolavikon_Epidosis
                                    + Kathisterisi_AitisisKataxorisis + Kathisterisi_Meletis + Kathisterisi_Anagelias + DayOfYearSine
                                    + DayOfYearCosine + DayOfYearCartesX + DayOfYearCartesY
                                    , data = paste(strXDF, "Training_DS.xdf", sep = "")

                                    ,type = "binary"
                                    ,sgdInitTol = 0
                                    ,l2Weight = 1
                                    ,l1Weight = 1
                                    ,optTol = 1e-07
                                    ,memorySize = 20
                                    ,initWtsScale = 0
                                    ,maxIterations = 100
                                    ,normalize = "auto"
                                    #trainThreads = NULL
                                    #denseOptimizer =

                                    ,blocksPerRead = rxGetOption("blocksPerRead")
                                    ,reportProgress = rxGetOption("reportProgress")
                                    # ,rowSelection =
  )
)
# summary(MLLRModel) #The Fast Logistic Regression Model

# Applying the Predictions ##
rxPredict(modelObject = MLLRModel,
          data = paste(strXDF, "Test_DS.xdf", sep = ""),
          outData = paste(strXDF, "tmp.xdf", sep = ""),
          overwrite = TRUE
)
tmpVarInfo <- list(
  PredictedLabel = list(newName = "MLLR_Prediction"),
  Probability.1 = list(newName = "MLLR_PredictionReal")
)
rxSetVarInfo(varInfo = tmpVarInfo,
             data = paste(strXDF, "tmp.xdf", sep = "")
)
rxDataStep(inData = paste(strXDF, "tmp.xdf", sep = ""),
           outFile = paste(strXDF, "tmp2.xdf", sep = ""),
           varsToKeep = c("MLLR_PredictionReal", "MLLR_Prediction"),
           overwrite = TRUE
)

CurVarNamesTest <- rxSummary(~., data = paste(strXDF, "Test_DS.xdf", sep = ""))$sDataFrame[[1]]
if ("MLLR_PredictionReal" %in% (CurVarNamesTest)) {
  rxDataStep(inData = paste(strXDF, "Test_DS.xdf", sep = ""),
             outFile = paste(strXDF, "tmp3.xdf", sep = ""),
             varsToDrop = c("MLLR_PredictionReal", "MLLR_Prediction"),
             overwrite = TRUE
  )
  Test_DS <- rxImport(inData = paste(strXDF, "tmp3.xdf", sep = ""),
                      outFile = paste(strXDF, "Test_DS.xdf", sep = ""),
                      rowsPerRead = RowsPerRead,
                      overwrite = TRUE
  )
  file.remove(paste(strXDF, "tmp3.xdf", sep = ""))
}

remove(CurVarNamesTest)
rxMerge(inData1 = paste(strXDF, "Test_DS.xdf", sep = ""),
        inData2 = paste(strXDF, "tmp2.xdf", sep = ""),
        outFile = paste(strXDF, "tmp.xdf", sep = ""),
        type = "oneToOne",
        overwrite = TRUE
)
Test_DS <- rxImport(inData = paste(strXDF, "tmp.xdf", sep = ""),
                    outFile = paste(strXDF, "Test_DS.xdf", sep = ""),
                    rowsPerRead = RowsPerRead,
                    overwrite = TRUE
)
remove(tmpVarInfo)
file.remove(paste(strXDF, "tmp.xdf", sep = ""))
file.remove(paste(strXDF, "tmp2.xdf", sep = ""))


# rxGetVarInfo(paste(strXDF, "Test_DS.xdf", sep = ""))
# rxSummary(~., data = paste(strXDF, "Test_DS.xdf", sep = ""))$sDataFrame

## Statistics ##
tmp <- rxCube(~ F(Label):F(MLLR_Prediction), data = paste(strXDF, "Test_DS.xdf", sep = ""))
Statistics(tmp, 3, Test_DS)
remove(tmp)

## Graphs ##
rxRocCurve(actualVarName = "Label",
           predVarName = "MLLR_PredictionReal",
           data = paste(strXDF, "Test_DS.xdf", sep = ""),
           chanceGridLine = TRUE,
           title = "Logistic Regression"
)

saveRDS(MLLRModel, "models/MLLRModel.rds")
remove(MLLRModel)

tmp <- rxDataStep(inData = Test_DS, varsToKeep = c("ID_Erga", "MLLR_PredictionReal", "MLLR_Prediction"))
write.csv(tmp, file = paste(strXDF, "FastLogisticRegression_Results", sep = ""), row.names = FALSE)
remove(tmp)


rocOut <- rxRoc(actualVarName = "Label",
                predVarName = c("LogRe_PredictionReal"
                                ,"Tree_PredictionReal"
                                ,"NB_PredictionReal"
                                ,"RF_PredictionReal"
                                ,"StochGB_PredictionReal"
                                ,"SDCA_PredictionReal"
                                ,"BDT_PredictionReal"
                                ,"EoDT_PredictionReal"
                                ,"NN_PredictionReal"
                                ,"MLLR_PredictionReal"
                                ),
                data = paste(strXDF, "Test_DS.xdf", sep = "")
)
#Show ROC Information
rocOut
round(rxAuc(rocOut), 3)

plot(rocOut,
     title = "ROC Curve for Label",
     lineStyle = c("solid", "twodash", "dashed")
)

remove(rocOut)
# remove(data)
# remove(k)
# remove(n_Classification)
# remove(n_Test)
# remove(n_Train)


} else {
  "RevoScaleR not found. Please use Microsoft R Server or equivalent."
}