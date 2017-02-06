#################
### Functions ###
#################
Spaces <- function(NumOfSpaces) {
  str <- ""

  if (NumOfSpaces > 0) {
    for (i in 1:NumOfSpaces) {
      str = paste(str, " ", sep = "")
    }
  }

  return(str)
}

#####################################
### Creating the confusion matrix ###
#####################################
ConfMatrix <- function(ActAndPredValues, RoundAt) {
  TN <- ActAndPredValues$Counts[1]
  FN <- ActAndPredValues$Counts[2]
  FP <- ActAndPredValues$Counts[3]
  TP <- ActAndPredValues$Counts[4]

  MaxItemLength <- max(nchar(ActAndPredValues$Counts))

  strConfMatrix <- paste(Spaces(nchar("Pred_Value  ")), "Actual_Value", Spaces(((2 * MaxItemLength) + nchar(paste("  "))) - nchar("Actual_Value")), sep = "")
  strConfMatrix <- rbind(strConfMatrix, paste("Pred_Value  " , Spaces(MaxItemLength - 1) , "0  " , Spaces(MaxItemLength - 1) , "1", sep = ""))
  strConfMatrix <- rbind(strConfMatrix, paste(Spaces(nchar("Pred_Value  ") - 3), "0  " , Spaces(MaxItemLength - nchar(TN)), TN, "  ",
                        Spaces(MaxItemLength - nchar(FN)), FN, sep = ""))
  strConfMatrix <- rbind(strConfMatrix, paste(Spaces(nchar("Pred_Value  ") - 3), "1  " , Spaces(MaxItemLength - nchar(FP)), FP, "  ",
                        Spaces(MaxItemLength - nchar(TP)), TP, sep = ""))


  ActAndPredValues <- as.data.frame(ActAndPredValues)
  ActAndPredValues$Result <- c("True Negative", "False Negative", "False Positive", "True Positive")
  ActAndPredValues$PCT <- round(ActAndPredValues$Counts / sum(ActAndPredValues$Counts), RoundAt) * 100
  ActAndPredValues$Rates <- round(c(TN / (TN + FP),
                                    FN / (FN + TP),
                                    FP / (TN + FP),
                                    TP / (FN + TP)), RoundAt)
  names(ActAndPredValues) <- c("Actual Value", "Predicted Value", "Cases", "Results", "Percentage", "Rates")
  retList <- list(strConfMatrix, ActAndPredValues)
  return(retList)
}

###############################
### Printing all Statistics ###
###############################
#Applied Predictive Modeling By Max Kuhn and Kjell Johnson ISBN 978-1-4614-6849-3 http://www.springer.com/gp/book/9781461468486
#In relation to Bayesian statistics, the sensitivity and specificity are the conditional probabilities, the prevalence is the prior, and the positive/negative predicted values are the posterior probabilities.
ShowStatistics <- function(ActAndPredValues, RoundAt, DataVarOrPath) {
  TN <- ActAndPredValues$Counts[1]
  FN <- ActAndPredValues$Counts[2]
  FP <- ActAndPredValues$Counts[3]
  TP <- ActAndPredValues$Counts[4]

  tmp <- rxSummary(~LabelFactorial, data = DataVarOrPath)$categorical
  MaxClass <- max(tmp[[1]][[2]])
  MinClass <- min(tmp[[1]][[2]])

  P0 <- (TP + TN) / (TN + FN + FP + TP)
  Ma <- ((TP + FP) * (TP + FN)) / (TN + FN + FP + TP)
  Mb <- ((FN + TN) * (FP + TN)) / (TN + FN + FP + TP)
  Pe <- (Ma + Mb) / (TN + FN + FP + TP)

  Stat <- list(
    ConfusionMatrix = ConfMatrix(ActAndPredValues, RoundAt),
    TotalPredictionPercentages = data.frame(Correctly = round(100 * (TN + TP) / sum(ActAndPredValues$Counts), RoundAt),
                                            Incorrectly = round(100 * (FN + FP) / sum(ActAndPredValues$Counts), RoundAt)),
    Measures = data.frame(
      #This is a weighted average of the true positive rate (recall) and precision, their harmonic mean.
      #F-score, like recall and precision, only considers the so-called positive predictions, with recall being the probability of predicting just the positive class, precision being the probability of a positive prediction being correct, and F-score equating these probabilities under the effective assumption that the positive labels and the positive predictions should have the same distribution and prevalence
      F1 = round((2 * TP) / ((2 * TP) + FP + FN), RoundAt),
      #indicates the central tendency or typical value of a set of numbers
      G1 = round(sqrt((TP / (TP + FP)) * (TP / (TP + FN))), RoundAt),
      G2 = round(sqrt((((TP / (TP + FN) * (FP + TP) / (TN + FN + FP + TP)) / (TP / (TP + FN) * (FP + TP) / (TN + FN + FP + TP)) + (1 - (TN / (TN + FP))) * (1 - ((FP + TP) / (TN + FN + FP + TP))))) * (TP / (TP + FN))), RoundAt),
      #Matthews correlation coefficient is a measure of the quality of binary classifications. The MCC is in essence a correlation coefficient between the observed and predicted binary classifications; it returns a value between −1 and +1. A coefficient of +1 represents a perfect prediction, 0 no better than random prediction and −1 indicates total disagreement between prediction and observation. is generally regarded as a balanced measure which can be used even if the classes are of very different sizes.
      PhiMCC = round((((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))), RoundAt),
      #A measure of how well the classifier performed as compared to how well it would have performed simply by chance. In other words, a model will have a high Kappa score if there is a big difference between the accuracy and the null error rate.
      CohensK = round((P0 - Pe) / (1 - Pe), RoundAt),
      #Youden's J statistic (also called Youden's index). estimates the probability of an informed decision.
      #Its value ranges from -1 to 1, and has a zero value when a diagnostic test gives the same proportion of positive results for groups with and without the disease, i.e the test is useless.
      #Youden's index is often used in conjunction with Receiver Operating Characteristic (ROC) analysis.[2] The index is defined for all points of an ROC curve, and the maximum value of the index may be used as a criterion for selecting the optimum cut-off point when a diagnostic test gives a numeric rather than a dichotomous result.
      YoudensJ = round((TP/(TP+FN)) + (TN/(TN+FP)) - 1, RoundAt)
    ),
    Rates = data.frame(
      #The accuracy paradox for predictive analytics states that predictive models with a given level of accuracy may have greater predictive power than models with higher accuracy. It may be better to avoid the accuracy metric in favor of other metrics such as precision and recall
      #Overall, how often is the classifier correct?
      Accuracy = round((TN + TP) / (TN + FN + FP + TP), RoundAt),
      BalancedAccuracy = round(((TP / (TP + FN)) + (TN / (TN + FP))) / 2, RoundAt),
      DetectionRate = round(TP / (TN + FN + FP + TP), RoundAt),
      #Overall, how often is it wrong?
      MisclassRate = round((FP + FN) / (TN + FN + FP + TP), RoundAt),
      #When it's actually yes, how often does it predict yes?
      SensitRecallTPR = round(TP / (TP + FN), RoundAt),
      #When it's actually no, how often does it predict yes? The proportion of all negatives that still yield positive test outcomes, i.e., the conditional probability of a positive test result given an event that was not present.
      FPR = round(FP / (FP + TN), RoundAt),
      #When it's actually no, how often does it predict no?
      SpecificityTNR = round(TN / (TN + FP), RoundAt),
      #The proportion of positives which yield negative test outcomes with the test, i.e., the conditional probability of a negative test result given that the condition being looked for is present
      FNR = round(FN / (FN + TP), RoundAt),
      #A description of random errors, a measure of statistical variability; when it predicts yes, how often is it correct?
      PrecisionPPV1 = round(TP / (TP + FP), RoundAt),
      #Very similar to precision, except that it takes prevalence into account. In the case where the classes are perfectly balanced (meaning the prevalence is 50%), the positive predictive value (PPV) is equivalent to precision
      PPV2 = round(((TP / (TP + FN) * (FP + TP) / (TN + FN + FP + TP)) / (TP / (TP + FN) * (FP + TP) / (TN + FN + FP + TP)) + (1 - (TN / (TN + FP))) * (1 - ((FP + TP) / (TN + FN + FP + TP)))), RoundAt),
      #When it predicts no, how often is it correct
      NPV1 = round(TN / (TN + FN), RoundAt),
      #Very similar to precision, except that it takes prevalence into account. In the case where the classes are perfectly balanced (meaning the prevalence is 50%), the negative predictive value (NPV2) is equivalent to NPV1
      NPV2 = round((TN / (TN + FP) * (1 - (FP + TP) / (TN + FN + FP + TP))) / ((1 - TP / (TP + FN)) * (FP + TP) / (TN + FN + FP + TP) + TN / (TN + FP) * (1 - (FP + TP) / (TN + FN + FP + TP))), RoundAt),
      #A way of conceptualizing the rate of type I errors in null hypothesis testing when conducting multiple comparisons
      FDR = round(FP / (FP + TP), RoundAt),
      #The accuracy paradox for predictive analytics states that predictive models with a given level of accuracy may have greater predictive power than models with higher accuracy. It may be better to avoid the accuracy metric in favor of other metrics such as precision and recall
      #This is how often you would be wrong if you always predicted the majority class. This can be a useful baseline metric to compare your classifier against. However, the best classifier for a particular application will sometimes have a higher error rate than the null error rate, as demonstrated by the Accuracy Paradox.
      NullErrorRate = round(MinClass / MaxClass, RoundAt),
      #How often does the yes condition actually occur in our sample?
      Prevalence = round((FP + TP) / (TN + FN + FP + TP), RoundAt)
    )
  )
  return(Stat)
}


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
#One needs to import the xdf trhough RxXdfData to a var and give that to the inData for the transformation to work.
#Keep it in mind because many an hour can go wasted otherwise. Talking from experience.
ClassificationColInfo <- list("LabelFactorial" = list(type = "factor", levels = c("0", "1"), newLevels = c("Cancelled", "Approved")))
Classification_DS <- rxImport(inData = preClassification2_DS,
                              outFile = paste(strXDF, "Classification_DS.xdf", sep = ""),
                              colInfo = ClassificationColInfo,
                              overwrite = TRUE
)
remove(ClassificationColInfo)
file.remove(paste(strXDF, "preClassification2_DS.xdf", sep = ""))
remove(preClassification2_DS)

#Visualising the Class Imbalance
rxHistogram(~LabelFactorial, data = Classification_DS)

#Forming the Training set
#The temporary object is removed later on when it's outlived its usefulness
tmp <- rxDataStep(inData = Classification_DS,
                  varsToKeep = c("SelectionRatio")
)
ActualTrainingPercentage <- length(subset(tmp$SelectionRatio, tmp$SelectionRatio <= 8)) / nrow(tmp) * 100
ActualTrainingPercentage
rxDataStep(inData = paste(strXDF, "Classification_DS.xdf", sep = ""),
           outFile = paste(strXDF, "Training_DS.xdf", sep = ""),
           rowSelection = SelectionRatio <= 8, #About 80% of the data
           varsToDrop = "SelectionRatio",
           blocksPerRead = 20,
           rowsPerRead = RowsPerRead,
           overwrite = TRUE
)
Training_DS <- RxXdfData(file = paste(strXDF, "Training_DS.xdf", sep = ""))

#forming the test set
ActualTestPercentage <- length(subset(tmp$SelectionRatio, tmp$SelectionRatio > 8)) / nrow(tmp) * 100
ActualTestPercentage
remove(tmp) #It is of no use after this, so removing it.
rxDataStep(inData = paste(strXDF, "Classification_DS.xdf", sep = ""),
           outFile = paste(strXDF, "Test_DS.xdf", sep = ""),
           rowSelection = SelectionRatio > 8, #About 20% of the data
           varsToDrop = "SelectionRatio",
           blocksPerRead = 20,
           rowsPerRead = RowsPerRead,
           overwrite = TRUE
)
Test_DS <- RxXdfData(file = paste(strXDF, "Test_DS.xdf", sep = ""))

remove(ActualTestPercentage)
remove(ActualTrainingPercentage)

####################################################################
### 1) Creating a Classification Model using Logistic Regression ###
####################################################################

## Creating the Model ##

#SAP_Eidos_Aitimatos creates loads of error messages, ergo it is removed
#rxLogit will only classify cases where no variables are missing, and since SAP_Typos_Pelati is mostly missing, it's deemed imperative that it be excluded
system.time(
  LogisticRegressionModel <- rxLogit(Label ~ TimeSeriesDate + GrafioEktelesisErgou + Katigoria + Xaraktirismos_Ergou
                                     + Skopos_Ergou + MelClientDelay + MelDEHDelay + MelOthersDelay + Sinergio_Meletis
                                     + Ektasi_Ergou + Anagi_YS #+ SAP_Typos_Pelati #+ SAP_Eidos_Aitimatos
                                     + Mel_Kathisterisi_Pelati + Mel_Kathisterisi_DEH + Mel_Kathisterisi_Triton + Meres_Meletis
                                     + Kostos_Ergatikon_Kataskevis + Kostos_Ilikon_Kataskevis + Kostos_Kataskevis + Kostos_Ergolavikon_Epidosis
                                     + Kathisterisi_AitisisKataxorisis + Kathisterisi_Meletis + Kathisterisi_Anagelias + DayOfYearSine
                                     + DayOfYearCosine + DayOfYearCartesX + DayOfYearCartesY
                                     , data = paste(strXDF, "Training_DS.xdf", sep = ""),
                                     reportProgress = rxGetOption("reportProgress")
  )
)
summary(LogisticRegressionModel)

# rxGetInfo(paste(strXDF, "Training_DS.xdf", sep = ""), getVarInfo=TRUE, numRows = 3)
# rxSummary(~., data = paste(strXDF, "Training_DS.xdf", sep = ""))$sDataFrame

## Applying the Predictions ##
rxPredict(modelObject = LogisticRegressionModel,
          data = paste(strXDF, "Test_DS.xdf", sep = ""),
          outData = paste(strXDF, "Test_DS.xdf", sep = ""),
          overwrite = TRUE,
          predVarNames = "LogRe_PredictionReal"
)

rxDataStep(inData = paste(strXDF, "Test_DS.xdf", sep = ""),
           outFile = paste(strXDF, "Test_DS.xdf", sep = ""),
           transforms = list(LogRe_Prediction = as.logical(round(LogRe_PredictionReal))),
           overwrite = TRUE
)

# rxGetInfo(paste(strXDF, "Test_DS.xdf", sep = ""), getVarInfo = TRUE, numRows = 5)
rxSummary(~., data = paste(strXDF, "Test_DS.xdf", sep = ""))$sDataFrame


## Creating and Viewing Statistics & Graphs ##

## Statistics ##
tmp <- rxCube(~ F(Label):F(LogRe_Prediction), data = paste(strXDF, "Test_DS.xdf", sep = ""))
ShowStatistics(tmp, 3, paste(strXDF, "Training_DS.xdf", sep = ""))
remove(tmp)

## Graphs ##
rxRocCurve(actualVarName = "Label",
           predVarName = "LogRe_PredictionReal",
           data = paste(strXDF, "Test_DS.xdf", sep = "")
)

remove(LogisticRegressionModel)

###############################################################
### 2) Creating a Classification Model using Decision Trees ###
###############################################################

## Creating the Model ##
system.time(
  #The total number of passes through the data is equal to a base of maxDepth + 3, plus xVal times (maxDepth + 2), where xVal is the number of folds for cross-validation and maxDepth is the maximum tree depth.
  #Scaling decision trees to very large data sets is possible with rxDTree but should be done with caution—the wrong choice of model parameters can easily lead to models that take hours or longer to estimate, even in a distributed computing environment. For example, in the Getting Started Guide, we estimated linear models using the big airline data and used the variable Origin as a predictor in several models. The Origin variable is a factor variable with 373 levels with no obvious ordering. Incorporating this variable into an rxDTree model that is performing more than two level classification can easily consume hours of computation time. To prevent such unintended consequences, rxDTree has a parameter maxUnorderedLevels which defaults to 32; in the case of Origin, this parameter would flag an error. However, a factor variable of “Region” which groups the airports of Origin by location may well be a useful proxy, and can be constructed to have only a limited number of levels. Numeric and ordered factor predictors are much more easily incorporated into the model.
  #The rxDTree function has a number of options for controlling the model fit. Most of these control parameters will be familiar to rpart users, but the defaults have been modified in some cases to better support large data tree models. A full listing of these options can be found in the rxDTree help file, but the following have been found in our testing to be the most useful at controlling the time required to fit a model with rxDTree:
  TreeModel <- rxDTree(LabelFactorial ~ TimeSeriesDate + GrafioEktelesisErgou + Katigoria + Xaraktirismos_Ergou
                       + Skopos_Ergou + MelClientDelay + MelDEHDelay + MelOthersDelay + Sinergio_Meletis
                       + Ektasi_Ergou + Anagi_YS #+ SAP_Typos_Pelati #+ SAP_Eidos_Aitimatos
                       + Mel_Kathisterisi_Pelati + Mel_Kathisterisi_DEH + Mel_Kathisterisi_Triton + Meres_Meletis
                       + Kostos_Ergatikon_Kataskevis + Kostos_Ilikon_Kataskevis + Kostos_Kataskevis + Kostos_Ergolavikon_Epidosis
                       + Kathisterisi_AitisisKataxorisis + Kathisterisi_Meletis + Kathisterisi_Anagelias + DayOfYearSine
                       + DayOfYearCosine + DayOfYearCartesX + DayOfYearCartesY
                       ,data = paste(strXDF, "Training_DS.xdf", sep = "")
                       ,xVal = 10 #this controls the number of folds used to perform cross-validation. The default of 2 allows for some pruning; once you have closed in a model you may want to increase the value for final fitting and pruning.
                       ,maxDepth = 15 #this sets the maximum depth of any node of the tree. Computations grow rapidly more expensive as the depth increases, so we recommend a maxDepth of 10 to 15.
                       ,method = "anova"
                       ,maxNumBins = 323 #this controls the maximum number of bins used for each variable. Managing the number of bins is important in controlling memory usage. The default is to use the larger of 101 and the square root of the number of observations for small to moderate size data sets (up to about one million observations), but for larger sets to use 1001 bins. For small data sets with continuous predictors, you may find that you need to increase the maxNumBins to obtain models that resemble those from rpart.
                       ,xdfCompressionLevel = rxGetOption("xdfCompressionLevel")
                       ,reportProgress = rxGetOption("reportProgress")
                       ,blocksPerRead = rxGetOption("blocksPerRead")
                       ,fweights = #If duplicate rows have been eliminated, creating a new variable of how many duplicate rows were, then this Variable/Column can be used as Frequency Weight
                       #,maxCompete = 0 #this specifies the number of “competitor splits” retained in the output. By default, rxDTree sets this to 0, but a setting of 3 or 4 can be useful for diagnostic purposes in determining why a particular split was chosen.
                       #,maxSurrogate = 0 #this specifies the number of surrogate splits retained in the output. Again, by default rxDTree sets this to 0. Surrogate splits are used to assign an observation when the primary split variable is missing for that observation.
                       #For large data sets (100000 or more observations), you may need to adjust the following parameters to obtain meaningful models:
                       #The default cp of 0 produces a very large number of splits; specifying cp = 1e-5 produces a more manageable set of splits in this model
                       ,cp = 0 #this is a complexity parameter and sets the bar for how much a split must reduce the complexity before being accepted. We have set the default to 0 and recommend using maxDepth and minBucket to control your tree sizes. If you want to specify a cp value, start with a conservative value, such as rpart’s 0.01; if you don’t see an adequate number of splits, decrease the cp by powers of 10 until you do. For our large airline data, we have found interesting models begin with a cp of about 1e-4.
                       #,minSplit = #determines how many observations must be in a node before a split is attempted
                       #,minBucket =  #determines how many observations must remain in a terminal node.
                      )
)
TreeModel #The Tree model
#write.csv(TreeModel$cptable, "TreeModel cptable.csv")
printcp(rxAddInheritance(TreeModel)) #Table of optimal prunings based on complexity
plotcp(rxAddInheritance(TreeModel))

#Based on where the decreate rate of Cross Validation Error (xerror) approaches a line,
#or better, for models fit with 2-fold or greater cross-validation, where cross-validation standard error approaches the line in the plot
TreeModelPruned <- prune(TreeModel, cp=2.2416e-04)

## Applying the Predictions ##
rxPredict(modelObject = TreeModelPruned,
          data = paste(strXDF, "Test_DS.xdf", sep = ""),
          outData = paste(strXDF, "Test_DS.xdf", sep = ""),
          overwrite = TRUE,
          predVarNames = "Tree_PredictionReal"
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
rxSummary(~., data = paste(strXDF, "Test_DS.xdf", sep = ""))$sDataFrame


## Creating and Viewing Statistics & Graphs ##

## Statistics ##
tmp <- rxCube(~ F(Label):F(Tree_Prediction), data = paste(strXDF, "Test_DS.xdf", sep = ""))
ShowStatistics(tmp, 3, paste(strXDF, "Test_DS.xdf", sep = ""))
remove(tmp)

## Graphs ##
rxRocCurve(actualVarName = "Label",
           predVarName = "Tree_PredictionReal",
           data = paste(strXDF, "Test_DS.xdf", sep = ""),
           title = "Decision Tree ROC Curve",
           chanceGridLine = TRUE,
           computeAuc = TRUE
)

#Visualise the decision tree
#In this interactive tree, click on the circular split nodes to expand or collapse the tree branch. Clicking a node will expand and collapse the node to the last view of that branch. If you use a CTRL + Click, the tree will display only the children of the selected node. If you click ALT + Click, the tree will display all levels below the selected node. The square-shaped nodes, called leaf or terminal nodes, cannot be expanded.
plot(createTreeView(TreeModel))

#USELESS || There are too many variables and each's values are too lengthy for the tree to be visualised properly
#fancyRpartPlot(rxAddInheritance(TreeModel))

#USELESS || TDrawing the Tree Plot
#drawTreeNodes(TreeModel)

#USELESS || rpart plot
#plot(rxAddInheritance(TreeModel))
#text(rxAddInheritance(TreeModel))

remove(TreeModel)
remove(TreeModelPruned)

############################################################
### 3) Creating a Classification Model using Naive Bayes ###
############################################################

## Creating the Model ##
# conditional probabilities are calculated for all of the factor (categorical) variables, and
# means and standard deviations are calculated for numeric variables
# rxNaiveBayes() follows the standard practice of assuming that these variables follow Gaussian distributions.
system.time(
  NaiveBayesModel <- rxNaiveBayes(LabelFactorial ~ TimeSeriesDate + GrafioEktelesisErgou + Katigoria + Xaraktirismos_Ergou
                                  + Skopos_Ergou + MelClientDelay + MelDEHDelay + MelOthersDelay + Sinergio_Meletis
                                  + Ektasi_Ergou + Anagi_YS #+ SAP_Typos_Pelati #+ SAP_Eidos_Aitimatos
                                  + Mel_Kathisterisi_Pelati + Mel_Kathisterisi_DEH + Mel_Kathisterisi_Triton + Meres_Meletis
                                  + Kostos_Ergatikon_Kataskevis + Kostos_Ilikon_Kataskevis + Kostos_Kataskevis + Kostos_Ergolavikon_Epidosis
                                  + Kathisterisi_AitisisKataxorisis + Kathisterisi_Meletis + Kathisterisi_Anagelias + DayOfYearSine
                                  + DayOfYearCosine + DayOfYearCartesX + DayOfYearCartesY
                                  , data = paste(strXDF, "Training_DS.xdf", sep = ""),
                                  smoothingFactor = 1
  )
)
NaiveBayesModel #The Naive Bayes model

## Applying the Predictions ##
NB_Pred <- rxPredict(modelObject = NaiveBayesModel,
                     data = paste(strXDF, "Test_DS.xdf", sep = ""),
                     outData = paste(strXDF, "Test_DS.xdf", sep = ""),
                     overwrite = TRUE
                     ,predVarNames = c("NBCancelledProbabil", "NB_PredictionReal")#, "tmpNB_Prediction")
                     , type="prob"
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
rxSummary(~., data = paste(strXDF, "Test_DS.xdf", sep = ""))$sDataFrame           #Naive Bayes has 35 missing values on its output

## Creating and Viewing Statistics & Graphs ##


## Statistics ##
tmp <- rxCube(~ F(LabelFactorial):F(NB_Prediction), data = paste(strXDF, "Test_DS.xdf", sep = ""))
ShowStatistics(tmp, 3, paste(strXDF, "Test_DS.xdf", sep = ""))
remove(tmp)

## Graphs ##
#Probably the most straightforward and intuitive metric for classifier performance is accuracy, Unfortunately, there are circumstances where simple accuracy does not work well.
#For example, with a disease that only affects 1 in a million people a completely bogus screening test that always reports “negative” will be 99.9999% accurate. Unlike accuracy, ROC curves are insensitive to class imbalance; the bogus screening test would have an AUC of 0.5, which is like not having a test at all.
#visualizes the performance of a binary classifier and summarises the performance of a classifier over all possible thresholds.
#It is generated by plotting the True Positive Rate (y-axis) against the False Positive Rate (x-axis) as you vary the threshold for assigning observations to a given class
#That is a huge benefit of using an ROC curve to evaluate a classifier instead of a simpler metric such as misclassification rate, in that an ROC curve visualizes all possible classification thresholds, whereas misclassification rate only represents your error rate for a single threshold
#The probabilistic interpretation is that if you randomly choose a positive case and a negative case, the probability that the positive case outranks the negative case according to the classifier is given by the AUC. This is evident from the figure, where the total area of the plot is normalized to one, the cells of the matrix enumerate all possible combinations of positive and negative cases, and the fraction under the curve comprises the cells where the positive case outranks the negative one
#By default, the threshold is 0.5, giving a certain amount of TPR and FPR, however viewing the ROC Curve we can overrule the default value and shape the label to our design.
#a classifier that does a very good job separating the classes will have an ROC curve that hugs the upper left corner of the plot. Conversely, a classifier that does a very poor job separating the classes will have an ROC curve that is close to this black diagonal line. That line essentially represents a classifier that does no better than random guessing
#Naturally, you might want to use the ROC curve to quantify the performance of a classifier, and give a higher score for this classifier than this classifier. That is the purpose of AUC, which stands for Area Under the Curve.
# you have to decide whether you would rather minimize your False Positive Rate or maximize your True Positive Rate. the ROC curve will help you to visually understand the impact of that choice
#A second note about this diagram is that it shows a case where your predicted probabilities have a very smooth shape, similar to a normal distribution. That was just for demonstration purposes. The probabilities output by your classifier will not necessarily follow any particular shape.
#The first note is that the ROC curve and AUC are insensitive to whether your predicted probabilities are properly calibrated to actually represent probabilities of class membership. In other words, the ROC curve and the AUC would be identical even if your predicted probabilities ranged from 0.9 to 1 instead of 0 to 1, as long as the ordering of observations by predicted probability remained the same. All the AUC metric cares about is how well your classifier separated the two classes, and thus it is said to only be sensitive to rank ordering. You can think of AUC as representing the probability that a classifier will rank a randomly chosen positive observation higher than a randomly chosen negative observation, and thus it is a useful metric even for datasets with highly unbalanced classes
rocOut <- rxRoc(actualVarName = "Label",
                     predVarName = c("LogRe_PredictionReal", "Tree_PredictionReal", "NB_PredictionReal"),
                     # predVarName = c("Tree_PredictionReal"),
                     # predVarName = c("Tree_PredictionReal",
                     #                 "Tree_PredictionRealAnoMl5",
                     #                 "Tree_PredictionRealAnoMl40",
                     #                 "Tree_PredictionRealAnoMl250",
                     #                 "Tree_PredictionRealAnoMl1000",
                     #                 "Tree_PredictionRealClMl5",
                     #                 "Tree_PredictionRealClMl5F",
                     #                 "Tree_PredictionRealClMl40",
                     #                 "Tree_PredictionRealClMl40F",
                     #                 "Tree_PredictionRealClMl250",
                     #                 "Tree_PredictionRealClMl1000F",
                     #                 "Tree_PredictionRealClMl1000"),
                     data = paste(strXDF, "Test_DS.xdf", sep = "")
)
#Show ROC Information
rocOut
rxAuc(rocOut)

plot(rocOut,
     title = "ROC Curve for Label",
     lineStyle = c("solid", "twodash", "dashed")
)
remove(rocOut)

# rxHistogram(~LogRe_PredictionReal | Label,
#             data = paste(strXDF, "Test_DS.xdf", sep = "")
#             #,histType = "Percent"
# )

remove(NaiveBayesModel)



##############################################################
### 4) Creating a Classification Model using Random Forest ###
##############################################################

## Creating the Model ##
system.time(
  RandForestModel <- rxDForest(Label ~ TimeSeriesDate + GrafioEktelesisErgou + Katigoria + Xaraktirismos_Ergou
                               + Skopos_Ergou + MelClientDelay + MelDEHDelay + MelOthersDelay + Sinergio_Meletis
                               + Ektasi_Ergou + Anagi_YS #+ SAP_Typos_Pelati #+ SAP_Eidos_Aitimatos
                               + Mel_Kathisterisi_Pelati + Mel_Kathisterisi_DEH + Mel_Kathisterisi_Triton + Meres_Meletis
                               + Kostos_Ergatikon_Kataskevis + Kostos_Ilikon_Kataskevis + Kostos_Kataskevis + Kostos_Ergolavikon_Epidosis
                               + Kathisterisi_AitisisKataxorisis + Kathisterisi_Meletis + Kathisterisi_Anagelias + DayOfYearSine
                               + DayOfYearCosine + DayOfYearCartesX + DayOfYearCartesY
                               , data = paste(strXDF, "Training_DS.xdf", sep = "")
                               ,blocksPerRead = 1
                               ,maxDepth = 40
                               ,reportProgress = rxGetOption("reportProgress")
                               ,method = "anova"
  )
)
RandForestModel #The Random Forest model

## Applying the Predictions ##
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
# rxPredict(modelObject = TreeModelClMl1000,
#           data = paste(strXDF, "Test_DS.xdf", sep = ""),
#           outData = paste(strXDF, "Test_DS.xdf", sep = ""),
#           overwrite = TRUE,
#           predVarNames = c("0_prob", "Tree_PredictionRealClMl1000")
# )
# rxDataStep(inData = paste(strXDF, "Test_DS.xdf", sep = ""),
#            outFile = paste(strXDF, "tmp.xdf", sep = ""),
#            transforms = list(Tree_PredictionClMl1000 = as.logical(round(Tree_PredictionRealClMl1000))),
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
rxSummary(~., data = paste(strXDF, "Test_DS.xdf", sep = ""))$sDataFrame


## Creating and Viewing Statistics & Graphs ##

## Statistics ##
tmp <- rxCube(~ F(Label):F(RF_Prediction), data = paste(strXDF, "Test_DS.xdf", sep = ""))
ShowStatistics(tmp, 3, Test_DS)
remove(tmp)

## Graphs ##
rxRocCurve(actualVarName = "Label",
           predVarName = "RF_PredictionReal",
           data = paste(strXDF, "Test_DS.xdf", sep = ""),
           chanceGridLine = TRUE
)

#Visualise the decision tree


remove(TreeModel)






#The rxFastLinear() algorithm is based on the Stochastic Dual Coordinate Ascent (SDCA) method, a state-of-the-art optimization technique for convex objective functions. The algorithm can be scaled for use on large out-of-memory data sets due to a semi-asynchronized implementation that supports multithreaded processing. Several choices of loss functions are also provided and elastic net regularization is supported. The SDCA method combines several of the best properties and capabilities of logistic regression and SVM algorithms.

#The rxFastTrees() algorithm is a high performing, state of the art scalable boosted decision tree that implements FastRank, an efficient implementation of the MART gradient boosting algorithm. MART learns an ensemble of regression trees, which is a decision tree with scalar values in its leaves. For binary classification, the output is converted to a probability by using some form of calibration.

#The rxFastForest() algorithm is a random forest that provides a learning method for classification that constructs an ensemble of decision trees at training time, outputting the class that is the mode of the classes of the individual trees. Random decision forests can correct for the overfitting to training data sets to which decision trees are prone.

#The rxNeuralNet() algorithm supports a user-defined multilayer network topology with GPU acceleration. A neural network is a class of prediction models inspired by the human brain. It can be represented as a weighted directed graph. Each node in the graph is called a neuron. The neural network algorithm tries to learn the optimal weights on the edges based on the training data. Any class of statistical models can be considered a neural network if they use adaptive weights and can approximate non-linear functions of their inputs. Neural network regression is especially suited to problems where a more traditional regression model cannot fit a solution.

#The rxLogisticRegression() algorithm is used to predict the value of a categorical dependent variable from its relationship to one or more independent variables assumed to have a logistic distribution. If the dependent variable has only two possible values (success/failure), then the logistic regression is binary. If the dependent variable has more than two possible values (blood type given diagnostic test results), then the logistic regression is multinomial.

