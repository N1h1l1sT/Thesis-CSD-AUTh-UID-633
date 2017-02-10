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
n_Classification <- rxGetInfo(data = Classification_DS)$numRows

remove(ClassificationColInfo)
file.remove(paste(strXDF, "preClassification2_DS.xdf", sep = ""))
remove(preClassification2_DS)

#Visualising the Class Imbalance
rxHistogram(~LabelFactorial, data = Classification_DS)

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
                                     # ,fweights = #If duplicate rows have been eliminated, creating a new variable of how many duplicate rows were, then this Variable/Column can be used as Frequency Weight
                                     # ,pweights = #Probablity weights for the observations
                                     
                                     ,reportProgress = rxGetOption("reportProgress")
                                     ,blocksPerRead = rxGetOption("blocksPerRead")
                                     # ,rowSelection =
                                     # ,variableSelection = #rxStepControl(method="stepwise", scope = ~ Age + Start + Number )); parameters that control aspects of stepwise regression; cube must be FALSE
  )
)
summary(LogisticRegressionModel)
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
          # ,interval = "confidence" #to control whether confidence or prediction (tolerance) intervals are computed at the specified level (confLevel). These are sometimes referred to as narrow and wide intervals, respectively
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

#rxGetInfo(paste(strXDF, "Test_DS.xdf", sep = ""), getVarInfo = TRUE, numRows = 3)
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
#Decision trees are effective algorithms widely used for classification and regression. Building a decision tree generally requires that all continuous variables be sorted in order to decide where to split the data. This sorting step becomes time and memory prohibitive when dealing with large data. Various techniques have been proposed to overcome the sorting obstacle, which can be roughly classified into two groups: performing data pre-sorting or using approximate summary statistic of the data. While pre-sorting techniques follow standard decision tree algorithms more closely, they cannot accommodate very large data sets. These big data decision trees are normally parallelized in various ways to enable large scale learning: data parallelism partitions the data either horizontally or vertically so that different processors see different observations or variables and task parallelism builds different tree nodes on different processors.
#The rxDTree algorithm is an approximate decision tree algorithm with horizontal data parallelism, especially designed for handling very large data sets. It uses histograms as the approximate compact representation of the data and builds the decision tree in a breadth-first fashion. The algorithm can be executed in parallel settings such as a multicore machine or a distributed environment with a master-worker architecture. Each worker gets only a subset of the observations of the data, but has a view of the complete tree built so far. It builds a histogram from the observations it sees, which essentially compresses the data to a fixed amount of memory. This approximate description of the data is then sent to a master with constant low communication complexity independent of the size of the data set. The master integrates the information received from each of the workers and determines which terminal tree nodes to split and how. Since the histogram is built in parallel, it can be quickly constructed even for extremely large data sets.
#The total number of passes through the data is equal to a base of maxDepth + 3, plus xVal times (maxDepth + 2), where xVal is the number of folds for cross-validation and maxDepth is the maximum tree depth.
#Scaling decision trees to very large data sets is possible with rxDTree but should be done with caution—the wrong choice of model parameters can easily lead to models that take hours or longer to estimate, even in a distributed computing environment. For example, in the Getting Started Guide, we estimated linear models using the big airline data and used the variable Origin as a predictor in several models. The Origin variable is a factor variable with 373 levels with no obvious ordering. Incorporating this variable into an rxDTree model that is performing more than two level classification can easily consume hours of computation time. To prevent such unintended consequences, rxDTree has a parameter maxUnorderedLevels which defaults to 32; in the case of Origin, this parameter would flag an error. However, a factor variable of “Region” which groups the airports of Origin by location may well be a useful proxy, and can be constructed to have only a limited number of levels. Numeric and ordered factor predictors are much more easily incorporated into the model.
#The rxDTree function has a number of options for controlling the model fit. Most of these control parameters will be familiar to rpart users, but the defaults have been modified in some cases to better support large data tree models. A full listing of these options can be found in the rxDTree help file, but the following have been found in our testing to be the most useful at controlling the time required to fit a model with rxDTree:
system.time(
  #Tweakable:
  #--Dependent Variables
  #--maxDepth
  #--method [anova/class]
  #//To be left as is on this experiment (though tweakable if needed)\\#
  #-- xval: Cross Validation Folds for Prunning, 10 is great (&very time consuming)
  #--cp: 0 is the best (&most time consuming)
  TreeModel <- rxDTree(LabelFactorial ~ TimeSeriesDate + GrafioEktelesisErgou + Katigoria + Xaraktirismos_Ergou + .rxCluster
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
                       ,maxNumBins = round(min(1001, max(101, sqrt(n_Train)))) #this controls the maximum number of bins used for each variable. Managing the number of bins is important in controlling memory usage. The default is min(1001, max(101, sqrt(num of obs))). For small data sets with continuous predictors, you may find that you need to increase the maxNumBins to obtain models that resemble those from rpart.
                       #For large data sets (100000 or more observations), you may need to adjust the following parameters to obtain meaningful models:
                       #The default cp of 0 produces a very large number of splits; specifying cp = 1e-5 produces a more manageable set of splits in this model
                       ,cp = 0 #this is a complexity parameter and sets the bar for how much a split must reduce the complexity before being accepted. We have set the default to 0 and recommend using maxDepth and minBucket to control your tree sizes. If you want to specify a cp value, start with a conservative value, such as rpart’s 0.01; if you don’t see an adequate number of splits, decrease the cp by powers of 10 until you do. For our large airline data, we have found interesting models begin with a cp of about 1e-4.
                       ,pruneCp = "auto"
                       #,maxCompete = 0 #this specifies the number of “competitor splits” retained in the output. By default, rxDTree sets this to 0, but a setting of 3 or 4 can be useful for diagnostic purposes in determining why a particular split was chosen.
                       #,useSurrogate = #0, 1 or 2
                       #,maxSurrogate = 0 #this specifies the number of surrogate splits retained in the output. Again, by default rxDTree sets this to 0. Surrogate splits are used to assign an observation when the primary split variable is missing for that observation.
                       #,surrogateStyle = #0 or 1, 0 penalises surrogates with many missing values
                       #,minSplit = #determines how many observations must be in a node before a split is attempted
                       #,minBucket =  #determines how many observations must remain in a terminal node.
                       #,fweights = #If duplicate rows have been eliminated, creating a new variable of how many duplicate rows were, then this Variable/Column can be used as Frequency Weight
                       #,pweights = #Probablity weights for the observations
                       #,cost = c("") #a vector of non-negative costs, containing one element for each variable in the model. Defaults to one for all variables. When deciding which split	to choose, the improvement on splitting on a variable is divided by its cost
                       #,parms = list(loss = c(0, 3, 1, 0))
  
                       ,blocksPerRead = rxGetOption("blocksPerRead")
                       ,reportProgress = rxGetOption("reportProgress")
                       ,xdfCompressionLevel = rxGetOption("xdfCompressionLevel")
                       # ,rowSelection = #name of a logical variable in the data set (in quotes) or a logical expression using variables in the data set to specify row selection.
  )
)
TreeModel #The Tree model
TreeModel$variable.importance   #[vector] A numerical value representing how important the variable has been to the model
# TreeModel$frame                 #[vector] var      n     wt          dev       yval   complexity ncompete nsurrogate

# write.csv(TreeModel$cptable, "TreeModel cptable.csv")
printcp(rxAddInheritance(TreeModel)) #Table of optimal prunings based on complexity
plotcp(rxAddInheritance(TreeModel))

#Based on where the decreate rate of Cross Validation Error (xerror) approaches a line,
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
          # ,interval = "confidence" #to control whether confidence or prediction (tolerance) intervals are computed at the specified level (confLevel). These are sometimes referred to as narrow and wide intervals, respectively
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
           title = "Decision Tree ROC Curve"
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

############################################################
### 3) Creating a Classification Model using Naive Bayes ###
############################################################

## Creating the Model ##
# conditional probabilities are calculated for all of the factor (categorical) variables, and
# means and standard deviations are calculated for numeric variables
# rxNaiveBayes() follows the standard practice of assuming that these variables follow Gaussian distributions.
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
                                  ,smoothingFactor = 1 #If we try to use our classifier on the test data without specifying a smoothing factor in our call to rxNaiveBayes the function rxPredict produces no results since our test data only has data from 2009. In general, smoothing is used to avoid overfitting your model. It follows that to achieve the optimal classifier you may want to smooth the conditional probabilities even if every level of each variable is observed. perform Laplace smoothing. A positive smoothing factor to account for cases not present in the training data. It avoids modeling issues by preventing zero conditional probability estimates. Since the conditional probabilities are being multiplied in the model, adding a small number to 0 probabilities, precludes missing categories from wiping out the calculation.
                                  #,fweights = #If duplicate rows have been eliminated, creating a new variable of how many duplicate rows were, then this Variable/Column can be used as Frequency Weight
                                  #,pweights = #Probablity weights for the observations
                                  
                                  ,blocksPerRead = rxGetOption("blocksPerRead")
                                  ,reportProgress = rxGetOption("reportProgress")
                                  ,xdfCompressionLevel = rxGetOption("xdfCompressionLevel")
                                  # ,rowSelection = #name of a logical variable in the data set (in quotes) or a logical expression using variables in the data set to specify row selection.
  )
)
NaiveBayesModel #The Naive Bayes model
# NaiveBayesModel$apriori           #[valueC] Proportion of the Label for each of its categories | Cancelled 0.2289043 Approved 0.7710957

## Applying the Predictions ##
NB_Pred <- rxPredict(modelObject = NaiveBayesModel,
                     data = paste(strXDF, "Test_DS.xdf", sep = ""),
                     outData = paste(strXDF, "Test_DS.xdf", sep = ""),
                     overwrite = TRUE
                     ,predVarNames = c("NBCancelledProbabil", "NB_PredictionReal")#, "tmpNB_Prediction")
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
rxSummary(~., data = paste(strXDF, "Test_DS.xdf", sep = ""))$sDataFrame           #Naive Bayes has 35 missing values on its output

## Creating and Viewing Statistics & Graphs ##


## Statistics ##
tmp <- rxCube(~ F(LabelFactorial):F(NB_Prediction), data = paste(strXDF, "Test_DS.xdf", sep = ""))
ShowStatistics(tmp, 3, paste(strXDF, "Test_DS.xdf", sep = ""))
remove(tmp)

## Graphs ##
rxRocCurve(actualVarName = "Label",
           predVarName = "NB_PredictionReal",
           data = paste(strXDF, "Test_DS.xdf", sep = ""),
           title = "Naive Bayes ROC Curve"
)

remove(NaiveBayesModel)


##############################################################
### 4) Creating a Classification Model using Random Forest ###
##############################################################

## Creating the Model ##
system.time(
  #Tweakable:
  #--Dependent Variables
  #--cp
  #--nTree
  #--mTry
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
                               #For large data sets (100000 or more observations), you may need to adjust the following parameters to obtain meaningful models:
                               #The default cp of 0 produces a very large number of splits; specifying cp = 1e-5 produces a more manageable set of splits in this model
                               ,cp = 0 #this is a complexity parameter and sets the bar for how much a split must reduce the complexity before being accepted. We have set the default to 0 and recommend using maxDepth and minBucket to control your tree sizes. If you want to specify a cp value, start with a conservative value, such as rpart’s 0.01; if you don’t see an adequate number of splits, decrease the cp by powers of 10 until you do. For our large airline data, we have found interesting models begin with a cp of about 1e-4.
                               ,nTree = 100
                               ,mTry = 3
                               ,maxDepth = 15 #this sets the maximum depth of any node of the tree. Computations grow rapidly more expensive as the depth increases, so we recommend a maxDepth of 10 to 15.
                               ,method = "anova"
                               ,importance = TRUE
                               ,maxNumBins = round(min(1001, max(101, sqrt(n_Train)))) #this controls the maximum number of bins used for each variable. Managing the number of bins is important in controlling memory usage. The default is min(1001, max(101, sqrt(num of obs))). For small data sets with continuous predictors, you may find that you need to increase the maxNumBins to obtain models that resemble those from rpart.
                               # ,findSplitsInParallel = TRUE
                               # ,replace = #a logical value specifying if the sampling of observations should be done with or without replacement
                               # ,strata = #a character string specifying the (factor) variable to use for stratified sampling.
                               # ,cost = c("") #a vector of non-negative costs, containing one element for each variable in the model. Defaults to one for all variables. When deciding which split	to choose, the improvement on splitting on a variable is divided by its cost
                               # ,minSplit = #determines how many observations must be in a node before a split is attempted
                               # ,minBucket =  #determines how many observations must remain in a terminal node.
                               # ,maxCompete = 0 #this specifies the number of “competitor splits” retained in the output. By default, rxDTree sets this to 0, but a setting of 3 or 4 can be useful for diagnostic purposes in determining why a particular split was chosen.
                               # ,useSurrogate = #0, 1 or 2
                               # ,maxSurrogate = 0 #this specifies the number of surrogate splits retained in the output. Again, by default rxDTree sets this to 0. Surrogate splits are used to assign an observation when the primary split variable is missing for that observation.
                               # ,surrogateStyle = #0 or 1, 0 penalises surrogates with many missing values
                               # ,fweights = #If duplicate rows have been eliminated, creating a new variable of how many duplicate rows were, then this Variable/Column can be used as Frequency Weight
                               # ,pweights = #Probablity weights for the observations
                               # ,parms = list(loss = c(0, 3, 1, 0))
                               
                               ,blocksPerRead = rxGetOption("blocksPerRead")
                               ,reportProgress = rxGetOption("reportProgress")
                               ,xdfCompressionLevel = rxGetOption("xdfCompressionLevel")
                               # ,rowSelection = #name of a logical variable in the data set (in quotes) or a logical expression using variables in the data set to specify row selection.
  )
)
RandForestModel #The Random Forest model
RandForestModel$oob.err        #[vector] a data frame containing the out-of-bag error estimate. For classification forests, this includes the OOB error estimate, which represents the proportion of times the predicted class is not equal to the true class, and the cumulative number of out-of-bag observations for the forest. For regression forests, this includes the OOB error estimate, which here represents the sum of squared residuals of the out-of-bag observations divided by the number of out-of-bag observations, the number of out-of-bag observations, the out-of-bag variance, and the “pseudo-R-Squared”, which is 1 minus the quotient of the oob.err and oob.var.

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
rxSummary(~., data = paste(strXDF, "Test_DS.xdf", sep = ""))$sDataFrame


## Creating and Viewing Statistics & Graphs ##

## Statistics ##
tmp <- rxCube(~ F(Label):F(RF_Prediction), data = paste(strXDF, "Test_DS.xdf", sep = ""))
ShowStatistics(tmp, 3, Test_DS)
remove(tmp)

## Graphs ##
plot(RandForestModel)           #How the Out-Of-Bags error decreases with number of trees
rxVarImpPlot(RandForestModel)   #Importance of Variables for the model

rxRocCurve(actualVarName = "Label",
           predVarName = "RF_PredictionReal",
           data = paste(strXDF, "Test_DS.xdf", sep = ""),
           chanceGridLine = TRUE
)

remove(RandForestModel)


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
                         #For large data sets (100000 or more observations), you may need to adjust the following parameters to obtain meaningful models:
                         #The default cp of 0 produces a very large number of splits; specifying cp = 1e-5 produces a more manageable set of splits in this model
                         
                         ,cp = 0 #this is a complexity parameter and sets the bar for how much a split must reduce the complexity before being accepted. We have set the default to 0 and recommend using maxDepth and minBucket to control your tree sizes. If you want to specify a cp value, start with a conservative value, such as rpart’s 0.01; if you don’t see an adequate number of splits, decrease the cp by powers of 10 until you do. For our large airline data, we have found interesting models begin with a cp of about 1e-4.
                         ,nTree = 100
                         ,mTry = 3
                         ,maxDepth = 15 #this sets the maximum depth of any node of the tree. Computations grow rapidly more expensive as the depth increases, so we recommend a maxDepth of 10 to 15.
                         ,lossFunction = "bernoulli"
                         ,importance = TRUE
                         ,maxNumBins = round(min(1001, max(101, sqrt(n_Train)))) #this controls the maximum number of bins used for each variable. Managing the number of bins is important in controlling memory usage. The default is min(1001, max(101, sqrt(num of obs))). For small data sets with continuous predictors, you may find that you need to increase the maxNumBins to obtain models that resemble those from rpart.
                         # ,learningRate = 0.1 #(shrinkage) is used to scale the contribution of each tree when it is added to the ensemble. The default learning rate is 0.1.. numeric scalar specifying the learning rate of the boosting procedure
                         # ,findSplitsInParallel = TRUE
                         # ,replace = #a logical value specifying if the sampling of observations should be done with or without replacement
                         # ,strata = #a character string specifying the (factor) variable to use for stratified sampling.
                         # ,cost = c("") #a vector of non-negative costs, containing one element for each variable in the model. Defaults to one for all variables. When deciding which split	to choose, the improvement on splitting on a variable is divided by its cost
                         # ,minSplit = #determines how many observations must be in a node before a split is attempted
                         # ,minBucket =  #the minimum number of observations in a terminal node (or leaf). By default, this is minSplit/3
                         # ,maxCompete = 0 #this specifies the number of “competitor splits” retained in the output. By default, rxDTree sets this to 0, but a setting of 3 or 4 can be useful for diagnostic purposes in determining why a particular split was chosen.
                         # ,useSurrogate = #0, 1 or 2
                         # ,maxSurrogate = 0 #this specifies the number of surrogate splits retained in the output. Again, by default rxDTree sets this to 0. Surrogate splits are used to assign an observation when the primary split variable is missing for that observation.
                         # ,surrogateStyle = #0 or 1, 0 penalises surrogates with many missing values
                         # ,fweights = #If duplicate rows have been eliminated, creating a new variable of how many duplicate rows were, then this Variable/Column can be used as Frequency Weight
                         # ,pweights = #Probablity weights for the observations
                         
                         ,blocksPerRead = rxGetOption("blocksPerRead")
                         ,reportProgress = rxGetOption("reportProgress")
                         ,xdfCompressionLevel = rxGetOption("xdfCompressionLevel")
                         # ,rowSelection = #name of a logical variable in the data set (in quotes) or a logical expression using variables in the data set to specify row selection.
  )
)
StochGBModel #The Stochastic Gradient Boosted Decision Trees model
StochGBModel$oob.err #a data frame containing the out-of-bag error estimate. For classification forests, this includes the OOB error estimate, which represents the proportion of times the predicted class is not equal to the true class, and the cumulative number of out-of-bag observations for the forest. For regression forests, this includes the OOB error estimate, which here represents the sum of squared residuals of the out-of-bag observations divided by the number of out-of-bag observations, the number of out-of-bag observations, the out-of-bag variance, and the “pseudo-R-Squared”, which is 1 minus the quotient of the oob.err and oob.var.

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

#// For method = class \\#
# rxPredict(modelObject = StochGBModel,
#           data = paste(strXDF, "Test_DS.xdf", sep = ""),
#           outData = paste(strXDF, "Test_DS.xdf", sep = ""),
#           overwrite = TRUE
#           # ,predVarNames = c("0_prob", "StochGB_PredictionReal")
# )
# rxDataStep(inData = paste(strXDF, "Test_DS.xdf", sep = ""),
#            outFile = paste(strXDF, "tmp.xdf", sep = ""),
#            transforms = list(StochGB_Prediction = as.logical(round(StochGB_PredictionReal))),
#            overwrite = TRUE
# )
# rxDataStep(inData = paste(strXDF, "tmp.xdf", sep = ""),
#            outFile = paste(strXDF, "Test_DS.xdf", sep = ""),
#            varsToDrop = "0_prob",
#            overwrite = TRUE
# )
#\\ For method = class //#

#// For method = Anova \\#
# rxPredict(modelObject = StochGBModel,
#           data = paste(strXDF, "Test_DS.xdf", sep = ""),
#           outData = paste(strXDF, "Test_DS.xdf", sep = ""),
#           overwrite = TRUE,
#           predVarNames = "StochGB_PredictionReal"
# )
# rxDataStep(inData = paste(strXDF, "Test_DS.xdf", sep = ""),
#            outFile = paste(strXDF, "Test_DS.xdf", sep = ""),
#            transforms = list(StochGB_Prediction = as.logical(round(StochGB_PredictionReal))),
#            overwrite = TRUE
# )
#\\ For method = Anova //#

# rxGetInfo(paste(strXDF, "Test_DS.xdf", sep = ""), getVarInfo = TRUE, numRows = 5)
rxSummary(~., data = paste(strXDF, "Test_DS.xdf", sep = ""))$sDataFrame

## Statistics ##
tmp <- rxCube(~ F(Label):F(StochGB_Prediction), data = paste(strXDF, "Test_DS.xdf", sep = ""))
ShowStatistics(tmp, 3, Test_DS)
remove(tmp)

## Graphs ##
plot(StochGBModel)           #How the Out-Of-Bags error decreases with number of trees
rxVarImpPlot(StochGBModel)   #Importance of Variables for the model

rxRocCurve(actualVarName = "Label",
           predVarName = "StochGB_PredictionReal",
           data = paste(strXDF, "Test_DS.xdf", sep = ""),
           chanceGridLine = TRUE
)

remove(StochGBModel)


##################################################################################
### 6) Creating a Classification Model using Stochastic Dual Coordinate Ascent ###
##################################################################################
#The rxFastLinear() algorithm is based on the Stochastic Dual Coordinate Ascent (SDCA) method, a state-of-the-art optimization technique for convex objective functions. The algorithm can be scaled for use on large out-of-memory data sets due to a semi-asynchronized implementation that supports multithreaded processing. Several choices of loss functions are also provided and elastic net regularization is supported. The SDCA method combines several of the best properties and capabilities of logistic regression and SVM algorithms.
## Creating the Model ##
system.time(
  #Tweakable:
  #-- Variables
  #--lossFunction [logLoss/hingeLoss/smoothHingeLoss]
  #--l2Weight
  #--l1Weight
  #--maxIterations
  #--shuffle
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
                            ,convergenceTolerance = 0.0001 #Specifies the tolerance threshold used as a convergence criterion. It must be between 0 and 1. The default value is 0.1. The algorithm is considered to have converged if the relative duality gap, which is the ratio between the duality gap and the primal loss, falls below the specified convergence tolerance.
                            ,normalize = "auto" #Specifies the type of automatic normalization used: [auto/no/yes/warn] "warn": if normalization is needed, a warning message is displayed, but normalization is not performed. Normalization rescales disparate data ranges to a standard scale. Feature scaling insures the distances between data points are proportional and enables various optimization methods such as gradient descent to converge much faster. If normalization is performed, a MaxMin normalizer is used. It normalizes values in an interval [a, b] where -1 <= a <= 0 and 0 <= b <= 1 and b - a = 1. This normalizer preserves sparsity by mapping zero to zero
                            # ,maxIterations = 25 #Specifies an upper bound on the number of training iterations. This parameter must be positive or NULL. If NULL is specified, the actual value is automatically computed based on data set. Each iteration requires a complete pass over the training data. Training terminates after the total number of iterations reaches the specified upper bound or when the loss function converges, whichever happens earlier.
                            # ,lossFunction = #Specifies the empirical loss function to optimize. For binary classification, the following choices are available: logLoss: The log-loss. This is the default. hingeLoss: The SVM hinge loss. Its parameter represents the margin size. smoothHingeLoss: The smoothed hinge loss. Its parameter represents the smoothing constant. For linear regression, squared loss squaredLoss is currently supported.
                            # ,l2Weight = #Specifies the L2 regularization weight. The value must be either non-negative or NULL. If NULL is specified, the actual value is automatically computed based on data set
                            # ,l1Weight = #Specifies the L1 regularization weight. The value must be either non-negative or NULL. If NULL is specified, the actual value is automatically computed based on data set
                            # ,shuffle = FALSE #Specifies whether to shuffle the training data. Set TRUE to shuffle the data; FALSE not to shuffle. The default value is FALSE. SDCA is a stochastic optimization algorithm. If shuffling is turned on, the training data is shuffled on each iteration.

                            ,blocksPerRead = rxGetOption("blocksPerRead")
                            ,reportProgress = rxGetOption("reportProgress")
                            # ,rowSelection = #name of a logical variable in the data set (in quotes) or a logical expression using variables in the data set to specify row selection.
  )
)
summary(SDCAModel) #The Stochastic Dual Coordinate Ascend Model
SDCAModel$coefficients


# Applying the Predictions ##
rxPredict(modelObject = SDCAModel,
          data = paste(strXDF, "Test_DS.xdf", sep = ""),
          outData = paste(strXDF, "tmp.xdf", sep = ""),
          overwrite = TRUE
)
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
rxSummary(~., data = paste(strXDF, "Test_DS.xdf", sep = ""))$sDataFrame

## Statistics ##
tmp <- rxCube(~ F(Label):F(SDCA_Prediction), data = paste(strXDF, "Test_DS.xdf", sep = ""))
ShowStatistics(tmp, 3, Test_DS)
remove(tmp)

## Graphs ##
rxRocCurve(actualVarName = "Label",
           predVarName = "SDCA_PredictionReal",
           data = paste(strXDF, "Test_DS.xdf", sep = ""),
           chanceGridLine = TRUE
)

remove(SDCAModel)


######################################################################
### 7) Creating a Classification Model using Boosted Decision Tree ###
######################################################################
#The rxFastTrees() algorithm is a high performing, state of the art scalable boosted decision tree that implements FastRank, an efficient implementation of the MART gradient boosting algorithm. MART learns an ensemble of regression trees, which is a decision tree with scalar values in its leaves. For binary classification, the output is converted to a probability by using some form of calibration.
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
                            ,numTrees = 1000 #Specifies the total number of decision trees to create in the ensemble.By creating more decision trees, you can potentially get better coverage, but the training time increases. The default value is 100
                            ,numLeaves = 25 #The maximum number of leaves (terminal nodes) that can be created in any tree. Higher values potentially increase the size of the tree and get better precision, but risk overfitting and requiring longer training times. The default value is 20.
                            ,numBins = round(min(1001, max(101, sqrt(n_Train)))) #this controls the maximum number of bins used for each variable. Managing the number of bins is important in controlling memory usage. The default is min(1001, max(101, sqrt(num of obs))). For small data sets with continuous predictors, you may find that you need to increase the maxNumBins to obtain models that resemble those from rpart.
                            ,gainConfLevel = 0 #Tree fitting gain confidence requirement (should be in the range [0,1)). The default value is 0.
                            ,unbalancedSets = TRUE
                            #,learningRate = #Determines the size of the step taken in the direction of the gradient in each step of the learning process. This determines how fast or slow the learner converges on the optimal solution. If the step size is too big, you might overshoot the optimal solution. If the step size is too samll, training takes longer to converge to the best solution.
                            #,minSplit = 10 #Minimum number of training instances required to form a leaf. That is, the minimal number of documents allowed in a leaf of a regression tree, out of the sub-sampled data. A 'split' means that features in each level of the tree (node) are randomly divided. The default value is 10. Only the number of instances is counted even if instances are weighted.
                            #,exampleFraction = 0.7 #The fraction of randomly chosen instances to use for each tree. The default value is 0.7
                            #,featureFraction = 1 #The fraction of randomly chosen features to use for each tree. The default value is 1
                            #,splitFraction = 1 #The fraction of randomly chosen features to use on each split. The default value is 1
                            #,firstUsePenalty = 0 #The feature first use penalty coefficient. This is a form of regularization that incurs a penalty for using a new feature when creating the tree. Increase this value to create trees that don't use many features. The default value is 0.
                            #,trainThreads = 8
                            
                            ,blocksPerRead = rxGetOption("blocksPerRead")
                            ,reportProgress = rxGetOption("reportProgress")
                            # ,rowSelection = #name of a logical variable in the data set (in quotes) or a logical expression using variables in the data set to specify row selection.
  )
)
summary(BDTModel) #The Stochastic Dual Coordinate Ascend Model

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
rxSummary(~., data = paste(strXDF, "Test_DS.xdf", sep = ""))$sDataFrame

## Statistics ##
tmp <- rxCube(~ F(Label):F(BDT_Prediction), data = paste(strXDF, "Test_DS.xdf", sep = ""))
ShowStatistics(tmp, 3, Test_DS)
remove(tmp)

## Graphs ##
rxRocCurve(actualVarName = "Label",
           predVarName = "BDT_PredictionReal",
           data = paste(strXDF, "Test_DS.xdf", sep = ""),
           chanceGridLine = TRUE
)

remove(BDTModel)



###########################################################################
### 8) Creating a Classification Model using Ensemble of Decision Trees ###
###########################################################################
#The rxFastForest() algorithm is a random forest that provides a learning method for classification that constructs an ensemble of decision trees at training time, outputting the class that is the mode of the classes of the individual trees. Random decision forests can correct for the overfitting to training data sets to which decision trees are prone.
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
                            ,numTrees = 5000 #Specifies the total number of decision trees to create in the ensemble.By creating more decision trees, you can potentially get better coverage, but the training time increases. The default value is 100
                            ,numLeaves = 30 #The maximum number of leaves (terminal nodes) that can be created in any tree. Higher values potentially increase the size of the tree and get better precision, but risk overfitting and requiring longer training times. The default value is 20.
                            ,numBins = round(min(1001, max(101, sqrt(n_Train)))) #this controls the maximum number of bins used for each variable. Managing the number of bins is important in controlling memory usage. The default is min(1001, max(101, sqrt(num of obs))). For small data sets with continuous predictors, you may find that you need to increase the maxNumBins to obtain models that resemble those from rpart.
                            ,gainConfLevel = 0 #Tree fitting gain confidence requirement (should be in the range [0,1)). The default value is 0.
                           
                            
                            #,minSplit = 10 #Minimum number of training instances required to form a leaf. That is, the minimal number of documents allowed in a leaf of a regression tree, out of the sub-sampled data. A 'split' means that features in each level of the tree (node) are randomly divided. The default value is 10. Only the number of instances is counted even if instances are weighted.
                            #,exampleFraction = 0.7 #The fraction of randomly chosen instances to use for each tree. The default value is 0.7
                            #,featureFraction = 0.7 #The fraction of randomly chosen features to use for each tree. The default value is 1
                            #,splitFraction = 0.7 #The fraction of randomly chosen features to use on each split. The default value is 1
                            #,firstUsePenalty = 0 #The feature first use penalty coefficient. This is a form of regularization that incurs a penalty for using a new feature when creating the tree. Increase this value to create trees that don't use many features. The default value is 0.
                            #,trainThreads = 8
                            
                            ,blocksPerRead = rxGetOption("blocksPerRead")
                            ,reportProgress = rxGetOption("reportProgress")
                            # ,rowSelection = #name of a logical variable in the data set (in quotes) or a logical expression using variables in the data set to specify row selection.
  )
)
summary(EoDTModel) #The Stochastic Dual Coordinate Ascend Model

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
rxSummary(~., data = paste(strXDF, "Test_DS.xdf", sep = ""))$sDataFrame

## Statistics ##
tmp <- rxCube(~ F(Label):F(EoDT_Prediction), data = paste(strXDF, "Test_DS.xdf", sep = ""))
ShowStatistics(tmp, 3, Test_DS)
remove(tmp)

## Graphs ##
rxRocCurve(actualVarName = "Label",
           predVarName = "EoDT_PredictionReal",
           data = paste(strXDF, "Test_DS.xdf", sep = ""),
           chanceGridLine = TRUE
)

remove(EoDTModel)



################################################################
### 9) Creating a Classification Model using Neural Networks ###
################################################################
#The rxNeuralNet() algorithm supports a user-defined multilayer network topology with GPU acceleration. A neural network is a class of prediction models inspired by the human brain. It can be represented as a weighted directed graph. Each node in the graph is called a neuron. The neural network algorithm tries to learn the optimal weights on the edges based on the training data. Any class of statistical models can be considered a neural network if they use adaptive weights and can approximate non-linear functions of their inputs. Neural network regression is especially suited to problems where a more traditional regression model cannot fit a solution.
## Creating the Model ##
system.time(
  #Tweakable:
  #--Variables
  #--
  #--
  #--
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
                         ,numHiddenNodes = 1000 #The default number of hidden nodes in the neural net. The default value is 100
                         ,numIterations = 100 #The number of iterations on the full training set. The default value is 100
                         ,acceleration = "gpu" #[sse/gpu]
                         ,optimizer = sgd() #[sgd()/adaDeltaSgd()] A list specifying either the sgd or adaptive optimization algorithm. This list can be created using sgd or adaDeltaSgd. The default value is sgd.
                         
                         # ,miniBatchSize = 1 #[1/256] Sets the mini-batch size. Recommended values are between 1 and 256. This parameter is only used when the acceleration is GPU. Setting this parameter to a higher value improves the speed of training, but it might negatively affect the accuracy. The default value is 1.
                         # ,netDefinition = #The Net# definition of the structure of the neural network. For more information about the Net# language, see https://docs.microsoft.com/en-us/azure/machine-learning/machine-learning-azure-ml-netsharp-reference-guide
                         # ,initWtsDiameter = 0.1 #Sets the initial weights diameter that specifies the range from which values are drawn for the initial learning weights. The weights are initialized randomly from within this range. The default value is 0.1.
                         # ,maxNorm = #Specifies an upper bound to constrain the norm of the incoming weight vector at each hidden unit. This can be very important in maxout neural networks as well as in cases where training produces unbounded weights
                         # ,normalize = "auto" #[auto/no/yes/warn] "warn": if normalization is needed, a warning message is displayed, but normalization is not performed. Normalization rescales disparate data ranges to a standard scale. Feature scaling insures the distances between data points are proportional and enables various optimization methods such as gradient descent to converge much faster. If normalization is performed, a MaxMin normalizer is used. It normalizes values in an interval [a, b] where -1 <= a <= 0 and 0 <= b <= 1 and b - a = 1. This normalizer preserves sparsity by mapping zero to zero
                         
                         
                         ,blocksPerRead = rxGetOption("blocksPerRead")
                         ,reportProgress = rxGetOption("reportProgress")
                         # ,rowSelection = #name of a logical variable in the data set (in quotes) or a logical expression using variables in the data set to specify row selection.
  )
)
summary(NNModel) #The Stochastic Dual Coordinate Ascend Model

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
rxSummary(~., data = paste(strXDF, "Test_DS.xdf", sep = ""))$sDataFrame

## Statistics ##
tmp <- rxCube(~ F(Label):F(NN_Prediction), data = paste(strXDF, "Test_DS.xdf", sep = ""))
ShowStatistics(tmp, 3, Test_DS)
remove(tmp)

## Graphs ##
rxRocCurve(actualVarName = "Label",
           predVarName = "NN_PredictionReal",
           data = paste(strXDF, "Test_DS.xdf", sep = ""),
           chanceGridLine = TRUE,
           title = "Neural Networks"
)

remove(NNModel)


#####################################################################
### 10) Creating a Classification Model using Logistic Regression ###
#####################################################################
#The rxLogisticRegression() algorithm is used to predict the value of a categorical dependent variable from its relationship to one or more independent variables assumed to have a logistic distribution. If the dependent variable has only two possible values (success/failure), then the logistic regression is binary. If the dependent variable has more than two possible values (blood type given diagnostic test results), then the logistic regression is multinomial.
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
                                    
                                    ,sgdInitTol = 0 #Set to a number greater than 0 to use Stochastic Gradient Descent (SGD) to find the initial parameters. A non-zero value set specifies the tolerance SGD uses to determine convergence. The default value is 0 specifying that SGD is not used.
                                    
                                    ,l2Weight = 1 #The L2 regularization weight. Its value must be greater than or equal to 0 and the default value is set to 1
                                    ,l1Weight = 1 #The L1 regularization weight. Its value must be greater than or equal to 0 and the default value is set to 1
                                    ,optTol = 1e-07 #Threshold value for optimizer convergence. If the improvement between iterations is less than the threshold, the algorithm stops and returns the current model. Smaller values are slower, but more accurate. The default value is 1e-07
                                    ,memorySize = 20 #Memory size for L-BFGS, specifying the number of past positions and gradients to store for the computation of the next step. This optimization parameter limits the amount of memory that is used to compute the magnitude and direction of the next step. When you specify less memory, training is faster but less accurate. Must be greater than or equal to 1 and the default value is 20.
                                    ,initWtsScale = 0 #Sets the initial weights diameter that specifies the range from which values are drawn for the initial weights. These weights are initialized randomly from within this range. For example, if the diameter is specified to be d, then the weights are uniformly distributed between -d/2 and d/2. The default value is 0, which specifies that allthe weights are initialized to 0.
                                    ,maxIterations = 100 #Sets the maximum number of iterations. After this number of steps, the algorithm stops even if it has not satisfied convergence criteria
                                    ,normalize = "auto" #[auto/no/yes/warn] "warn": if normalization is needed, a warning message is displayed, but normalization is not performed. Normalization rescales disparate data ranges to a standard scale. Feature scaling insures the distances between data points are proportional and enables various optimization methods such as gradient descent to converge much faster. If normalization is performed, a MaxMin normalizer is used. It normalizes values in an interval [a, b] where -1 <= a <= 0 and 0 <= b <= 1 and b - a = 1. This normalizer preserves sparsity by mapping zero to zero
                                    #trainThreads = NULL #The number of threads to use in training the model. This should be set to the number of cores on the machine. Note that L-BFGS multi-threading attempts to load dataset into memory. In case of out-of-memory issues, set trainThreads to 1 to turn off multi-threading. If NULL the number of threads to use is determined internally. The default value is NULL
                                    #denseOptimizer = #If TRUE, forces densification of the internal optimization vectors. If FALSE, enables the logistic regression optimizer use sparse or dense internal states as it finds appropriate. Setting denseOptimizer to TRUE requires the internal optimizer to use a dense internal state, which may help alleviate load on the garbage collector for some varieties of larger problems
                                    
                                    
                                    ,blocksPerRead = rxGetOption("blocksPerRead")
                                    ,reportProgress = rxGetOption("reportProgress")
                                    # ,rowSelection = #name of a logical variable in the data set (in quotes) or a logical expression using variables in the data set to specify row selection.
  )
)
summary(MLLRModel) #The Stochastic Dual Coordinate Ascend Model

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


# rxGetInfo(paste(strXDF, "Test_DS.xdf", sep = ""), getVarInfo = TRUE, numRows = 0)
rxSummary(~., data = paste(strXDF, "Test_DS.xdf", sep = ""))$sDataFrame

## Statistics ##
tmp <- rxCube(~ F(Label):F(MLLR_Prediction), data = paste(strXDF, "Test_DS.xdf", sep = ""))
ShowStatistics(tmp, 3, Test_DS)
remove(tmp)

## Graphs ##
rxRocCurve(actualVarName = "Label",
           predVarName = "MLLR_PredictionReal",
           data = paste(strXDF, "Test_DS.xdf", sep = ""),
           chanceGridLine = TRUE,
           title = "Logistic Regression"
)

remove(MLLRModel)

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
rxAuc(rocOut)

plot(rocOut,
     title = "ROC Curve for Label",
     lineStyle = c("solid", "twodash", "dashed")
)

remove(rocOut)
remove(data)
remove(k)
remove(n_Classification)
remove(n_Test)
remove(n_Train)
remove(NB_Pred)
