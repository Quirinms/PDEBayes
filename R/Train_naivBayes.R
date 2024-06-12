Train_naivBayes=function(TrainX, TrainY,
                         Plausible = TRUE, Gaussian = TRUE,
                         PDEVersion = 2, Centered = TRUE, Capped = TRUE,
                         ...){
  # V = Train_naivBayes(TrainX, TrainY, Gaussian = TRUE)
  # 
  # INPUT
  # TrainX[1:n,1:d]    Numeric matrix for TrainX with n observations and d features.
  # TrainY[1:n]         Numeric vector with class labels
  # 
  # OPTIONAL
  # Gaussian         (Optional: Default=TRUE). TRUE: Assume gaussian distribution.         
  # 
  # OUTPUT
  # TrainYTrain[1:n]           Numeric vector with training TrainY.
  # Posteriors[1:n, 1:l]    Numeric matrices with posterior probabilities
  #                         according to the bayesian Theorem.
  # Priors[1:l]             Numeric vector with prior probability for each class.
  # c_2List_Train
  # Thetas
  #
  
  # Default application of RobustNormalization
  #RobustNormalization
  #TrainX = RobustNormalization(Data = TrainX, Capped = TRUE, na.rm = TRUE)
  #TrainX = RobustNormalization(Data = TrainX, Centered = Centered, Capped = TRUE, na.rm = TRUE)
  VTrafo = RobustNormalization(Data = TrainX, Centered = Centered,
                               Capped = Capped, na.rm = TRUE,
                               WithBackTransformation = TRUE)
  Trafo  = list("MinX" = VTrafo$MinX, "MaxX" = VTrafo$MaxX, "Denom" = VTrafo$Denom, "Center" = VTrafo$Center)
  TrainX = VTrafo$TransformedData
  
  # Cast TrainY to a classification vector with following conditions:
  # Only integers, enumerating all classes from 1 to k, where k
  # equals the max number of class types
  # The class enumeration does not have any gaps!
  # Cast(TrainY)
  # Save the cast for the prediction method
  #UniqueTrainY = unique(TrainY)
  #NumCls       = length(UniqueTrainY)
  #NewCls = TrainY
  #for(i in 1:NumCls){
  #  tmpIdx = which(TrainY == UniqueTrainY[i])
  #  NewCls[tmpIdx] = i
  #}
  
  
  TrainY       = as.factor(TrainY)
  UniqueTrainY = unique(TrainY)
  NumCls       = length(UniqueTrainY)
  
  # Welches ist die beste?
  # Fixe Einstellungen nach Capping, Verschiebung, Skalierung, ...
  # Anwendbarkeit auf Testdaten sicherstellen
  
  # Prior
  Priors            = getPriors(TrainY)                                        # Get Priors: Class proportions within TrainX
  
  # Likelihood
  c_2List_Train     = GetLikelihoods(Data = TrainX, Cls = TrainY, Gaussian = Gaussian,
                                     PDEVersion = PDEVersion)   #
  # c_2List_Train = GetLikelihoods(TrainX,TrainY,Gaussian = Gaussian,PDEVersion=PDEVersion,UserExts=UserExts)
  #c_2List_Train     = GetLikelihoods(TrainX, TrainY, Gaussian = Gaussian)        # 
  c_Kernels_list    = c_2List_Train$c_Kernels_list                          # 
  ListOfLikelihoods = c_2List_Train$ListOfLikelihoods                       # 
  Thetas            = c_2List_Train$Thetas                                  # 
  
  # Interpolate estimates so that input TrainX can be provided with estimated values
  if(isFALSE(Gaussian)){
    ListOfLikelihoods = interpolateDistributionOnData(c_Kernels_list = c_Kernels_list,
                                                      c_PDFS_List    = ListOfLikelihoods,
                                                      Data           = TrainX)
  }else{
    ListOfLikelihoods = ListOfLikelihoods
  }
  
  # Apply Bayes theorem for classification
  #Posteriors = ApplyBayesTheorem4Likelihoods(ListOfLikelihoods = ListOfLikelihoods,
  #                                           Priors = Priors)
  VABT = ApplyBayesTheorem4Likelihoods(ListOfLikelihoods = ListOfLikelihoods,
                                       Priors = Priors, TrainX = TrainX, TrainY = TrainY,
                                       Plausible = Plausible)
  
  Posteriors = VABT$Posteriors
  Model      = VABT$Model
  # Use the max rule for posteriori to determine the class
  TrainY_train  = apply(Posteriors, 1, which.max)                                  # DefineDecisionBoundaries: TakeOptimalDecision with MAP
  if(!is.null(rownames(TrainX))){
    names(TrainY_train) = rownames(TrainX)
  }
  ReturnList = list("Modelfit"    = TrainY_train,
                    "Posteriors"  = Posteriors,
                    "Priors"      = Priors,
                    "Likelihoods" = ListOfLikelihoods,
                    "TrafoInfo"   = Trafo,
                    "TrainX"      = TrainX,
                    "TrainY"      = TrainY,
                    "NaiveBayes"  = list("Plausible"     = Plausible,
                                         "Gaussian"      = Gaussian,
                                         "Model"         = Model,
                                         "c_2List_Train" = c_2List_Train,
                                         "Thetas"        = Thetas))
  return(ReturnList)
}