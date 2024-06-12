Predict_naivBayes=function(TestX, TrainingModel){
  # V = Predict_naivBayes(TestX, Priors, c_2List_Train, Thetas)
  # 
  # INPUT
  # TestX[1:n,1:d]         Numeric matrix for testdata with n observations and d features.
  # Priors[1:l]               Numeric vector with prior probability for each class.
  # c_2List_Train
  # Thetas                    List of what?
  # 
  # OUTPUT
  # ClsTest[1:n]         Numeric vector with predicted class labels
  # Posteriors[1:n,1:l]  Numeric matrix with posterior probability according to
  #                      the bayesian theorem.
  # ListOfLikelihoods    List of m numeric matrices with l columns containing 
  #                      the distribution of class i in 1:l
  # 
  if(isTRUE(TrainingModel$NaiveBayes$Gaussian)){
    if(is.null(TrainingModel$NaiveBayes$Thetas)){
      stop("Predict_naiveBayes: Parameters Thetas are missing.")
    }else{
      class_len         = length(TrainingModel$Priors)
      ListOfLikelihoods = list()
      for(m in 1:ncol(TestX)){
        Feature       = TestX[,m]
        Likelihood    = list()
        ThetaPerClass = TrainingModel$NaiveBayes$Thetas[[m]]
        for(cc in 1:class_len){
          me               = ThetaPerClass[cc,1]
          std              = ThetaPerClass[cc,2]
          Likelihood[[cc]] = dnorm(Feature,me,std)
        }
        ListOfLikelihoods[[m]] = do.call(cbind,Likelihood)
      }
    }
    PDFs = ListOfLikelihoods
  }else{
    
    Trafo = TrainingModel$TrafoInfo
    for(i in 1:dim(TestX)[2]){
      TestX[,i] = (TestX[,i] - Trafo$MinX[i])/Trafo$Denom[i]                    # Scale
      TestX[,i] = TestX[,i] - Trafo$Center[i]                                   # Center
      TestX[TestX[,i] >  1, i] = 1                                              # Cap max
      TestX[TestX[,i] < -1, i] = -1                                             # Cap min
    }
    
    c_Kernels_list    = TrainingModel$NaiveBayes$c_2List_Train$c_Kernels_list
    ListOfLikelihoods = TrainingModel$NaiveBayes$c_2List_Train$ListOfLikelihoods
    PDFs              = interpolateDistributionOnData(c_Kernels_list = c_Kernels_list,
                                                      c_PDFS_List    = ListOfLikelihoods,
                                                      Data           = TestX)
  }
  #Posteriors = ApplyBayesTheorem4Likelihoods(ListOfLikelihoods = PDFs, Priors = Priors)
  VABT = ApplyBayesTheorem4Likelihoods(ListOfLikelihoods = PDFs,
                                       Priors            = TrainingModel$Priors,
                                       TrainX            = TrainingModel$TrainX,
                                       TestX             = TestX,
                                       Model             = TrainingModel$NaiveBayes$Model,
                                       Plausible         = TrainingModel$NaiveBayes$Plausible)
  Posteriors = VABT$Posteriors
  cls_test   = apply(Posteriors, 1, which.max)                             # DefineDecisionBoundaries: TakeOptimalDecision with MAP
  if(!is.null(rownames(TestX))){
    names(cls_test)=rownames(TestX)
  }
  return(list("Prediction"  = cls_test,
              "Posteriors"  = Posteriors,
              "Likelihoods" = ListOfLikelihoods))
}
