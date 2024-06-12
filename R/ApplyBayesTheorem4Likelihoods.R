ApplyBayesTheorem4Likelihoods = function(ListOfLikelihoods, Priors,
                                         TrainX = NULL, TrainY = NULL,
                                         TestX = NULL, Model = NULL,
                                         Plausible = TRUE,
                                         threshold = 0.00001){
  # V=ApplyBayesTheorem4Likelihoods(ListOfLikelihoods,Priors,threshold=0.00001)
  # 
  # INPUT
  # ListOfLikelihoods    List of m numeric matrices with l columns containing 
  #                      the distribution of class i in 1:l
  # Priors[1:l]          Numeric vector with prior probability for each class.
  # 
  # OPTIONAL
  # threshold           (Optional: Default=0.00001).
  # NormalizeWithGMM    (Optional: Default=FALSE).
  # 
  # OUTPUT
  # Posteriors[1:n, 1:l]    Numeric matrices with posterior probabilities
  #                         according to the bayesian Theorem.
  # 
  
  NumClasses  = length(Priors)
  NumFeatures = length(ListOfLikelihoods)
  LogPropMat  = matrix(NaN, nrow = nrow(ListOfLikelihoods[[1]]), ncol = NumClasses)
  # if(isFALSE(NormalizeWithGMM)){ #priors werden direkt mit eingerechnet
  #   factor=1
  # }else{
  #   factor=0#priors werden spaeter mit eingerechnet
  # }
  
  #----------------------------------------------------------------------------#
  # Naive Bayes Classification formula see "Machine Learning" by Mitchel
  #----------------------------------------------------------------------------#
  for(cc in 1:NumClasses){
    probability = 0 # log(Priors[cc])*0
    for(f in 1:NumFeatures){
      #bayes theorem ohne normierung, da die fuer MAP ein konstanter faktor
      #muss sieben zahlen pro case geben, falls sieben klassen gegeben
      distribution_CurrCls = ListOfLikelihoods[[f]][,cc]
      vec                  = distribution_CurrCls[!is.nan(distribution_CurrCls)]
      vec[vec<threshold]   = threshold
      probability          = probability + log(vec)
    }
    LogPropMat[1:length(probability),cc] = probability + log(Priors[cc])
  }
  PropMat             = exp(LogPropMat)
  NormalizationFactor = PropMat %*% Priors
  ZeroInd             = which(NormalizationFactor == 0)
  if (length(ZeroInd) > 0) {
    NormalizationFactor[ZeroInd] = 10^(-7)
  }
  Posteriors = PropMat * 0
  for(i in c(1:NumClasses)){
    Posteriors[, i] = PropMat[, i] * Priors[i]/NormalizationFactor
  }
  
  if(isFALSE(Plausible)){
    return(list("Posteriors" = Posteriors, "Model" = NULL))
  }
  
  #return(Posteriors)
  #----------------------------------------------------------------------------#
  # Plausible Bayes Classification see Ultsch Alfred and Loetsch Joerg
  # Ultsch, A., & Lötsch, J. (2022). Robust classification using posterior
  # probability threshold computation followed by Voronoi cell based class
  # assignment circumventing pitfalls of Bayesian analysis of biomedical data.
  # International Journal of Molecular Sciences, 23(22), 14081.
  #----------------------------------------------------------------------------#
  # Reasonable cases
  # Threshold with ABC-Analysis
  # Set A contains the highly probable cases to occur
  # Set B contains intermediate cases
  # Set C contains cases which are unlikely to occur (where Bayes theorem can fail - see Paper Ultsch)
  
  # TODO
  # Problem
  # Threshold is not accurate chosen if ABCAnalysis is applied over the Likelihoods
  # Paper proposed to apply ABC on equdistant points
  
  Parameter_k = 1
  # Plausible Bayes
  if(is.null(Model)){
    # Klassifiziere die Evidenz
    ReasonableThreshold = GetThreshold4UnreasonableCases(PropMat)
    LowEvIdx            = GetUnreasonableCases(PropMat, ReasonableThreshold)
    HighEvIdx           = setdiff(1:dim(TrainX)[1], LowEvIdx)
    N1                  = length(LowEvIdx)
    ClsLowEv            = c()
    if(N1 >= 1){
      N2                  = length(HighEvIdx)
      tmpMat              = rbind(TrainX[LowEvIdx, ], TrainX[HighEvIdx, ])
      DistMat             = as.matrix(parallelDist::parDist(tmpMat))
      DM                  = DistMat[1:N1, (N1+1):(N1+N2)]
      if(N1 >= 2){
        for(i in 1:N1){
          tmpVar   = order(DM[i, ])
          tmpIdx   = HighEvIdx[tmpVar[1:min(Parameter_k, length(tmpVar))]]
          #tmpTable = table(apply(Posteriors[tmpIdx, ], 1, which.max))
          #tmpTable = table(apply(Posteriors[tmpIdx, ], 1, which.max))
          #tmpCls   = as.numeric(names(tmpTable)[which.max(tmpTable)])
          #ClsLowEv = c(ClsLowEv, tmpCls)
          ClsLowEv = c(ClsLowEv, which.max(Posteriors[tmpIdx, ]))
        }
      }else{
        tmpVar   = order(DM)
        tmpIdx   = HighEvIdx[tmpVar[1:min(Parameter_k, length(tmpVar))]]
        #tmpTable = table(apply(Posteriors[tmpIdx, ], 1, which.max))
        #tmpCls   = as.numeric(names(tmpTable)[which.max(tmpTable)])
        #ClsLowEv = c(ClsLowEv, tmpCls)
        ClsLowEv = c(ClsLowEv, which.max(Posteriors[tmpIdx, ]))
      }
      #plot(TrainX, col = Colors[TrainY])
      #points(TrainX[LowEvIdx,], col = "red")
      #points(rbind(TrainX[LowEvIdx[1],], TrainX[LowEvIdx[1],]), col = "cyan")
      #points(rbind(TrainX[tmpIdx,]), col = "dodgerblue")
    }
    
    ReasonableCls           = apply(Posteriors, 1, which.max)
    PlausibleCls            = ReasonableCls
    PlausibleCls[LowEvIdx]  = ClsLowEv
    PlausibleImproves       = FALSE
    AccuracyReasonableBayes = sum(ReasonableCls == TrainY)/length(TrainY)
    AccuracyPlausibleBayes  = sum(PlausibleCls == TrainY)/length(TrainY)
    
    if(AccuracyPlausibleBayes > AccuracyReasonableBayes){
      PlausibleImproves = TRUE
    }
    
    Model = list("ReasonableThreshold" = ReasonableThreshold,
                 "HighEvIdx"           = HighEvIdx,
                 "TrainPosteriors"     = Posteriors,
                 "PlausibleImproves"   = PlausibleImproves)
  }else{
    ReasonableThreshold = Model$ReasonableThreshold
    LowEvIdx            = GetUnreasonableCases(PropMat, ReasonableThreshold)
    HighEvIdx           = Model$HighEvIdx
    tmpMat              = rbind(TestX[LowEvIdx, ], TrainX[HighEvIdx, ])
    DistMat             = as.matrix(parallelDist::parDist(tmpMat))
    N1                  = length(LowEvIdx)
    N2                  = length(HighEvIdx)
    DM                  = DistMat[1:N1, (N1+1):(N1+N2)]
    TrainPosteriors     = Model$TrainPosteriors
    ClsLowEv            = c()
    
    if(N1 >= 2){
      for(i in 1:N1){
        tmpVar   = order(DM[i, ])
        tmpIdx   = HighEvIdx[tmpVar[1:min(Parameter_k, length(tmpVar))]]
        #tmpTable = table(apply(TrainPosteriors[tmpIdx, ], 1, which.max))
        #tmpCls   = as.numeric(names(tmpTable)[which.max(tmpTable)])
        #ClsLowEv = c(ClsLowEv, tmpCls)
        ClsLowEv = c(ClsLowEv, which.max(TrainPosteriors[tmpIdx, ]))
        #plot(TrainX, col = Colors[TrainY])
        #points(TrainX[LowEvIdx,], col = "red")
        #points(rbind(TrainX[LowEvIdx[1],], TrainX[LowEvIdx[1],]), col = "cyan")
        #points(rbind(TrainX[tmpIdx,]), col = "dodgerblue")
      }
    }else if (N1 == 1){
      tmpVar   = order(DM)
      tmpIdx   = HighEvIdx[tmpVar[1:min(Parameter_k, length(tmpVar))]]
      #tmpTable = table(apply(TrainPosteriors[tmpIdx, ], 1, which.max))
      #tmpCls   = as.numeric(names(tmpTable)[which.max(tmpTable)])
      #ClsLowEv = c(ClsLowEv, tmpCls)
      ClsLowEv = c(ClsLowEv, which.max(TrainPosteriors[tmpIdx, ]))
    }
    
    ReasonableCls          = apply(Posteriors, 1, which.max)
    PlausibleCls           = ReasonableCls
    PlausibleCls[LowEvIdx] = ClsLowEv
  }
  
  #ReasonableIdx = GetUnreasonableCases(PropMat, ReasonableThreshold)
  
  # Plausible Bayes
  #if(length(ReasonableIdx) > 0){
  #  PlausibleClass = PlausibleBayes(Data, ReasonableIdx, Center, Cov, NumClasses, NumFeatures)
  #  Posteriors[ReasonableIdx,]                       = 0
  #  Posteriors[cbind(ReasonableIdx, PlausibleClass)] = 1
  #}
  
  return(list("Posteriors" = Posteriors, "Model" = Model))
}

GetThreshold4UnreasonableCases = function(TotalProb){
  ReasonableThreshold = c()
  for(f in 1:dim(TotalProb)[2]){
    tmpVal              = ABCanalysis(Data = TotalProb[, f])$BCLimit * 0.3
    ReasonableThreshold = c(ReasonableThreshold, tmpVal)
  }
  return(ReasonableThreshold)
}

GetUnreasonableCases = function(TotalProb, ReasonableThreshold){
  LocalReasonableIdx  = cbind()
  for(class in 1:dim(TotalProb)[2]){
    LocalReasonableIdx  = cbind(LocalReasonableIdx, TotalProb[, class] < ReasonableThreshold[class])
  }
  ReasonableIdx = which(apply(LocalReasonableIdx, 1, all))
  return(ReasonableIdx)
}
