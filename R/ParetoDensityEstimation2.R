ParetoDensityEstimation2 = function(DataVector, Kernel, PlotIt = FALSE){
  # 
  # 
  # 
  # 
  # 
  # 
  # Author: Quirin Stier, 2024
  
  if(missing(DataVector)){
    stop("ParetoDensityEstimation2: No DataVector given.")
  }
  if(!is.vector(DataVector) |!is.numeric(DataVector)){
    stop("ParetoDensityEstimation2: DataVector must be a numeric vector.")
  }
  if(missing(Kernel)){
    warning("ParetoDensityEstimation2: Missing kernel. Assuming parameter DataVector as kernel.")
    Kernel = sort(DataVector)
  }
  
  PR      = DataVisualizations::ParetoRadius(Data = DataVector)
  XMin    = min(DataVector)
  XMax    = max(DataVector)
  
  # PDE2 war besser, als größere Intervalle gesampled wurden
  #XRange  = abs(XMax - XMin)
  #XExts   = RelExts * XRange
  #XMin2   = XMin - XExts - abs(AbsExts)
  #XMax2   = XMax + XExts + abs(AbsExts)
  XMin2    = XMin - 1.6
  XMax2    = XMax + 1.6
  StepSize = 0.05
  
  #tmpVarX = quantile(DataVector)
  #tmpVarY = abs(tmpVarX[3]-tmpVarX[2])
  #XMin    = min(DataVector) - tmpVarY - 1
  #XMax    = max(DataVector) + tmpVarY + 1
  tmpVar  = seq(XMin2, XMax2, StepSize)
  
  tmpN          = length(DataVector)
  tmpLen        = length(tmpVar)
  DensityCount1 = PDE_Kernel(DataVector = DataVector, DomainX = tmpVar, PR = PR, NSeq = tmpLen, NData = tmpN)
  # plot(tmpVar, DensityCount1)
  DensityCount2 = approx(x = tmpVar, y = DensityCount1)
  #plot(DensityCount2$x, DensityCount2$y)
  DensityApprox = approx(x = DensityCount2$x, y = DensityCount2$y, xout = Kernel)
  Kernels       = DensityApprox$x
  RawDensity    = DensityApprox$y
  RawDensity[which(is.na(RawDensity))] = 0
  AUC           = pracma::trapz(Kernels, RawDensity)
  if(AUC <= 0){
    message("ParetoDensityEstimation: Integral yields zero.")
    AUC = 1
  }
  Density       = RawDensity/AUC
  
  if(isTRUE(PlotIt)){
    plot(Kernels, Density, type = "l", col = "blue", lwd = 4)
  }
  
  #PDEplot(Data = DataVector)
  #plot(Kernels, Density, type = "l", col = "blue", lwd = 4)
  
  # Sollten mehr Punkte gezogen werden, dann erst nach dem die Dichtefunktion
  # mit sehr wenigen Punkten smooth interpoliert wurde
  # Dadurch wird die Methode robuster gegen Ausreißer und unglatte Verläufe
  # Sampled man direkt am Anfang aus der feinen Sequenz viele Punkte,
  # dann ergibt es einen stark oszillierenden Verlauf, der durch die
  # Unglattheit der Pareto-Schaetzung resultiert
  #plot(tmpDensity4$x, tmpDensity4$y, type = "l", col = "blue", lwd = 4)
  
  #plot(tmpRes$kernels, tmpRes$paretoDensity, type = "l", col = "blue", lwd = 4)
  #lines(Kernels, Density, type = "l", col = "red", lwd = 4)
  
  return(list("Kernel"       = Kernels,
              "Density"      = Density,
              "ParetoRadius" = PR))
}
