NaiveBayesPlotter = function(Kernels, Likelihoods, FeatureNames, nrows = 1){
  
  Colors = c("blue",         "gold", "firebrick",  "limegreen",
             "dodgerblue", "orange",   "magenta",      "green",
             "cyan",          "red",      "plum",  "darkgreen",
             "turquoise",  "bisque",  "lavender", "lightgreen")
  
  #UniqueCls = unique(Cls)
  #NumClasses = length(UniqueCls)
  
  NumClasses  = dim(Kernels[[1]])[2]
  UniqueCls   = 1:NumClasses
  NumFeatures = length(Kernels)
  
  if(missing(FeatureNames)){
    FeatureNames = paste0("X", 1:NumFeatures)
  }
  
  library(plotly)
  
  ListFigs = list()
  for(j in 1:NumFeatures){
    CurrKernel = Kernels[[j]]
    CurrLikeli = Likelihoods[[j]]
    #fig = plot_ly(type = 'scatter', mode = 'lines', fill = "tozeroy")
    #for(i in 1:NumClasses){
    #  fig = add_lines(p = fig, x = CurrKernel[,i], y = CurrLikeli[,i],
    #                  name = UniqueCls[i], legendgroup = UniqueCls[i],
    #                  line = list(color = paste0("rgba(", paste0(col2rgb(Colors[i]), collapse = ", "), ", 1)")),
    #                  fillcolor = paste0("rgba(", paste0(col2rgb(Colors[i]), collapse = ", "), ", 0.2)"),
    #                  showlegend = FALSE, yaxis = "y2")
    #  fig = add_lines(p = fig, x = CurrKernel[,i], y = -CurrLikeli[,i],
    #                  line = list(color = paste0("rgba(", paste0(col2rgb(Colors[i]), collapse = ", "), ", 1)")),
    #                  fillcolor = paste0("rgba(", paste0(col2rgb(Colors[i]), collapse = ", "), ", 0.2)"),
    #                  showlegend = FALSE, yaxis = "y2")
    #}
    
    fig = plot_ly(fill = "tozeroy")
    for(i in 1:NumClasses){
      fig = add_trace(p = fig, y = CurrKernel[,i], x = CurrLikeli[,i],
                      type = "scatter", mode = "lines",
                      name = UniqueCls[i], legendgroup = UniqueCls[i],
                      line = list(color = paste0("rgba(", paste0(col2rgb(Colors[i]), collapse = ", "), ", 1)")),
                      fillcolor = paste0("rgba(", paste0(col2rgb(Colors[i]), collapse = ", "), ", 0.2)"),
                      showlegend = FALSE)
      fig = add_trace(p = fig, y = CurrKernel[,i], x = -CurrLikeli[,i],
                      type = "scatter", mode = "lines",
                      line = list(color = paste0("rgba(", paste0(col2rgb(Colors[i]), collapse = ", "), ", 1)")),
                      fillcolor = paste0("rgba(", paste0(col2rgb(Colors[i]), collapse = ", "), ", 0.2)"),
                      showlegend = FALSE)
    }
    fig = layout(p = fig, title = list(text = FeatureNames[j]))
    ListFigs[[j]] = fig
  }
  
  FinFig = subplot(ListFigs, nrows = 1)
  
  return(FinFig)
}
