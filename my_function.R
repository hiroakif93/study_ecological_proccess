
make_ggPCAobj = function(pcobj,
                        topn = NULL,
                     choices = 1:2,
                     scale = 1,
                     pc.biplot = TRUE,
                     obs.scale = 1 - scale,
                     var.scale = scale,
                     ellipse = FALSE,
                     ellipse.prob = 0.68,
                     labels = NULL,
                     alpha = 1,
                     var.axes = TRUE,
                     circle = FALSE,
                     circle.prob = 0.69,
                     varname.adjust = 1.5,
                     varname.abbrev = FALSE){
  
  library(ggplot2)
  library(plyr)
  library(scales)
  library(grid)
  library(purrr)
  library(ggrepel)
  
  stopifnot(length(choices) == 2)
  
  if (inherits(pcobj, "prcomp")) {
    nobs.factor <- sqrt(nrow(pcobj$x) - 1)
    d <- pcobj$sdev
    u <- sweep(pcobj$x, 2, 1/(d * nobs.factor), FUN = "*")
    v <- pcobj$rotation
    
    if(!is.null(topn)){
      top5 = \(x) c(order(x)[1:topn], order(x, decreasing = TRUE)[1:topn])
      rowid = apply(v[,1:2], 2, top5) |> as.vector() |> unique()
      v = v[rowid,]
    }
    
  }
  
  choices <- pmin(choices, ncol(u))
  df.u <- as.data.frame(sweep(u[, choices], 2, d[choices]^obs.scale, 
                              FUN = "*"))
  v <- sweep(v, 2, d^var.scale, FUN = "*")
  df.v <- as.data.frame(v[, choices])
  
  names(df.u) <- c("xvar", "yvar")
  names(df.v) <- names(df.u)
  if (pc.biplot) {
    df.u <- df.u * nobs.factor
  }
  r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)
  v.scale <- rowSums(v^2)
  df.v <- r * df.v/sqrt(max(v.scale))
  if (obs.scale == 0) {
    u.axis.labs <- paste("standardized PC", choices, sep = "")
  }else {
    u.axis.labs <- paste("PC", choices, sep = "")
  }
  u.axis.labs <- paste(u.axis.labs, sprintf("(%0.1f%% explained var.)", 
                                            100 * pcobj$sdev[choices]^2/sum(pcobj$sdev^2)))
  df.u$labels <- gsub(".txt", " th", rownames(df.u))
  
  if (varname.abbrev) {
    df.v$varname <- abbreviate(rownames(v))
  }else {
    df.v$varname <- rownames(v)
  }
  df.v$angle <- with(df.v, (180/pi) * atan(yvar/xvar))
  df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar))/2)

  return(list(df.u, df.v))
  
}


my_biplot = function(df.u, df.v, label_col="var_name" ,arrow.size=1,
                     text.color='darkred',
                     segment.size=0.1,
                     segment.linetype=3, varname.size = 2,
                     labels.size = 3,
                     groups = NULL){
  library(ggplot2)
  library(plyr)
  library(scales)
  library(grid)
  library(purrr)
  library(ggrepel)
  g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + 
    xlab('PC1') + 
    ylab('PC2') 
  g <- g + geom_segment(data = df.v, 
                        aes(x = 0, y = 0, 
                            xend = xvar, yend = yvar), 
                        size = arrow.size,
                        arrow = arrow(length = unit(1/2,"picas")), color = muted("red"))+ 
    geom_line(linetype=3)+
    geom_text(aes(label = labels, color = groups), size = labels.size)
  
  g = (g + 
         geom_text_repel(data = df.v, aes(label = !!ensym(label_col), 
                                          x = xvar+0.01, y = yvar+0.01, angle = angle, hjust = hjust), 
                         color = text.color, size = varname.size, 
                         segment.size=segment.size, segment.linetype=segment.linetype,
                         segment.color='black',
                         bg.color = "white",
                         bg.r = 0.1) +
         theme_bw(base_family = 'sans')
  )
  return(g)
}

my_biplot_all = function(df.u, df.v, label_col="var_name" ,arrow.size=1,
                         text.color='darkred',
                         segment.size=0.1,
                         segment.linetype=3, varname.size = 2,
                         labels.size = 3,
                         groups = NULL){
  library(ggplot2)
  library(plyr)
  library(scales)
  library(grid)
  library(purrr)
  library(ggrepel)
  g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + 
    xlab('PC1') + 
    ylab('PC2') 
  g <- g + geom_segment(data = df.v, 
                        aes(x = 0, y = 0, 
                            xend = xvar, yend = yvar), 
                        size = arrow.size,
                        arrow = arrow(length = unit(1/2,"picas")), color = muted("red"))+ 
    geom_line(linetype=3)+
    geom_text(aes(label = labels, color = groups), size = labels.size)
  
  g = (g + 
         geom_text(data = df.v, aes(label = !!ensym(label_col), 
                                          x = xvar+0.01, y = yvar+0.01, angle = angle, hjust = hjust), 
                         color = text.color, size = varname.size) +
         theme_bw(base_family = 'sans')
  )
  return(g)
}
