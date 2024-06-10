
## ||||||||||||||||||||||||||||||||||||||||| ##
# install.packages(c('extrafont', 'cowplot', 'ggbiplot'))
# remotes::install_github("IshidaMotohiro/RMeCab")
#install.packages("RMeCab", repos = "http://rmecab.jp/R") 

## -- RMeCab :　テキストマイニング用のパッケージ
library(RMeCab)

library(ggplot2)
library(cowplot)

library(AnalysisHelper)

library(ggbiplot)

## ||||||||||||||||||||||||||||||||||||||||| ##
dir=make.dir('05_output')

## -- 各年ごとの単語頻度
docMat_res <- readRDS('02_output/RDS/docMatrix_result.rds')

## -- 割合に変換
docmat <- docMat_res/rowSums(docMat_res)
colnames(docmat)
## -- 2年以上でてきたかつ全ての年に出ていない単語を選択
docmat <- (docmat[,colSums(docmat>0)>1])
docmat <- docmat[,-grep('，', colnames(docmat))][,-c(1:50)]


## ||||||||||||||||||||||||||||||||||||||||| ##
## -- 主成分分析
pca <- prcomp(docmat)

pdf(sprintf('%s/PCA_biplot.pdf', dir$figdir), family="Japan1GothicBBB")
biplot(pca, cex=0.5)
dev.off()

ggbiplot(pcobj = pca, choices = 1:2, obs.scale = 1, var.scale = 1) 

## -- PC1 の寄与率
head(pca$rotation[ order(pca$rotation[,1], decreasing=TRUE),1:2], n=30)

tail(pca$rotation[ order(pca$rotation[,1], decreasing=TRUE),1:2], n=30)

## -- プラス方向へのPC２寄与率
head(pca$rotation[ order(pca$rotation[,2], decreasing=TRUE),1:2], n=30)

## -- マイナス方向へのPC２寄与率
tail(pca$rotation[ order(pca$rotation[,2], decreasing=TRUE),1:2], n=30)

write.csv(pca$rotation, "Table/PCA寄与率.csv", fileEncoding = "Shift-JIS")
## ||||||||||||||||||||||||||||||||||||||||| ##
## -- 主座標分析
# docmat_bi <- docmat
# docmat_bi[docmat_bi>0] <- 1
# 
# pcoa <- cmdscale( vegan::vegdist(docmat_bi, method='jaccard', binary=TRUE))

## ||||||||||||||||||||||||||||||||||||||||| ##
df <- data.frame(year=rownames(pca$x), path=as.numeric( gsub('.txt','', rownames(pca$x)) ), pca$x)
g <- ggplot(df, aes(x=PC1, y=PC2, color=path, label=path))+
    geom_text()+
    geom_path()+
    guides(color='none')
    
g2 <- ggplot(df, aes(x=PC1, y=PC3, color=path, label=path))+
    geom_text()+
    geom_path()+
    guides(color='none')

ggsave(plot=plot_grid(g, g2), filename=sprintf('%s/PCA_ggplot.pdf', dir$figdir), 
		w=10, h=5)

## ||||||||||||||||||||||||||||||||||||||||| ##


pcobj=pca
choices = 1:2
scale = 1
pc.biplot = TRUE
obs.scale = 1 - scale
var.scale = scale
groups = NULL
ellipse = FALSE
ellipse.prob = 0.68
labels = NULL
labels.size = 3
alpha = 1
var.axes = TRUE
circle = FALSE
circle.prob = 0.69
varname.size = 3
varname.adjust = 1.5
varname.abbrev = FALSE



library(ggplot2)
library(plyr)
library(scales)
library(grid)
library(purrr)

stopifnot(length(choices) == 2)

topn = 10
if (inherits(pcobj, "prcomp")) {
  nobs.factor <- sqrt(nrow(pcobj$x) - 1)
  d <- pcobj$sdev
  u <- sweep(pcobj$x, 2, 1/(d * nobs.factor), FUN = "*")
  v <- pcobj$rotation
  top5 = \(x) c(order(x)[1:topn], order(x, decreasing = TRUE)[1:topn])
  rowid = apply(v[,1:2], 2, top5) |> as.vector() |> unique()
  v = v[rowid,]
  
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
g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + 
  xlab(u.axis.labs[1]) + 
  ylab(u.axis.labs[2]) + coord_equal()
g <- g + geom_segment(data = df.v, 
                      aes(x = 0, y = 0,
                          xend = xvar, yend = yvar), 
                      arrow = arrow(length = unit(1/2,"picas")), color = muted("red"))+ 
  geom_line(linetype=3)+
  geom_text(aes(label = labels, color = groups), size = labels.size)

(g + geom_text(data = df.v, aes(label = varname, 
                               x = xvar, y = yvar, angle = angle, hjust = hjust), 
              color = "darkred", size = varname.size) + 
    theme_bw(base_family = 'sans')
  )|>
  ggsave(filename='test_pca.pdf', family = "Japan1", w=10, h=10)

