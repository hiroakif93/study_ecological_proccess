
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

if(file.exists('Table/pca_obj.rds')){
  ggobjs=readRDS('Table/pca_obj.rds')
}else{
  ggobjs = make_ggPCAobj(pca)
  saveRDS(ggobjs, 'Table/pca_obj.rds')
}

df.u = ggobjs[[1]]
df.v = ggobjs[[2]]

varname_en = deeplr::toEnglish2(
  df.v$varname,
  source_lang = NULL,
  split_sentences = TRUE,
  preserve_formatting = FALSE,
  get_detect = FALSE,
  auth_key = "36cb3262-4a66-4587-8f7a-89136fd627e9:fx"
)
df.v$varname_en = varname_en
df.v$varname_self = c('Like', 'Structure', 'Organism', 'Prey', 'Woods', '-er', 'Paddy\nfield', 'Food\nweb',
                      'Evolution', 'Light', 'Sex', 'Plant', 'Pollinator', 'Diversity', 'DNA', 'Flower', 'Soil',
                      'Composition', 'Research', 'Population', 'Fungi/Bacteria', 'Living-', 'Root', 'Number', '', '-voir')



