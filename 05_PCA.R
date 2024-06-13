
## ||||||||||||||||||||||||||||||||||||||||| ##
# install.packages(c('extrafont', 'cowplot', 'openxlsx'))
# remotes::install_github("IshidaMotohiro/RMeCab")
#install.packages("RMeCab", repos = "http://rmecab.jp/R") 

## -- RMeCab :　テキストマイニング用のパッケージ
library(RMeCab)
library(AnalysisHelper)

AnalysisHelper::load.lib(ggplot2, cowplot, tidyverse, openxlsx)

source('my_function.R')
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

rm_words = c("ここ",'こと','ごと', 'これ','これら', 'さ', 'ため', 
             'とき', 'ところ','ら','の','それら','そこ', '的', 'よう')
docmat = docmat[,which(!colnames(docmat)%in%rm_words)]

## ||||||||||||||||||||||||||||||||||||||||| ##
## -- 主成分分析
pca <- prcomp(docmat)

pdf(sprintf('%s/PCA_biplot.pdf', dir$figdir), family="Japan1GothicBBB")
biplot(pca, cex=0.5)
dev.off()

## -- PC1 の寄与率
head(pca$rotation[ order(pca$rotation[,1], decreasing=TRUE),1:2], n=30)

tail(pca$rotation[ order(pca$rotation[,1], decreasing=TRUE),1:2], n=30)

## -- プラス方向へのPC２寄与率
head(pca$rotation[ order(pca$rotation[,2], decreasing=TRUE),1:2], n=30)

## -- マイナス方向へのPC２寄与率
tail(pca$rotation[ order(pca$rotation[,2], decreasing=TRUE),1:2], n=30)

pca$rotation |>
  as.data.frame()|>
  rownames_to_column('Terms')|>
  openxlsx::write.xlsx(file="Table/PCA寄与率.xlsx")
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
  
  df.u = ggobjs[[1]]
  df.v = ggobjs[[2]]
  
}else{
  ggobjs = make_ggPCAobj(pca, topn = 10)
  
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
  df.v$varname_deepl = varname_en
  
  saveRDS(ggobjs, 'Table/pca_obj.rds')
}
df.v$varname_self = c('Structure', 'Prey','Organism', '-er', 'Woods', 'Food web', 'Paddy field', 'Evolution',
                      'Light','Food/Eat', 'Sex', 'Plant', 'Grasslands', 'Diversity', 'Pollination', 'DNA', 'Flower', 'Soil',
                      'Composition', 'Species', 'Individual', 'Fungi/Bacteria', 'Living',
                      'Root', 'Number', 'Phylogeny', 'Bottom', 'Community', 'Vegetation Community', 
                      'Vegetation', 'Environment', 'Coral', 'Decomposition', 'Research', 'Investigation')
df.v$varname_gpt = c('Structure', 'Predation', 'Organism', 'Person', 'Forest', 'Food Web', 'Paddy Field', 
                     'Evolution', 'Light', 'Food', 'Nature or Characteristic', 'Plant', 'Grassland', 
                     'Diversity', 'Pollination', 'DNA', 'Flower', 'Soil', 'Composition', 'Species', 
                     'Individual', 'Fungus', 'Life or Living', 'Root', 'Number', 'Phylogeny', 'Bottom', 
                     'Community', 'Vegetation Community', 'Vegetation', 'Environment', 'Coral', 
                     'Decay or Decomposition', 'Research', 'Survey or Investigation')

a = my_biplot(df.u=df.u, df.v = df.v, label_col='varname', arrow.size = 0.3, segment.size = 0.25)
ggsave(a, filename=sprintf('%s/gg_biplot.pdf', dir$figdir), w=12, h=12, units='cm', family='Japan1')

df.v|>
  select(starts_with('varname'))|>
  dplyr::rename('DeepL'='varname_deepl', 'Self'='varname_self', 'Chat GPT'='varname_gpt')|>
openxlsx::write.xlsx(file="Table/単語_日本語_英語対応表.xlsx")

## Apendix
ggobjs = make_ggPCAobj(pca, topn = NULL)

df.u = ggobjs[[1]]
df.v = ggobjs[[2]]
a = my_biplot_all(df.u=df.u, df.v = df.v, label_col='varname', arrow.size = 0.3, segment.size = 0.25)
ggsave(a, filename=sprintf('%s/gg_biplot_all.pdf', dir$figdir), w=12, h=12, units='cm', family='Japan1')

