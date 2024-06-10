
## ||||||||||||||||||||||||||||||||||||||||| ##
library(AnalysisHelper) #devtools::install_github("hiroakif93/R-functions/AnalysisHelper", force=TRUE)
library(ggplot2)

## -- Network
library(igraph)
library(ggnetwork)
library(graphlayouts)

## ||||||||||||||||||||||||||||||||||||||||| ##
## -- 結果の保存先フォルダ
dir <- make.dir('04_output')

## -- テキストデータの取得（01_...R の結果）
ngram_list <- readRDS("02_output/RDS/ngram_result.rds")

## ||||||||||||||||||||||||||||||||||||||||| ##

for(l in 1:length(word_freq_list)) {   

    ## ||||||||||||||||||||||||||||||||||||||||| ##
    ## -- ファイルの読み込み    
    year <- names(ngram_list)[l]
    
    ngram <- ngram_list[[l]]
    ngramsub <- ngram[ngram[,3]>5 & ngram[,1] !='的' &ngram[,2] !='的',]
    
    ## ||||||||||||||||||||||||||||||||||||||||| ##
    ## -- グラフオブジェクトの作成
    graph <- simplify(graph.data.frame(ngramsub, directed = FALSE))
    mem <- components(graph)
    mem2 <- mem$membership[mem$membership==which.max(mem$csize)]
    graphsub <- induced_subgraph(graph, names(mem2))
    
    mod <- infomap.community(graphsub)
    V(graphsub)$member <- mod$membership

    ## ||||||||||||||||||||||||||||||||||||||||| ##
    ## -- 描画
    #lay <- layout_as_backbone(graphsub)$xy
    #lay <- as.matrix(layout_igraph_centrality(graphsub, degree(graphsub))[,1:2])
    lay <- layout_nicely(graphsub)
    
    gnet2 <- ggnetwork(graphsub, layout=as.matrix(lay))
    g <- ggplot(gnet2, aes(x=x, y=y, xend=xend, yend=yend))+
        geom_edges(color='skyblue')+
        geom_text(aes(label=name), family = "Meiryo UI",
                  size=3)+
        theme_void()+
        scale_color_manual(values=palettes(x=length(table(V(graphsub)$member)),
                                           pal = 'rainbow'))
    
    ggsave(sprintf('%s/graphsub_%s.pdf', dir$figdir, year), w=18, h=18, units = 'cm',
           plot=g, device=cairo_pdf)
    ## ||||||||||||||||||||||||||||||||||||||||| ##
    
    #dev.off()
}

## ||||||||||||||||||||||||||||||||||||||||| ##