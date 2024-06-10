
## ||||||||||||||||||||||||||||||||||||||||| ##
## -- 最初に設定する箇所

## -- 使う品詞の選択
wordclasstmp <- c("一般", 
                  "固有名詞", 
                  "サ変接続"#,
                  #"副詞可能",
                  #'形容動詞語幹' 
                  )
wordclass_set <- sapply(1:length(wordclasstmp), function(x) combn(wordclasstmp, x))

## --　品詞の組み合わせ
wordclass_set <- wordclass_set[c(1,2,3)]

## -- 最小の頻度
min.freq <- 5

## -- 排除したい言葉. なければNAを指定
ex.word <- c('研究', '解析', '調査', '生物')

## ||||||||||||||||||||||||||||||||||||||||| ##

library(AnalysisHelper) #devtools::install_github("hiroakif93/R-functions/AnalysisHelper", force=TRUE)
library(ggplot2)

## -- wordcloud2 :　word cloud
library(wordcloud2)
library(webshot)
library(htmlwidgets)

## ||||||||||||||||||||||||||||||||||||||||| ##
## -- 結果の保存先フォルダ
dir <- make.dir('03_output')

## -- テキストデータの取得（02_...R の結果）
word_freq_list <- readRDS("02_output/RDS/mecab_result.rds")
    
## ||||||||||||||||||||||||||||||||||||||||| ##
## -- Word cloud

itr <- expand.grid(1:length(fl), 1:length(wordclass_set))

for(l in 1:length(word_freq_list)) {   

    year <- names(word_freq_list)[l]
    
    ## -- 形態素解析
    word_freq <- word_freq_list[[l]]
    
    ## +++++++++++++++++++++++++++++++++++++++ ##
    ## -- 品詞の組み合わせごとにプロット
    for(m in 1:length(wordclass_set)){
        wordclass <- wordclass_set[[m]]
        
        for(n in 1:ncol(wordclass)){
            
            ## +++++++++++++++++++++++++++++++++++ ##
            ## -- 特定の品詞を抜き出し
            
            ## -- 名詞のみ
            word_freq_sub1 <- word_freq[word_freq[,2]=='名詞',]
            
            ## -- 頻度が１より大きいもの
            word_freq_sub2 <- word_freq_sub1[word_freq_sub1[,4]>min.freq,]
            
            ## -- 名詞の中の特定の品詞
            word_freq_sub <- word_freq_sub2[word_freq_sub2[,3]%in%wordclass[,n],]
            
            ## +++++++++++++++++++++++++++++++++++ ##
            ## -- 英単語は品詞分解ができないので取り除く＝英語要旨は使わない
            eng <- c( grep("[A-Z]", word_freq_sub[,1]), 
                      grep("[a-z]", word_freq_sub[,1]),
                      grep("[1-9]", word_freq_sub[,1]))
            word_seleted <- word_freq_sub
            
            if( length(eng)>0 ){ word_seleted <- word_seleted[-eng,]}
            
            ## -- 不要な言葉の排除
            if( sum(word_seleted[,1]%in%ex.word )>0 ){ 
                word_seleted <- word_seleted[-which(word_seleted[,1]%in%ex.word ),]}
            
            ## -- 図の保存
            word <- word_seleted[, c(1,4)]
    
            plot <- wordcloud2(word_seleted[, c(1,4)], size=1.5, minSize=min.freq)
            saveWidget(plot,"tmp.html",selfcontained = F)
            webshot("tmp.html",sprintf('%s/wordclouds_%s_%s.png', 
                                       dir$figdir, 
                                       paste( sapply(wordclass[,n], substr, start=1, stop=2), collapse='-'), 
                                       year ), 
                    delay = 5, vwidth = 800, vheight=800)
            ## +++++++++++++++++++++++++++++++++++ ##
            
        }
        
    }
}

## ||||||||||||||||||||||||||||||||||||||||| ##
## -- Barplot
# 
# bardf <- c()
# for(l in 1:length(word_freq_list)){
#     
#     year <-names(wird_freq_list)[l]
#     
#     ## -- 形態素解析
#     word_freq <- word_freq_list[[l]]
#     
#     ## +++++++++++++++++++++++++++++++++++++++ ##
#     ## -- 特定の品詞を抜き出し
#     
#     ## -- 名詞のみ
#     word_freq_sub1 <- word_freq[word_freq[,2]=='名詞',]
#     
#     ## -- 頻度が１より大きいもの
#     word_freq_sub2 <- word_freq_sub1[word_freq_sub1[,4]>min.freq,]
#     
#     ## -- 名詞の中の特定の品詞
#     word_freq_sub <- word_freq_sub2[word_freq_sub2[,3]%in% wordclasstmp,]
#     
#     ## +++++++++++++++++++++++++++++++++++ ##
#     ## -- 英単語は品詞分解ができないので取り除く＝英語要旨は使わない
#     eng <- c( grep("[A-Z]", word_freq_sub[,1]), 
#               grep("[a-z]", word_freq_sub[,1]),
#               grep("[1-9]", word_freq_sub[,1]))
#               
#     word_seleted <- word_freq_sub[order(word_freq_sub[,4], decreasing=TRUE)[1:100],]
#     
#     ## -- 不要な言葉の排除
#     if( sum(word_seleted[,1]%in%ex.word )>0 ){ 
#         word_seleted <- word_seleted[-which(word_seleted[,1]%in%ex.word ),]}
#     
#     bardf[[l]] <- cbind(year=year, ranking=1:nrow(word_seleted), word_seleted) 
#     ## +++++++++++++++++++++++++++++++++++++++ ##
#     
# }
# 
# terms <- unique(unlist(sapply(bardf, '[', 3)))
# df <- data.frame(terms=terms, row.names=terms)
# 
# for(l in 1:length(bardf)){
#     
#     dftmp <- bardf[[l]]
#     df[dftmp[,3],unique(dftmp[,1])[1]] <- dftmp[,6]
#     
# }
# 
# df[is.na(df)] <- 0
# write.csv(df, sprintf('%s/bar_chart_race.csv', dir$tabledir), row.names=FALSE)
# 
# ## +++++++++++++++++++++++++++++++++++++++++ ##
# 
# ggdf <- do.call(rbind, bardf)
# ggplot(ggdf, aes(y=Term, x=year, fill=log(Freq)))+
#     geom_tile()+
#     theme_bw(base_family = "HiraKakuPro-W3")+
#     scale_fill_viridis_c()
# 
# 
# ## +++++++++++++++++++++++++++++++++++++++++ ##
# 
# 
# ## ||||||||||||||||||||||||||||||||||||||||| ##
