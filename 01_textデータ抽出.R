#mecab -d /usr/local/lib/mecab/dic/mecab-ipadic-neologd
## ||||||||||||||||||||||||||||||||||||||||| ##
library(AnalysisHelper) # devtools::install_github("hiroakif93/R-functions/AnalysisHelper", force=TRUE)
library(ggplot2)

## -- xml2 : xmlやhtml をいじる用のパッケージ
library(xml2)

## -- RMeCab :　テキストマイニング用のパッケージ
library(RMeCab)

## ||||||||||||||||||||||||||||||||||||||||| ##
## -- 生態学会の要旨を一括ダウンロード
# https://www.esj.ne.jp/meeting/abst/index.html

## -- 結果の保存先フォルダ
dir <- make.dir('01_output')

## -- ダウンロード先のファイル名を取得
fl <- list.files('abstructs', full.names=TRUE)

## ||||||||||||||||||||||||||||||||||||||||| ##
## -- 年度ごとに解析
textList <- textcomm <- NULL

for(l in 1:length(fl)){ #i=l=1
    
    ## +++++++++++++++++++++++++++++++++++++ ##
    ## -- 要旨のファイルを読み込み、不要なファイルを削除
    abst <- list.files(fl[l], full.names=TRUE)
    if(length(grep('index', abst)) ) abst <- abst[-grep('index', abst)]
    if(length(grep('schedule', abst)) ) abst <- abst[-grep('schedule', abst)]
    
    abstsub <- abst[grep('html$', abst)]
    
    ## +++++++++++++++++++++++++++++++++++++ ##
    ## -- 年度を取得
    year <- strsplit(fl[l], '/')[[1]][2]
    
    ## +++++++++++++++++++++++++++++++++++++ ##
    ## -- テキストデータの保存先
    sink(sprintf('%s/%s.txt', dir$tabledir, year)); cat(''); sink()
    
    presentation <- 0
    total <- 0
    for(i in 1:length(abstsub)){#i=1384
        
        ## -- ファイルの読み込み
        html <- xml2::read_html(abstsub[i])
        
        ## -- 要旨本文を抜き出し
        main.text<- xml2::xml_children(xml_children(html))[7]
        
        ## -- テキスト化
        if(length(xml_children(main.text))>3){
            
            title <- xml_text(xml_children(main.text)[2])
            main <- xml_children(main.text)[4]
            text <- xml_text(main)
            
            ## -- 改行コードを消す
            txt <- gsub("\n", '', text)
            txt <- gsub("\r", '', txt)
            #textList[[i]] <- txt
            
            ## -- 要旨に「群集」が入っているものをアウトプット
            if( length(grep('群集', txt)) > 0){
                
                ## -- テキストにアウトプット
                sink(sprintf('%s/%s.txt', dir$tabledir, year), append=TRUE)
                cat(txt); cat('\n'); sink()
                
                sink(sprintf('%s/tmp%s_%s.txt', dir$rdsdir, i, year))
                cat(txt); sink()
                
                ## -- リストに保存
                # freq <- RMeCabFreq(sprintf('%s/tmp_%s.txt', dir$tabledir, year))
                # freqsub <- freq[freq[,2]=='名詞' & freq[,3]!='接尾' & freq[,3]!='非自立' & freq[,3]!='数',]
                # 
                #textList <- rbind(textList, cbind(year=year, freqsub))
                
                ## -- 「群集」要旨のカウント
                presentation <- presentation + 1
               
            }
            
            total <- total + 1
            
        }
        
    }
    textcomm <- rbind(textcomm, data.frame(year=year, num.comm=presentation, total=total, pro=presentation/total))
    
}

saveRDS(textcomm, sprintf('%s/textcomm.rds', dir$rdsdir))

## ||||||||||||||||||||||||||||||||||||||||| ##
## -- 「群集」を含む発表の変遷

ggdf <- textcomm
colnames(ggdf)[-c(1)] <- c('「群集」を含む要旨数', "全ての要旨数", "「群集」を含む要旨数の割合")

lf <- tidyr::gather(ggdf, key, value, -c(1))
lf$group=1
theme_set(theme_bw(base_family = "HiraKaku"))
g <- ggplot(lf, aes(x=as.factor(year), y=value))+
    geom_line(aes(group=group))+
    geom_point()+
    #theme_bw(base_family = "HiraKakuPro-W3")+
    facet_wrap(~key, scales='free', strip.position = 'left', ncol=1)+
    labs(x='大会', y="")+
    theme(strip.placement = 'outside',
          strip.background = element_blank())

ggsave(plot=g, filename=sprintf('%s/変遷.pdf', dir$figdir),
       w=15,h=15, units='cm', device=cairo_pdf, family = "Japan1")


## ||||||||||||||||||||||||||||||||||||||||| ##