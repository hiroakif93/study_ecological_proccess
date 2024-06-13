
## ||||||||||||||||||||||||||||||||||||||||| ##

## -- RMeCab :　テキストマイニング用のパッケージ
library(AnalysisHelper)
library(RMeCab)

library(ggbiplot)

library(Rtsne)

## ||||||||||||||||||||||||||||||||||||||||| ##
## -- 結果の保存先フォルダ
dir <- make.dir('02_output')

## -- テキストデータの取得（01_...R の結果）
fl <- list.files("01_output/Table", full.names = TRUE)

## ||||||||||||||||||||||||||||||||||||||||| ##
## -- 形態素解析　N-gram

mecab_res <- ngram <- NULL
for(l in 1:length(fl)) {   

    year <- substr(strsplit(fl[l], '/')[[1]][3], 1,2)
    
    ## -- 形態素解析
    word_freq <- RMeCabFreq(fl[l])
    mecab_res[[sprintf('%s', year)]] <- word_freq
    
    ## -- N-gram
    ngram[[sprintf('%s', year)]] <- NgramDF(fl[l], type=1, pos=c("名詞"), N=2)
    
}

saveRDS(mecab_res, sprintf('%s/mecab_result.rds', dir$rdsdir))
saveRDS(ngram, sprintf('%s/ngram_result.rds', dir$rdsdir))

## ||||||||||||||||||||||||||||||||||||||||| ##
## -- document Term
docMatrix_res <- docMatrix("01_output/Table", pos = c("サ変","名詞"), 
                    minFreq = 10)
docmat <- t(as(docMatrix_res,"matrix"))
saveRDS(docmat, sprintf('%s/docMatrix_result.rds', dir$rdsdir))

write.table(colnames(docmat), sprintf('%s/words.txt', dir$tabledir),
            quote=FALSE, row.names = FALSE)
## ||||||||||||||||||||||||||||||||||||||||| ##

