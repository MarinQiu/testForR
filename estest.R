# #?????????в???
# Args <- str_conv(commandArgs(TRUE),"CP936")
# 
# cat("Args=",Args,"\n")



library('rjson')
library('elastic')
library('stringr')
library('leaps')
library('MASS')
library('bootstrap')
library('ggplot2')
library('rredis')


n <- 5

#????ϵ??ȡ?ö???ʽ?ع??ķ???ʽ
get.fmla <- function(ystr,xstr,n){
  xnam <- paste(xstr,1:n,sep = "^")
  ynam <- paste("I(",xnam,")",collapse = "+", sep = "")
  fmla <- paste0(ystr," ~ ",ynam)
  return(as.formula(fmla))
}

shrinkage <-function(fit, k=10){
  theta.fit <- function(x,y){lsfit(x,y)}
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef}
  
  x <- fit$model[,2:ncol(fit$model)]
  y <- fit$model[,1]
  
  results <- crossval(x, y, theta.fit, theta.predict, ngroup = k)
  r2 <- cor(y,fit$fitted.value)^2
  adjr2 <- 1-(length(y)-1)*(1-r2)/(length(y)-n-1)
  
  r2cv <- cor(y, results$cv.fit)^2
  adjr2cv <- 1-(length(y)-1)*(1-r2cv)/(length(y)-n-1)
  
  return(list(adjr2 = adjr2, adjr2cv = adjr2cv, coeffs = fit$coefficients))
  
  # cat("Original adjR2 =",adjr2,'\n')
  # cat(k, "Fold Cross-Validated adjR2 =",adjr2cv,'\n')
  # cat("Change =", adjr2-adjr2cv,'\n')
  # cat("r2cv = ",r2cv)
}

#cat("Args=",Args,"\n")

#��??ES
connect(es_host = "192.168.0.170", es_port = 9200, es_transport_schema = "http")


#DSL??ѯ????
querysentence <- '{
  "query": {
    "bool": {
      "must":[
      {
        "term": {
          "skuname_not_analyzed": "?´???"
          }
      },
       {
        "term": {
           "store_name": "??³?̳?"
          }
       }
       ]
    }
  }
}'

#querysentence <- Args

querysentence <- fromJSON(str_conv(querysentence,"CP936"), method = "R")

#??ȡ????
result <- Search(index = "retail", type = "receipt", body = querysentence, fields = c("time_sale","skuname","actual_selling_price","num_sale","store_name"), size = 100000, asdf = T, scroll = "1m")$hits$hits$fields

#ȥ??list??ʽ
result <- as.data.frame(apply(result,2,unlist), stringsAsFactors = F)

#ת????ʽ
result <- transform(result, actual_selling_price = as.numeric(actual_selling_price), num_sale = as.numeric(num_sale), time_sale = as.POSIXlt(time_sale), skuname = skuname, store_name = store_name)

#store_nameɸѡ
#result <- result[which(result$store_name=="?˼ҵ?"),]

#ʱ?䷶Χɸѡ
#result <- result[which(result$time_sale>as.POSIXlt("2015-05-15") & result$time_sale<as.POSIXlt("2015-06-15")),]

#????ÿ??????Ʒ????��
num_daily <- tapply(result$num_sale,as.character(result$time_sale),sum, simplify = T)

#????ÿ??????Ʒ?ļ۸?
price_daily <- tapply(result$actual_selling_price,as.character(result$time_sale),function(x){
  price_daily <- table(x)/length(x)
  price_daily <- as.numeric(names(price_daily))%*%as.numeric(price_daily)
  return(price_daily)
}, simplify = T)

data <- data.frame(price = as.numeric(price_daily), num = as.numeric(num_daily))


ggplot(data = data, aes(x=price,y=num))+geom_point()+stat_smooth(method = lm, formula = get.fmla("y","x",n), level = 0.95)


model <- lm(data = data, get.fmla("num","price",n))

#ѡ????��???𲽻ع鷨
bestmodel <- stepAIC(model, direction = "backward")

resultlist <- shrinkage(bestmodel)

redisConnect(host = "192.168.1.250",port = 6379)

redisMSet(resultlist)

redisGet("coeffs")
