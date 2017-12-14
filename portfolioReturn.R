library("quantmod")
library("financial")
options("getSymbols.warning4.0"=FALSE)

data_stock <- function(symbol,src='yahoo',from,to)
{
  getSymbols(symbol,src=src,from=from,to=to)
  datas <- get(symbol)
  # datas <- na.fill(datas, 0)
  return (datas)
}

data_stock_adj <- function(symbol,data)
{  
  return (adjustOHLC(data,use.Adjusted=TRUE,symbol.name=symbol))
}

irr <- function(cashflow)
{
  cf <- cf(cashflow)
  temp <- as.numeric(unlist(cf['irr']))
  irr <- temp[temp>-100 & temp < 100]
  return (irr)
}

annual_returns <- function(symbol,adj=TRUE,from,to)
{
  datas <- data_stock(symbol,from=from,to=to)
  datas_adj <- data_stock_adj(symbol,datas)
  
  if(adj)
    return( 100*periodReturn(datas_adj,period='yearly',subset='1970::') )
  else
    return( 100*periodReturn(datas,period='yearly',subset='1970::') )
}

portfolio_annual_returns <- function(symbol,ratio,from='1970-01-01',to= Sys.Date())
{
  ar <- annual_returns(symbol[1],from=from,to=to) * ratio[1]
  for(i in 2:length(symbol))
  {
    ar <- merge(ar,annual_returns(symbol[i],from=from,to=to) * ratio[i])
  }
  names(ar)<-symbol
  ar <- na.fill(ar, 0)
  years <-format(index(ar),"%Y")
  ar <- cbind(years, ar)
  
  years <- unique(years)
  result <- as.data.frame(matrix(0, ncol=ncol(ar)-1, nrow=length(years)))
  colnames(result) <- colnames(ar)[2:ncol(ar)]
  rownames(result) <- years
  for(y in years)
  {
    result[y,] <- colSums(ar[ar[,1]==y],)[2:ncol(ar)]
  }
  
  return (result)
}

plot_portfolio <- function(PAR,combine=T,symbol,ratio)
{
  if(combine)
  {
    barx <- barplot(t(as.matrix(PAR)),
                    names.arg=rownames(PAR),xlab="year",ylab="Portfolio Return(%)",
                    ylim=c(min(rowSums(PAR),na.rm=TRUE)-10,max(rowSums(PAR),na.rm=TRUE)+40),
                    col=rainbow(length(names(PAR))),beside=F,axes=TRUE)
                    # axis(1, at = format(index(PAR),"%Y"), las = 2)
                    # step = (max(PAR,na.rm=TRUE)-min(PAR,na.rm=TRUE))/10
                    # axis(2, at = floor(seq(min(PAR,na.rm=TRUE),max(PAR,na.rm=TRUE),step)), las = 1)
    legend("topleft", paste(symbol,paste(ratio*100,"%",sep=""),sep=" - "), cex=0.55, 
           bty="n", fill=rainbow(length(names(PAR))));
    
    sum <- as.xts(rowSums(PAR,na.rm=T),order.by=index(PAR))
    points(barx,sum)
    text(barx[sum>=0],sum[sum>=0],labels=paste(format(sum[sum>=0],digits=2,nsmall=2),"%",sep=""), cex=0.7, pos=3, col="red")
    text(barx[sum<0],sum[sum<0],labels=paste(format(sum[sum<0],digits=2,nsmall=2),"%",sep=""), cex=0.7, pos=1, col="green") 
  }
  else
  {    
    barx <- barplot(t(as.matrix(PAR)),
                    names.arg=rownames(PAR),xlab="year",ylab="Portfolio Return(%)",
                    border = T,
                    col=rainbow(length(names(PAR))),beside=T,axes=TRUE)
    legend("topleft", paste(symbol,paste(ratio*100,"%",sep=""),sep=" - "), cex=0.55, 
           bty="n", fill=rainbow(length(names(PAR))));
    
    if (all(ratio != rep(1,length(symbol))))
    {
      sum <- rowSums(PAR)
      plot(names(sum),sum,type="b",xlab="year",ylab="Portfolio Return(%)",ylim=c(min(sum)-10,max(sum)+10))
      text(names(sum)[sum>=0],sum[sum>=0],labels=paste(format(sum[sum>=0],digits=2,nsmall=2),"%",sep=""), cex=0.7, pos=3, col="red")
      text(names(sum)[sum<0],sum[sum<0],labels=paste(format(sum[sum<0],digits=2,nsmall=2),"%",sep=""), cex=0.7, pos=1, col="green") 
    }
  }
}

symbol = c("VTI","VBR","VPL","VGK","VWO","BND","BWX","VNQ")
#ratio =  c(0.15,0.0375,0.1875,0.1875,0.1875,0.06,0.14,0.05)
ratio =  rep(1,length(symbol))
PAR = portfolio_annual_returns(symbol,ratio,from='2005-01-01')
plot_portfolio(PAR,combine=F,symbol,ratio)

#                                           中華電    中鋼      台積電    鴻海      台塑      群創
symbol_TW = c("0050.TW","0051.TW","0056.TW","2412.TW","2002.TW","2330.TW","2317.TW","6505.TW","3481.TW")
ratio_TW =  rep(1,length(symbol_TW))
PAR_TW = portfolio_annual_returns(symbol_TW,ratio_TW,from='2005-01-01')
plot_portfolio(PAR_TW,combine=F,symbol_TW,ratio_TW)

#barChart(datas_adj)
portfolioR <- rowSums(PAR)
# plot(names(portfolioR),portfolioR,col="blue",type='b',xlab="year",ylab="Portfolio Return(%)",ylim=c(min(portfolioR)-10,max(portfolioR)+10))
# text(names(portfolioR),portfolioR[portfolioR>=0],labels=paste(format(portfolioR[portfolioR>=0],digits=2,nsmall=2),"%",sep=""), cex=0.8, pos=3, col="red") 
# text(names(portfolioR),portfolioR[portfolioR<0],labels=paste(format(portfolioR[portfolioR<0],digits=2,nsmall=2),"%",sep=""), cex=0.8, pos=1, col="green") 
print(portfolioR)


