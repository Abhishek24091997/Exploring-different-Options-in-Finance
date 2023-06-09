#SMIF
#Bullish Market


#When Premium of Long Call and Premium of Short Call are equal: C1
payoff<-function(St,K1,K2,c1,c2){
  if(St<0 |K1<0 |K2<0){
    break
  }else{
    p<-c()
    for (i in 1:St){
      if(i<K1){
        p[i]=-c1
      }
      else if (i<K2){
        p[i]=i-K1-c1
      }
      else{
        p[i]=K2-K1-c1-c2
      }
    }
  }
 return(cat(plot(p,type="l",main="Bullish Market Payoff",xlab="Market Price",ylab="Payoff"),abline(h=0)))
}
