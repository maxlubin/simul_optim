
set.seed(12345)
set.seed(1234)

n.step <- 300;
ret <- rbinom(n.step, 1, prob = 0.50);
ret

s0 <- 100;
sigma <- 1.5;
ret0 <- c(s0, rep(0, n.step-1));
for(i in 2:n.step){ ret0[i] <- ret0[i-1]*(1 + 0.30/n.step + (runif(n.step) - 1/2) * sigma/sqrt(n.step)) }
ret0

# prices <- rep(200,255)
# for (i in 2:255){prices[i] <- prices[i-1]+(runif(1)-0.5)*5}
# plot(prices, type="l")

plot(ret0 ~ 1)

ret.asset <- as.numeric(diff(log(ret0)))[!is.na(as.numeric(diff(log(ret0))))]
ret <- round(as.numeric(diff(log(ret0)))[!is.na(as.numeric(diff(log(ret0))))],4)
ret
length(ret)

# library(quantmod)

# ret <- getSymbols("SBUX", src="yahoo", from="2015-05-01", to="2021-11-18", auto.assign = FALSE)
# head(ret)
# 
# ret.asset <- as.numeric(diff(log(ret$SBUX.Close)))[!is.na(as.numeric(diff(log(ret$SBUX.Close))))]
# ret <- round(as.numeric(diff(log(ret$SBUX.Close)))[!is.na(as.numeric(diff(log(ret$SBUX.Close))))],4)
# ret

# options("getSymbols.warning4.0"=FALSE)
# options("getSymbols.yahoo.warning"=FALSE)
# # Downloading Apple price using quantmod
# 
# ret0 <- getSymbols("AAPL", from = '2017-01-01',
#            to = "2018-03-01",warnings = FALSE,
#            auto.assign = TRUE)
# 
# 
# ret0 <- getSymbols("INTC", src="yahoo", from="2015-05-01", to="2019-05-01", auto.assign = FALSE)
# head(ret0)


# ret.asset <- as.numeric(diff(log(ret0[,4])))[!is.na(as.numeric(diff(log(ret0[,4]))))]
# ret <- round(as.numeric(diff(log(ret0[,4])))[!is.na(as.numeric(diff(log(ret0[,4]))))],4)
# ret
# length(ret)

# ret <- ret[1:300]

hist(ret.asset)
sum(ret.asset>0)
sum(ret.asset<0)

par(mfrow=c(3,3))
hist(rnorm(pnorm(ret.asset)))

# s <- seq(1,10,,n.step)
# z <- rep(0,n.step);
# for(j in 1:length(s)){ z[j] <- paste(round(as.numeric(quantile( ret.asset * s[j], probs = c(0.001,0.999)))*100,3), collapse=" - ") }
# as.matrix(z)

# s <- seq(1,10,,n.step)
# m <- matrix(0, ncol=n.step, nrow = n.step);
# for(j in 1:length(s)){ m[,j] <- round(ret.asset,3) * s[j] }
# as.matrix(m)
# 
# matplot(m[,1:5], type="p", pch=19)
# legend("topright", legend=c(1:5), pch=19, col=1:5)

# ret <- round(as.vector(m[30:80, sample.int(ncol(m))[floor(ncol(m)/(7/3)):floor(ncol(m)/(7/4))]]),3) # this gives returns around E(ret)=0
# ret <- round(as.vector(m[20:80, sample.int(ncol(m))[floor(ncol(m)/(7/3)):floor(ncol(m)/(7/4))]]),3)


plot(as.numeric(ret0[,4]), type="l")
abline(h=0, col="red", lwd=2);

plot(cumsum(ret), type="l")
abline(h=0, col="red", lwd=2);

plot(ret, type = "l")
abline(h=0, col="red", lwd=2);

plot(cumsum(1*(ret[-length(ret)]*(1+ret[-1]))), type = "l")
abline(h=0, col="red", lwd=2);

# (palette(gray(seq(0,.9,len = 25)))) # gray scales; print old palette
# matplot(outer(1:100, 1:30), type = "l", lty = 1,lwd = 2, col = 1:30,
#         main = "Gray Scales Palette",
#         sub = "palette(gray(seq(0, .9, len=25)))")
# palette("default")      # reset back to the default



# define freq. based color
freq.ret <- table(round(ret,3))
df.freq <- data.frame(freq.ret)
df.freq
df.freq.df <- data.frame(ret=as.numeric(as.character(df.freq$Var1)), n.freq=as.numeric(df.freq$Freq))
df.freq.df
sum(df.freq.df$n.freq)


# hist(round(ret,3))$counts
# hist(round(ret,3))$mids
# hist(round(ret,3))$density
# hist(round(ret,3))$breaks
# hist(round(ret,3))$xname

# color assign. based on real returns

# seq.colr <- seq_along(sort(unique(df.freq.df$ret)))/nrow(df.freq.df)
# seq.colr.df <- data.frame(color=gray(seq.colr), seq.colr, uniq.ret=sort(unique(df.freq.df$ret)))
# seq.colr.df

# abs return color assing,

seq.colr <- seq_along(sort(unique(abs(df.freq.df$ret))))/length(seq_along(sort(unique(abs(df.freq.df$ret)))))
seq.colr.df <- data.frame(color=gray(seq.colr), seq.colr, uniq.ret=sort(unique(abs(df.freq.df$ret))))
seq.colr.df


org.ret <- data.frame(ret.abs=abs(ret), ret=ret, t=seq_along(ret));
head(org.ret)
head(seq.colr.df)

plot.df <- merge(seq.colr.df, org.ret, by.x = "uniq.ret", by.y="ret.abs", all.y = T)
head(plot.df)

plot.df.o <- plot.df[order(plot.df$t), ]
plot.df.o$exist.pos.previous <- rep(0,nrow(plot.df.o)); # prefill column for later filling in 
nrow(plot.df.o)
head(plot.df.o)

plot(uniq.ret ~ t, data = plot.df.o, pch=19, col=color, main="abs. return = uniq.ret")
abline(h=0, col="red", lwd=2)
plot(ret ~ t, data = plot.df.o, pch=19, col=color, main="real return = ret")
abline(h=0, col="red", lwd=2)

start.pos <- 1000; # nominal value of the possition (in euros)
all.pos <- c(1000,5000,10000,15000,20000,25000);
all.neg <- c(1000,5000,10000,15000,20000,25000)*(-1);


m.neg.ret <- mean(plot.df.o$ret[plot.df.o$ret<0])

net.ret.cdf <- plot.df.o[plot.df.o$ret<0, ];
# plot(ret ~ seq_along(ret), data=net.ret.cdf[order(net.ret.cdf$ret, decreasing=T), ], ylim=c(-0.15, 0.05))
# abline(h=0, col="red", lwd=2)

# estim negative curve

q.neg.val <- quantile(plot.df.o$ret[plot.df.o$ret<0], probs = seq(0.01,0.5,,6)); #, probs = exp(seq(log(0.1), log(0.5), by=0.15)));
q.neg.ret <- data.frame(q=rownames(data.frame(q.neg.val)), data.frame(q.neg.val, row.names = NULL)); colnames(q.neg.ret) <- c("quantile","pos.ret")
q.neg.ret$q <- with(q.neg.ret, as.numeric(rank(pos.ret)))
q.neg.ret

# plot(pos.ret ~ q, data=q.neg.ret, pch=19)
# lines(pos.ret ~ q, data=q.neg.ret, col="red",lwd=2)


# plot(rev(q.neg.ret$pos.ret)*seq(2,0.75,,6) ~ 1, type="b", ylim=c(-0.15,0.15))
# 
# lines(rev(q.neg.ret$pos.ret)*seq(2,1.15,,6) ~ 1, col="blue",lwd=2)
# lines(rev(q.neg.ret$pos.ret)*seq(3,1.35,,6) ~ 1, col="red",lwd=2)
# lines(rev(q.neg.ret$pos.ret)*seq(4,1.75,,6) ~ 1, col="green3",lwd=2)


# estim positive curve

multip <- 3;
q.pos.val <- quantile(plot.df.o$ret[plot.df.o$ret>0], probs = seq(0.2,1,,6)); #exp(seq(log(0.1), log(0.95), by=0.05)));
q.pos.ret <- data.frame(q=rownames(data.frame(q.pos.val)), data.frame(q.pos.val, row.names = NULL)*multip); colnames(q.pos.ret) <- c("quantile","pos.ret")
q.pos.ret$q <- with(q.pos.ret, as.numeric(rank(pos.ret)))
q.pos.ret[seq(1,nrow(q.pos.ret),,6), ]
q.pos.ret

# plot(pos.ret ~ q, data=q.pos.ret, pch=19, ylim=c(0,0.95))
# 
# lines(q.pos.ret$pos.ret*seq(2,1.15,,6) ~ 1, col="blue",lwd=2)
# lines(q.pos.ret$pos.ret*seq(3,1.35,,6) ~ 1, col="red",lwd=2)
# lines(q.pos.ret$pos.ret*seq(4,1.75,,6) ~ 1, col="green3",lwd=2)


fun.rebal <- function(x,r,b,p){ ifelse(x==p[1] & (r<b[1] & r>0), x,
                                ifelse(x==p[1] & (r>=b[1] & r>0), p[2], 
                                ifelse(x==p[2] & (r>=b[2] & r>0), p[3],
                                ifelse(x==p[3] & (r>=b[3] & r>0), p[4],
                                ifelse(x==p[4] & (r>=b[4] & r>0), p[5], 
                                ifelse(x==p[5] & (r>=b[5] & r>0), p[6], x ))))))
                              }

# fun.rebal(15000, q.pos.ret$pos.ret[2], q.pos.ret$pos.ret, all.pos)

fun.rebal.loss <- function(x,r,b,p){ ifelse(x>=p[6] & (r<b[1] & r<0), p[5], 
                                     ifelse(x==p[5] & (r<b[2] & r<0), p[4],
                                     ifelse(x==p[4] & (r<b[3] & r<0), p[3],
                                     ifelse(x==p[3] & (r<b[4] & r<0), p[2], 
                                     ifelse(x==p[2] & (r<b[5] & r<0), p[1], 
                                     ifelse(x==p[1] & (r<=b[5] & r<0), -1000, x))))))
                              }


# fun.rebal.loss(10000, q.neg.ret$pos.ret[1], q.neg.ret$pos.ret, all.pos)

# i <- 24;
# j <- 25;

thr.rebal <- sapply(10:20, function(i) rev(q.neg.ret$pos.ret)*seq(i*(1/35),i*(1/15),,6))
thr.rebal

matplot(thr.rebal, type="l", ylim=c(-0.30,0.05))

# plot(thr.rebal[,1] ~ 1, type="b", ylim=c(-0.20,0.1))
# lines(thr.rebal[,2] ~ 1, col="blue",lwd=2)
# lines(thr.rebal[,3] ~ 1, col="red",lwd=2)
# lines(thr.rebal[,4] ~ 1, col="green3",lwd=2)
# lines(thr.rebal[,5] ~ 1, col="blue",lwd=2)
# lines(thr.rebal[,6] ~ 1, col="red",lwd=2)
# lines(thr.rebal[,7] ~ 1, col="green3",lwd=2)
# lines(thr.rebal[,8] ~ 1, col="blue",lwd=2)
# lines(thr.rebal[,9] ~ 1, col="red",lwd=2)
# lines(thr.rebal[,10] ~ 1, col="green3",lwd=2)

thr.rebal.pos <- sapply(10:20, function(i) q.pos.ret$pos.ret*seq(i*(1/25),i*(1/35),,6))
thr.rebal.pos

matplot(thr.rebal.pos, type="l", ylim=c(0.0,0.35))

r <- as.vector(0);
acc.view.list <- list();

for(h in 1:ncol(thr.rebal.pos)){
     print(thr.rebal[ ,h])
  
for(i in 1:nrow(plot.df.o)){
     r[i] <- start.pos * (1+plot.df.o$ret[i]); # starting value of return generation (day 1 = opening position)
     acc.view.i <- with(plot.df.o[i, ], data.frame(start.pos, next.pos=NA, return.rebalance=NA, real.loss=0, market.ret=ret, net.pnl=0, portfolio.balance=r[i], trade.t=i, return.portfolio=0, exist.pos=1000, pos.change=0));
for(j in 2:nrow(plot.df.o)){
     r[j] <- acc.view.i[j-1,]$portfolio.balance * (1+plot.df.o$ret[j]); # next step after initial opening position j=2; j=28
     plot.df.o[j,]$exist.pos.previous <- acc.view.i$exist.pos[j-1]; 
     acc.view.i <- rbind(acc.view.i, data.frame(start.pos=NA, next.pos=NA, return.rebalance=NA, return.portfolio=0, pos.change=0, real.loss=0, net.pnl=0, with(plot.df.o[j, ], data.frame(exist.pos=exist.pos.previous, market.ret=ret, portfolio.balance=r[j], trade.t=j))));
     
    
     acc.view.i[j,]$return.rebalance <-  acc.view.i[j,]$portfolio.balance/acc.view.i[j,]$exist.pos-1 
     
     acc.view.i[j,]$next.pos <- with(acc.view.i[j,], ifelse(return.rebalance<0, fun.rebal.loss(exist.pos, return.rebalance, thr.rebal[ ,1], all.pos),
                                                     ifelse(return.rebalance>0, fun.rebal(exist.pos, return.rebalance, thr.rebal.pos[ ,h], all.pos), NA)));
                            
     # with(acc.view.i[j,], fun.rebal.loss(exist.pos, return.rebalance, rev(q.neg.ret$pos.ret)*seq(2,0.75,,6), all.pos))
 
     if(acc.view.i[j,]$next.pos<0) {  acc.view.i[j,] <- with(acc.view.i[j,], data.frame(start.pos=1000, next.pos=1000, return.rebalance=0, real.loss=(acc.view.i[j-1,]$portfolio.balance - acc.view.i[j,]$exist.pos), 
                                                             market.ret=market.ret, net.pnl=0, portfolio.balance=all.pos[1]*(1+plot.df.o$ret[j]), trade.t=0, return.portfolio=0, exist.pos=1000, pos.change=-1)) }
                                      
     acc.view.i[j,]$pos.change <-  ifelse((acc.view.i[j,]$next.pos/acc.view.i[j,]$exist.pos)>1,1,ifelse((acc.view.i[j,]$next.pos/acc.view.i[j,]$exist.pos)<1,-1, 0));
     acc.view.i[j,]$exist.pos <-  ifelse((acc.view.i[j,]$next.pos/acc.view.i[j,]$exist.pos)>1,acc.view.i[j,]$next.pos,ifelse((acc.view.i[j,]$next.pos/acc.view.i[j,]$exist.pos)<1,acc.view.i[j,]$next.pos, acc.view.i[j-1,]$exist.pos));
     
     acc.view.i[j,]$return.portfolio <-  acc.view.i[j,]$portfolio.balance/acc.view.i[j,]$next.pos-1 
     
     acc.view.i[j,]$portfolio.balance <-  ifelse((acc.view.i[j,]$pos.change>0),(acc.view.i[j,]$next.pos - acc.view.i[j-1,]$exist.pos)+acc.view.i[j,]$portfolio.balance,
                                          ifelse((acc.view.i[j,]$pos.change<0),(acc.view.i[j-1,]$portfolio.balance - acc.view.i[j-1,]$exist.pos)+acc.view.i[j,]$next.pos, acc.view.i[j,]$portfolio.balance));
     
     # j <- 5
     acc.view.i[j,]$net.pnl <- ((acc.view.i[j, ]$portfolio.balance - acc.view.i[j, ]$exist.pos) + sum(acc.view.i$real.loss));
     
     print(tail(acc.view.i[j,],j))

     par(mfrow=c(2,2))
     plot(portfolio.balance ~ 1, data = acc.view.i)
     abline(v=acc.view.i$trade.t[acc.view.i$pos.change==1], col="green3", lwd=2)
     abline(v=acc.view.i$trade.t[acc.view.i$pos.change==-1], col="red", lwd=2)
     
     plot(net.pnl ~ 1, data = acc.view.i) # net pnl 
     abline(h=0, col="red", lwd=2)
     plot(return.rebalance ~ 1, data = acc.view.i) # return 
     abline(h=0, col="red", lwd=2)
     plot(cumsum(market.ret) ~ 1, data = acc.view.i) # return 
     abline(h=0, col="red", lwd=2)
     
     # Sys.sleep(0.01)
  
     if(j>nrow(plot.df.o)-1) { break }
  }
     if(i==1) { break }
  }

  acc.view.list[[h]] <- acc.view.i;
}


head(acc.view.list[[1]])
length(acc.view.list)

par(mfrow=c(4,6));

for(r in 1:length(acc.view.list)){

  acc.view.i <- acc.view.list[[r]];
  
  plot(cumsum(market.ret) ~ 1, data = acc.view.i);
  
  plot(portfolio.balance ~ 1, data = acc.view.i);
  abline(v=acc.view.i$trade.t[acc.view.i$pos.change==1], col="green3", lwd=2);
  abline(v=acc.view.i$trade.t[acc.view.i$pos.change==-1], col="red", lwd=2);
  
  plot(net.pnl ~ 1, data = acc.view.i); # net pnl 
  abline(h=0, col="red", lwd=2);
  
 }


# trade run duration before re-balance

# acc.view.i$trade.duration <- rep(0, nrow(acc.view.i))
# i1 <- acc.view.i$trade.t == 0
# acc.view.i$trade.duration[i1] <- with(rle(acc.view.i$trade.t), sequence(acc.view.i$trade.t))
# 
# 
# sequence(rle(acc.view.i$trade.t)$lengths) * acc.view.i$trade.t
# 
# rle_len <- rle(acc.view.i$trade.t);
# rle_len$lengths[rle_len$values==0]

###################################### total return after the end period ####################################

nrow.obj <- nrow(acc.view.list[[1]])
x <- acc.view.list[[1]]
(x[nrow.obj, ]$portfolio.balance - x[nrow.obj, ]$exist.pos) + sum(x$real.loss) == x[nrow.obj, ]$net.pnl
(x[nrow.obj, ]$portfolio.balance - x[nrow.obj, ]$exist.pos) + sum(x$real.loss)
x[nrow.obj, ]$net.pnl


nrow.obj <- nrow(acc.view.list[[1]])
cbind(do.call(rbind, lapply(acc.view.list, function(x) { (x[nrow.obj, ]$portfolio.balance - x[nrow.obj, ]$exist.pos) + sum(x$real.loss) })),
      t(thr.rebal.pos))


cbind(do.call(rbind, lapply(acc.view.list, function(x) { x[nrow.obj, ]$net.pnl })),
      t(thr.rebal.pos))

# [,1]      [,2]       [,3]       [,4]      [,5]      [,6]      [,7]
# [1,] -1085.0523 0.0358560 0.06852837 0.09035561 0.1051397 0.1196763 0.1265143
# [2,] -1085.0523 0.0394416 0.07538120 0.09939117 0.1156537 0.1316440 0.1391657
# [3,] -1085.0523 0.0430272 0.08223404 0.10842673 0.1261677 0.1436116 0.1518171
# [4,] -1085.0523 0.0466128 0.08908688 0.11746229 0.1366817 0.1555792 0.1644686
# [5,] -1368.8944 0.0501984 0.09593971 0.12649786 0.1471956 0.1675469 0.1771200
# [6,]  1471.6651 0.0537840 0.10279255 0.13553342 0.1577096 0.1795145 0.1897714
# [7,]  1056.6644 0.0573696 0.10964539 0.14456898 0.1682236 0.1914821 0.2024229
# [8,]  1056.6644 0.0609552 0.11649822 0.15360454 0.1787376 0.2034498 0.2150743
# [9,]  1056.6644 0.0645408 0.12335106 0.16264010 0.1892515 0.2154174 0.2277257
# [10,]   664.5361 0.0681264 0.13020389 0.17167566 0.1997655 0.2273851 0.2403771
# [11,] -1393.2473 0.0717120 0.13705673 0.18071122 0.2102795 0.2393527 0.2530286

###################################### interim/marginal, non-cumulative return ############################## 

acc.view.i1 <- with(acc.view.i, transform(acc.view.i, interm.loss=(portfolio.balance-exist.pos)))
head(acc.view.i1,10)

df.loss <- data.frame(xtabs(acc.view.i1$interm.loss ~ acc.view.i1$trade.t))
colnames(df.loss) <- c("t","pnl")
df.loss

plot(pnl ~ 1, data = df.loss, col=c("red","green3")[ifelse(df.loss$pnl<0,1,2)], pch=19)

###################################### realized losses ######################################################

table(acc.view.i$real.loss)
hist(acc.view.i$real.loss)


head(acc.view.i)

xtabs(~ acc.view.i$portfolio.balance + as.factor(acc.view.i$pos.change))
table(acc.view.i$pos.change)

with(acc.view.i, fun.rebal(current.pos, pos.return, q.pos.ret$pos.ret, all.pos))>1000;

acc.view.i$cleared <- with(acc.view.i, fun.rebal.loss(current.pos, pos.return, q.neg.ret$pos.ret, all.pos)) #>1000;
acc.view.i
sample(ret, replace = T)>0

vol.step <- 
  
bal_trade <-
  
