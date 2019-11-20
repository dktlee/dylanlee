# R function to simulate data
pdat = c()
for (i in 1:500){
  x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  e = rnorm(10,0,4)
  y = 50 + 10*x + e
  Sxx = sum((x - mean(x))^2)
  Sxy = sum((x - mean(x))*(y - mean(y)))
  B1hat = Sxy/Sxx
  B0hat = mean(y) - B1hat*mean(x)
  ygiven5 = B0hat + 5*B1hat # for part b and d
  Sig2hat = sum((y - (B0hat + B1hat*x))^2)/(length(x)-2)
  B1se = sqrt(Sig2hat/Sxx)
  LB1 = B1hat - qt(0.975, length(x)-2) * B1se
  RB1 = B1hat + qt(0.975, length(x)-2) * B1se
  YHat.se = sqrt((1 + 1/length(x) + ((5-mean(x))^2)/Sxx)*Sig2hat)
  LY = ygiven5 - qt(0.975, length(x) - 2)*YHat.se
  RY = ygiven5 + qt(0.975, length(x) - 2)*YHat.se
  pdat = rbind(pdat, cbind(i, B1hat, B0hat, LB1, RB1, ygiven5, LY, RY))
}
pdat
hist(pdat[,2])
hist(pdat[,3])
hist(pdat[,6])

counterB1=0
for (i in 1:500){
  if (pdat[i,4] <= 10 & 10 <= pdat[i,5]) counterB1 = counterB1 + 1
}

counterYgiven5=0
for (i in 1:500){
  if (pdat[i,7] <= 100 & 100 <= pdat[i,8]) counterYgiven5 = counterYgiven5 + 1
}

##########################################################################################

x = c(22, 68, 108, 137, 255, 315, 390, 405, 685, 700, 1100)
y = c(0.75, 2.4, 3.2, 4.7, 9.3, 12, 13.4, 14.4, 24.5, 26, 38)
plot(x,y,xlab="Distance to a Galazy",ylab="Velocity",main = "Plot of Galaxies Data")
dat = cbind(x,y)
dat = as.data.frame(dat)
fit1 = lm(y~x,data=dat)
fit2 = lm(y~x+0,data=dat)

##########################################################################################

stock = read.table("/stock.txt",header=T)
plot(stock$PortfolioRate,stock$StockRate)
stock = as.data.frame(stock)
fit1 = lm(StockRate ~ PortfolioRate, data=stock)

Sxx = sum((stock$PortfolioRate - mean(stock$PortfolioRate))^2)
Sxy = sum((stock$PortfolioRate - mean(stock$PortfolioRate))*(stock$StockRate - mean(stock$StockRate)))
B1hat = Sxy/Sxx
B0hat = mean(stock$StockRate) - B1hat*mean(stock$PortfolioRate)
Sig2hat = sum((stock$StockRate - (B0hat + B1hat*stock$PortfolioRate))^2)/(length(stock$PortfolioRate)-2)
B1se = sqrt(Sig2hat/Sxx)
LB1 = B1hat - qt(0.975, length(stock$PortfolioRate)-2) * B1se
RB1 = B1hat + qt(0.975, length(stock$PortfolioRate)-2) * B1se

YHatGiven6.5 = B0hat + B1hat*6.5
YHat.se = sqrt((1 + 1/length(stock$PortfolioRate) + ((6.5-mean(stock$PortfolioRate))^2)/Sxx)*Sig2hat)
LY = YHatGiven6.5 - qt(0.975, length(stock$PortfolioRate) - 2)*YHat.se
RY = YHatGiven6.5 + qt(0.975, length(stock$PortfolioRate) - 2)*YHat.se

tval = B1hat / B1se
tvalF = (B1hat - 1) / B1se
qt = qt(0.975, length(stock$PortfolioRate) - 2)

MuHat = YHatGiven6.5
MuHatse = sqrt(Sig2hat*(1/length(stock$PortfolioRate) + (6.5 - mean(stock$PortfolioRate))^2/Sxx))
Lmu = MuHat - qt(0.975, length(stock$PortfolioRate) - 2)*MuHatse
Rmu = MuHat + qt(0.975, length(stock$PortfolioRate) - 2)*MuHatse