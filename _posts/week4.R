#chapter 5 : chartprograming
par(mfrow=c(2,1))
par(mar=c(1,1,1,1))

#pie chart
x = c(9,15,20,6)
label = c("Team1","Team2","Team3","Team4")
pie(x, labels=label, main="Business performance by department")
pie(x, init.angle=90, labels=label, main="Business performance by department")
dev.off()

pct = round(x/sum(x)*100)
label = paste(label, pct)
label = paste(label, pct, "%", sep=" ")
pie(x, init.angle = 90, labels = label, col = rainbow(length(x)), main="Business performance by department")
dev.off()

#rainbow 
par(mfrow=c(2,1), mar=c(0,1,0,1))
pie(rep(1,15), labels=seq(1,15), col=rainbow(15))
pie(rep(1,12), labels=seq(1,12), col=terrain.colors(12))
dev.off()

#3D pie
install.packages("plotrix")
library(plotrix)
pie3D(x, labels=label, explode=0.01, labelcex=0.8, main="Business performance by department")

#Barchart 
par(mfrow=c(2,2), mar=c(4,4,4,4))
height = c(9,15,20,6)
name = c("Team1", "Team2", "Team3", "Team4")
barplot(height, names.arg=name, main="Business performance by department")
barplot(height, names.arg=name, main="Business performance by department", col=rainbow(length(height)))
barplot(height, names.arg=name, main="Business performance by department", col=rainbow(length(height)), xlab="Department", ylab="Business performance")
barplot(height, names.arg=name, main="Business performance by department", col=rainbow(length(height)), xlab="Department", ylab="Business performance", ylim=c(0,25) )
dev.off()

bp = barplot(height, names.arg=name, main="Business performance by department", col=rainbow(length(height)), xlab="Department", ylab="Business performance", ylim=c(0,25))
text(x=bp, y=height, labels=round(height,0), pos=3)
text(x=bp, y=height, labels=round(height,0), pos=1)
barplot(height, names.arg=name, main="Business performance by department", col=rainbow(length(height)), xlab="Department", ylab="Business performance", horiz=TRUE, width=50)

#Groups bar chart
par(mfrow=c(3,1), mar=c(5,5,5,5))
height1 = c(4,18,5,8)
height2 = c(9,15,20,6)
height = rbind(height1,height2)
legend_lbl = c("2014","2015")
barplot(height,main="Business performance by department", names.arg=name, xlab="department", ylab="영업실적(억)", col=c("darkblue","red", legend.text=legend_lbl, ylim=c(0,35)))
barplot(height, main="Business performance by department", names.arg=name, xlab="department", ylab="영업실적(억)", col=c("darkblue","red", legend.text=legend_lbl, ylim=c(0,35)), beside=TRUE, args.legend=list(x="top"))
barplot(height, main="Business performance by department", names.arg=name, xlab="department", ylab="영업실적(억)", col=c("darkblue","red", legend.text=legend_lbl, ylim=c(0,35)), beside=TRUE)
dev.off()

#Historgram
data(quakes)
head(quakes)
mag = quakes$mag
hist(mag, main="Distribution of seismic intensity", xlab="seismic intensity", ylab="Number of occurrences")
colors = c("red","orange","green","blue","navy","violet")
hist(mag, main="Distribution of seismic intensity", xlab="seismic intensity", ylab="Number of occurrences", col=colors, breaks=seq(4,6.5, by=0.05), freq =FALSE)
hist(mag, main="Distribution of seismic intensity", xlab="seismic intensity", ylab="Number of occurrences", col=colors, breaks="Sturges", freq = FALSE)
lines(density(mag))

#Combination Historgram, density
hist(mag, main="Distribution of seismic intensity", xlab="seismic intensity", ylab="Number of occurrences", col=colors, breaks="Sturges", freq = FALSE)
lines(density(mag, bw=0.05))
rug(jitter(mag))

#boxplot
quantile(mag, c(0.2, 0.5, 0.75))
boxplot(mag, main="Distribution of seismic intensity", xlab="seismic intensity", ylab="Number of occurrences", col='red')
boxplot(quakes, notch = TRUE, col=1:5)

#random number boxplot
mat <- cbind(Uni05 = (1:100)/21, Norm = rnorm(100), T5 = rt(100, df=5), Gam2 = rgamma(100, shape=2))
boxplot(mat, main="boxplot.matrix", notch=TRUE, col=1:4)
boxplot(mat, main="boxplot.matrix")
dev.off()

#X-Y basic plot basic
label = c("A","B","C","D","E")
X = c(2,3,7,4,5)
Y = c(4,3,6,5,2)
plot(X,Y)
plot(X,Y,type='n')
text(X,Y,label=label)

dt = cbind(label, X, Y)
dt = data.frame(dt)
dt = data.frame(label,X,Y)
row.names(dt) = label

plot(dt)
dt = dt[,-1]
plot(dt, type='n')
text(dt, label=row.names(dt))
