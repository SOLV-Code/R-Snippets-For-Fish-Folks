# Prelim, turn into function

n <- 20
exp.test <- 5

x.vec <- 1:n
y.vec <-  sample(1: 10^exp.test,n)


y.range<- c(0,max(y.vec))
y.ticks <- pretty(y.range)
y.lim <- range(y.ticks)

calc.scale <- floor(log10(max(y.ticks)) )



if(calc.scale <= 3){
                    y.label.scale <- 1
                    y.label <- "Y"
                      
                    }

if(calc.scale > 3 & calc.scale <= 5 ){
                    y.label.scale <- 3
                    y.label <- "Y (1000s)"   
                    }

if(calc.scale > 5){
                    y.label.scale <- 6
                    y.label <- "Y (Mill)"
                    
                    }


plot(x.vec,y.vec, axes=FALSE, bty = "n", pch=19, type="o", ylim=y.lim,ylab=y.label)
axis(1)
axis(2,at=y.ticks,labels= round(y.ticks / 10^y.label.scale,2),las=2)

