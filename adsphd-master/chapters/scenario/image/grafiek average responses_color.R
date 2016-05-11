# data worden ingelezen
data=read.csv(file=file.choose(), sep = ";", header=TRUE)

# alle punten worden tesamen geplot 
plot(data$Y, data$WPET, type= "p",pch=1, col = "darkmagenta",main = "scatterplot all responses",xlab="Y response (%)", ylab = "WPET response (%)")
abline(a=0,b=1) #this draws the 1:1line
abline(h=0) #this draws the horizontal line of the cross
abline(v=0) #this draws the vertical line of the cross

# grafiek met negen vakjes
library(lattice)
library(latticeExtra)

#settings aanpassen
myTheme <- trellis.par.get()
myTheme$superpose.symbol$col <-rep(c("gray74", "skyblue","darkslateblue","darkgoldenrod4","orchid3","yellow1","darkorange","limegreen","darkgreen")) 
myTheme$superpose.symbol$pch <-c(19)
myTheme$superpose.symbol$cex <-c (0.9)
trellis.par.set(myTheme)
show.settings(myTheme)


## create factors with value labels
    Climate.f<-factor(data$Climate)
    Fert.f<-factor(data$Fert)
    Year.f<-factor(data$Year)
    Soil.f<-factor(data$Soil)

###################GRAFIEK MET LEGENDE ##############################
    
## create the graph object
grafiekfunctie <- xyplot(WPET~Y|Year.f*Climate.f,data=data,
                        main= "scatterplots by climate and season type", ylab= expression(bold("WP"[ET]*" response (%)")), xlab= "Y response (%)",
                        group=data$FM, auto.key = list(title= "Field management",cex.title=1, columns=1, between = 5, space="right", cex=1),
                        abline=list(h=0, v=0,c(a=0,b=1), lty="dotted", col="grey"),
                        scales=list(x=list(relation="free",tick.number=3,axs="i"), y=list(relation="same",tick.number=2, alternating=1,tck=c(1,0))),
                        xlim=list(c(-50,200),c(-50,50),c(-50,50),c(-50,200),c(-50,50),c(-50,50),c(-50,200),c(-50,50), c(-50,50)),
                        layout=c(3,3), aspect=1,between=list(y=1,x=1),index.cond=list(3:1,3:1),
                        par.strip=list(cex=0.8)
                        )
grafiekfinal = useOuterStrips(grafiekfunctie, strip=strip.custom(bg="gray70"),strip.left=strip.custom(bg="gray70"))

## show the graph
print(grafiekfinal)



###################GRAFIEK ZONDER LEGENDE ##############################

## create the graph object
grafiekfunctie <- xyplot(WPET~Y|Year.f*Climate.f,data=data,
                         ylab= expression(bold("WP"[ET]*" response (%)")), xlab=expression(bold("Y response (%)")),
                         group=data$FM,
                         abline=list(h=0, v=0,c(a=0,b=1), lty="dotted", col="black"),
                         scales=list(x=list(relation="free",tick.number=3), y=list(tck=c(1,0),relation="same",tick.number=2,alternating=1)),
                         layout=c(3,3), aspect=1,between=list(y=1,x=1),index.cond=list(3:1,3:1),
                         xlim=list(c(-50,200),c(-50,50),c(-50,50),c(-50,200),c(-50,50),c(-50,50),c(-50,50),c(-50,50), c(-50,50)),
                         par.strip=list(cex=0.8)
                         )
grafiekfinal = useOuterStrips(grafiekfunctie, strip=strip.custom(bg="gray74"),strip.left=strip.custom(bg="gray74"))

## show the graph
print(grafiekfinal)


###################GRAFIEK ADAPTED ##############################

## create the graph object
grafiekfunctie <- xyplot(WPET~Y|Year.f*Climate.f,data=data,
                         ylab= expression(bold("WP"[ET]*" response (%)")), xlab=expression(bold("Y response (%)")),
                         group=data$FM,
                         abline=list(h=0, v=0,c(a=0,b=1), lty="dotted", col="black"),
                         scales=list(x=list(relation="free",tick.number=3), y=list(tck=c(1,0),relation="same",tick.number=2,alternating=1)),
                         layout=c(3,3), aspect=1,between=list(y=1,x=1),index.cond=list(3:1,3:1),
                         xlim=list(c(-50,200),c(-50,50),c(-50,50),c(-50,200),c(-50,50),c(-50,50),c(-50,200),c(-50,50), c(-50,50)),
                         par.strip=list(cex=1)
)
grafiekfinal = useOuterStrips(grafiekfunctie, strip=strip.custom(bg="gray74"),strip.left=strip.custom(bg="gray74"))

## show the graph
print(grafiekfinal)



### export as figure
#maak breedte 400 zonder lengte te veranderen
#zet dan breedte op 600 en lengte automatisch meeveranderen



####TO DO #####
(as labels op x as enkel op onderste rij krijgen)
leeg panel wegkrijgen
legende titel in het vet zetten
streepjes van xas labels laten samenvallen met randen plot ( argunment "at=...." helpt niet) argument axs maar geen mooi resultaat
