# data worden ingelezen
data=read.csv(file=file.choose(), sep = ";", header=TRUE)

#### PACKAGEs LOADEN
library(lattice)
library(latticeExtra)
library(grid)

#### ALGEMENE SETTINGS 

#settings aanpassen zodat kleuren en symbolen worden gekozen zoals ik wil
myTheme <- trellis.par.get()
myTheme$superpose.symbol$col <-c("black","gray48","gray81") 
myTheme$superpose.symbol$fill<-c("black","gray48","gray81") 
myTheme$superpose.symbol$pch <-rep(c(6,1,16,3,25,2,17,0,15), each=3)
myTheme$superpose.symbol$cex <-c(0.75)
myTheme$par.xlab.text$cex<-c(1)
myTheme$par.ylab.text$cex<-c(1)
myTheme$axis.text$cex<-c(0.8)
myTheme$layout.widths$left.padding<-c(0)
myTheme$layout.widths$right.padding<-c(0)
myTheme$layout.widths$key.ylab.padding<-c(0)
myTheme$layout.widths$ylab.axis.padding<-c(0)
myTheme$layout.widths$axis.key.padding<-c(0)
myTheme$layout.heights$top.padding<-c(0)
myTheme$layout.heigths$bottom.padding<-c(0)
myTheme$layout.heights$main.key.padding<-c(0)
myTheme$layout.heigths$key.axis.padding<-c(0)
myTheme$layout.heigths$key.sub.padding<-c(0)
myTheme$layout.heigths$axis.xlab.padding<-c(0)
myTheme$layout.heigths$xlab.key.padding<-c(0)
myTheme$layout.widths$key.right<-c(0)
myTheme$layout.widths$key.left<-c(0)
myTheme$layout.heigths$key.top<-c(0)
myTheme$layout.heights$key.bottom<-c(0)



#for color graphs
myTheme$superpose.symbol$col <-rep(c("gray50", "skyblue","darkslateblue","darkgoldenrod4","orchid3","gold","darkorange","limegreen","darkgreen"),each=3) 
myTheme$superpose.symbol$pch <-rep(c(19,15,3))
myTheme$superpose.symbol$cex <-c(0.75)


#settings checken
trellis.par.set(myTheme)
names(myTheme)
myTheme$superpose.symbol
show.settings(myTheme)

#### CREATIE GRAFIEK FUNCTIES

# creert het grafiek object (trellis object)
FunctieSoil<- xyplot(WPET~Y, data=data,group=FMSoil)
FunctieFert<- xyplot(WPET~Y, data=data,group=FMFert)
FunctieCrop<- xyplot(WPET~Y, data=data,group=FMCrop)


#### CREATE GRAFIEK OBJECTEN 

###########SOIL##################################

GrafiekSoilBig<-update(FunctieSoil,  
                       ylab= expression("WP"[ET]*" response (%)"),xlab= "Y response (%)",
                       aspect=1, 
                       abline=list(h=0, v=0,c(a=0,b=1),lty="dotted", col="grey"),
                       xlim=c(-100, 210), ylim=c(-100, 210),  
                       scales=list(tck=c(1,0),x=list(at=c(-100,0,100, 200),tick.number=4), y=list(at=c(-100,0,100,200), tick.number=4)),
                       #auto.key = list(title= "Legend",cex.title=1.5, columns=1, space="right", cex=1),
                       panel=function(x,y,...){
                            panel.text(-85,195,"A",cex=1.2,font=2)
                            panel.xyplot(x,y,...)
                                        }  
                       )
print(GrafiekSoilBig)
GrafiekSoilSmall<-update(FunctieSoil,  
                         xlab=NULL,ylab=NULL,
                         abline=list(h=0, v=0,c(a=0,b=1),lty="dotted", col="grey"),
                         aspect=1,
                         xlim=c(-100, 100), ylim=c(-100, 100), 
                         scales=list(tck=c(1,0),x=list(at=c(-100,0,100), tick.number=3), y=list(at=c(-100,0,100),tick.number=3)),
                         panel=function(x,y,...){
                                    panel.text(-70,90,"A zoom",cex=1.2,font=2)
                                    panel.xyplot(x,y,...)
                                    }  
                         )
print(GrafiekSoilSmall)

GrafiekSoilSmall2<-update(FunctieSoil,  
                         xlab=NULL,ylab=NULL,
                         abline=list(h=0, v=0,c(a=0,b=1),lty="dotted", col="grey"),
                         aspect=1,
                         xlim=c(-50, 50), ylim=c(-50, 50), 
                         scales=list(tck=c(0,0),x=list(at=c(-50,0,50), tick.number=3), y=list(at=c(-50,0,50),tick.number=3,alternating=2)),
                         panel=function(x,y,...){
                           panel.text(-35,45,"A zoom",cex=1.2,font=2)
                           panel.xyplot(x,y,...)
                         }  
)
print(GrafiekSoilSmall2)
###########Fertility##################################
GrafiekFertBig<-update(FunctieFert,  
                       ylab= expression("WP"[ET]*" response (%)"),xlab= "Y response (%)",
                       aspect=1, 
                       abline=list(h=0, v=0,c(a=0,b=1),lty="dotted", col="grey"),
                       xlim=c(-100, 210), ylim=c(-100, 210),  
                       scales=list(tck=c(1,0),x=list(at=c(-100,0,100,200),tick.number=4), y=list(at=c(-100,0,100,200), tick.number=4)),
                       #auto.key = list(title= "Legend",cex.title=1.5, columns=1, space="right", cex=1),
                       panel=function(x,y,...){
                         panel.text(-85,195,"B",cex=1.2,font=2)
                         panel.xyplot(x,y,...)
                       }  
)
print(GrafiekFertBig)
GrafiekFertSmall<-update(FunctieFert,  
                         xlab=NULL,ylab=NULL,
                         abline=list(h=0, v=0,c(a=0,b=1),lty="dotted", col="grey"),
                         aspect=1,
                         xlim=c(-100, 100), ylim=c(-100, 100), 
                         scales=list(tck=c(1,0),x=list(at=c(-100,0,100), tick.number=3), y=list(at=c(-100,0,100),tick.number=3)),
                         panel=function(x,y,...){
                           panel.text(-70,90,"B zoom",cex=1.2,font=2)
                           panel.xyplot(x,y,...)
                         }  
)
print(GrafiekFertSmall)
###########CROP##################################
GrafiekCropBig<-update(FunctieCrop,  
                       ylab= expression("WP"[ET]*" response (%)"),xlab= "Y response (%)",
                       aspect=1, 
                       abline=list(h=0, v=0,c(a=0,b=1),lty="dotted", col="grey"),
                       xlim=c(-100, 210), ylim=c(-100, 210),  
                       #auto.key = list(title= "Legend",cex.title=1.5, columns=1, space="right", cex=1),
                       scales=list(tck=c(1,0),x=list(at=c(-100,0,100,200),tick.number=4), y=list(at=c(-100,0,100,200), tick.number=4)),
                       panel=function(x,y,...){
                         panel.text(-85,195,"C",cex=1.2,font=2)
                         panel.xyplot(x,y,...)
                       }  
)
print(GrafiekCropBig)
GrafiekCropSmall<-update(FunctieCrop,  
                         xlab=NULL,ylab=NULL,
                         abline=list(h=0, v=0,c(a=0,b=1),lty="dotted", col="grey"),
                         aspect=1,
                         xlim=c(-100, 100), ylim=c(-100, 100), 
                         scales=list(tck=c(1,0),x=list(at=c(-100,0,100), tick.number=3), y=list(at=c(-100,0,100),tick.number=3)),
                         panel=function(x,y,...){
                           panel.text(-70,90,"C zoom",cex=1.2,font=2)
                           panel.xyplot(x,y,...)
                         }  
)
print(GrafiekCropSmall)

#### CREATE VAN GRAFIEK PRINTS          ##herschalen enkel breedte 450 en dan breedte en lengte naar 300
print(GrafiekSoilBig)

print(GrafiekSoilSmall2)

print(GrafiekFertBig)

print(GrafiekCropBig)


























OVERSCHOT TRY OUTS

# pas de grote grafiek aan voor verzamelingfiguur
GrafiekCropBigFig<-update(FunctieCrop,  
                       ylab= expression("WP"[ET]*" response (%)"), xlab= "Y response (%)",
                       aspect=1, 
                       abline=list(h=0, v=0,c(a=0,b=1),lty="dotted", col="grey"),
                       xlim=c(-100, 250), ylim=c(-100, 250),                                   
                       scales=list(tck=c(1,0),x=list(at=c(-100,0,100,250),tick.number=4), y=list(at=c(-100,0,100,250), tick.number=4))
)
#toon het gecreerde object
print(GrafiekCropBigFig)



##### CREATE COLLECTION ONF GRAPHS ON ONE PAGE
## OPTIE 1 : windows metafile ## (uitrekbaar, maar plotten wordt niet hertekend als groter wordt gemaaktin word dus lengte hier al goed specifieren)
trellis.device(win.metafile, filename = "allresponse.wmf", theme=myTheme, width=9, height=7)
print(GrafiekSoilBigFig,split=c(1,1,3,1),more=TRUE)
print(GrafiekFertBigFig,split=c(2,1,3,1),more=TRUE)
print(GrafiekCropBigFig,split=c(3,1,3,1))
trellis.par.set(layout.widths =list(left.padding=0,right.padding=0),
                layout.heights=list(top.padding =0,main.key.padding =0,axis.xlab.padding=0,key.sub.padding  =0,bottom.padding =0)
                )
dev.off()

##OPTIE2: windows venster ## save as functie daar geeft geen goede resultaten
windows(width=2, height=5)
print(GrafiekSoilBigFig,split=c(1,1,3,1),more=TRUE)
print(GrafiekFertBigFig,split=c(2,1,3,1),more=TRUE)
print(GrafiekCropBigFig,split=c(3,1,3,1))





 #comment: je moet eerst lege figuur openenen (device openen) en dan pas de figuur erin printen
panel.width = list(5, "cm"),panel.height=list(5, "cm")















#####TO DO ########
show the legende in klassen (colorlegend, symbol legend)
venster met ingezoomde grafiek bij in venster plaatsen
symbolen ingezoomde grafiek verkleinen
achtergrond ingezoomde grafiek op wit zetten en niet doorschijnend

# plot grafieken in 1 Kader
trellis.device(win.metafile, filename = "Soilall.wmf", theme=myTheme,
               width=2.5, height=2.5)
print(GrafiekSoilBig,position=c(0,0,1,1), more=TRUE)
print(GrafiekSoilSmall, panel.width = list(0.4, "inches"),panel.height=list(0.4, "inches"),position=c(0.5,0,0.7,0.6), more=TRUE)
dev.off()

#grid.arrange(p1, p2, p3 ncol=3)

#### PLOT DE GRAFIEKEN OP 1 PAGINA
trellis.device(win.metafile, filename = "Soil.wmf", theme=myTheme,
               width=2, height=2)
print(GrafiekSoilBig)
dev.off()

