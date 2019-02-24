library(stringr)

I=12 #number of litters
J=rep(c(2,2,4,5,6,7), 2) #number of individuals/litter
sim.diff=0 #null effect
sd.litter=1 #difference between litters
sd.res=0.5 #difference within litter

b <- seq(-0.05,1.05,0.05)
length(b)

empty<- array(dim=c(1000,1))
#empty <- as.data.frame(c(b,empty))

index <- function(ind){

    litter.ID=rep(c(1:I), times=J)
    animal.ID=sequence(J)
    treat.ID=rep(c(0,1), each=0.5*length(animal.ID)) #each group with parallel litter sizes
    
    s.mean.0=rep(rnorm(n=0.5*I, mean=0, sd=sd.litter), times=J[1:c(0.5*I)])
    s.mean.1=rep(rnorm(n=0.5*I, mean=sim.diff, sd=sd.litter), times=J[1:c(0.5*I)])
    s.y=rnorm(n=sum(J), mean=c(s.mean.0, s.mean.1), sd=sd.res)
    
    #Create Df
    s.data <- as.data.frame(cbind(litter.ID, animal.ID, treat.ID, s.y), row.names = NULL)
    
    #Type I errors
    p<- summary(lm(s.y~treat.ID, data=s.data))$coefficient[8]
    empty[ind,]<<-p   #BASE VALUES + IND
    
    e <- as.data.frame(empty)
    
    colors <- c(rep("lightsteelblue3",1), rep("white",22))
    
   g <-  ggplot(data=e, aes(e[,1])) +  #CHANGED FROM $V1 to [,1]
      geom_histogram(fill="white", binwidth=0.05, col="black") +
      labs(x="Saved P-value", y= "Number of studies")+
     scale_x_continuous(limits=c(-0.05,1.05), expand = c(0, 0)) +
     scale_y_continuous(limits=c(0,300), expand = c(0, 0)) +
     theme_classic() + theme(panel.grid = element_blank(), panel.border = element_blank())
      #ylim(0,400)+
      #xlim(-0.05,1.05)
    
      ggsave(g, filename = paste0("/Users/Desiree/Documents/New R Projects/Histograms/hist",str_pad(ind, width=4, pad="0"),".jpeg"), device="jpeg", width=100, height=100, units = "mm", dpi=150)
}

lapply(1:1000, function(x) index(x))


#ImageMagick
setwd("~/Documents/New R Projects/Histograms/Every 10 Files/")
my_command <- 'convert *.jpeg -delay 1 -loop 1 animation.gif'
system(my_command)


prop <- function(x){
  pr <- length(x[x<0.05])/1000 #1000--> number of rows
  return(pr)
}
type <- apply(empty, 2, prop)
type






