#histogram animiation of the uniform distribution

library(stringr)


I=12 #number of litters
J=rep(c(2,2,4,5,6,7), 2) #number of individuals/litter
sim.diff=0 #null effect
sd.litter=1 #difference between litters
sd.res=0.5 #difference within litter


sem <- function(x){
  sd(x)/sqrt(length(x))
}

empty.uni<- array(dim=c(1000,1))

index.uni <- function(ind){
  
  litter.ID=rep(c(1:I), times=J)
  animal.ID=sequence(J)
  treat.ID=rep(c(0,1), each=0.5*length(animal.ID)) #each group with parallel litter sizes
  
  s.mean.0=rep(rnorm(n=0.5*I, mean=0, sd=sd.litter), times=J[1:c(0.5*I)])
  s.mean.1=rep(rnorm(n=0.5*I, mean=sim.diff, sd=sd.litter), times=J[1:c(0.5*I)])
  s.y=rnorm(n=sum(J), mean=c(s.mean.0, s.mean.1), sd=sd.res)
  
  #Create Df
  s.data <- as.data.frame(cbind(litter.ID, animal.ID, treat.ID, s.y), row.names = NULL)
  
  s.avg <- aggregate( s.y ~ litter.ID, s.data, mean)
  s.new <- s.avg
  colnames(s.new) <- c("litter.ID", "litter.avg")
  s.new$group <- rep(c(0,1), each=I*0.5)
  
  #Type I errors
  p<- summary(lm(litter.avg~group, data=s.new))$coefficient[8]
  empty.uni[ind,]<<-p
  
  
  e.uni <- as.data.frame(empty.uni)
  
  colors <- c(rep("lightsteelblue3",1), rep("white",22))
  
  g.uni <-  ggplot(data=e.uni, aes(e.uni$V1)) + 
    geom_histogram(fill="white", binwidth=0.05, col="black", 
                   aes()) +
    labs(x="Saved P-value", y = "Number of studies")+
    scale_x_continuous(limits=c(-0.05,1.05), expand = c(0, 0)) +
    scale_y_continuous(limits=c(0,300), expand = c(0, 0)) +
    theme_classic() + theme(panel.grid = element_blank(), panel.border = element_blank())
  #ylim(0,400)+
  #xlim(-0.05,1.05)
  
  ggsave(g.uni, filename = paste0("/Users/Desiree/Documents/New R Projects/Hist_uni/hist",str_pad(ind, width=4, pad="0"),".jpeg"), device="jpeg", dpi=150, width=100, height=100,  units = "mm")
}

lapply(1:1000, function(x) index.uni(x))

#ImageMagick, creates gif

setwd("~/Documents/New R Projects/Hist_uni/Every 10 Files/")
my_command <- 'convert *.jpeg -delay 1 -loop 1 animation.gif'
system(my_command)

prop <- function(x){
  pr <- length(x[x<0.05])/1000 #1000--> number of rows
  return(pr)
}
type <- apply(empty.uni, 2, prop)
type



setwd("~/Documents/New R Projects/Animation plots/")
my_command <- 'convert *.jpeg -delay 1 -loop 1 animation.gif'
system(my_command)



