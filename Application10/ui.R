
WPM <- function(x,w){
  w<- w/sum(w)
  sum(w*x)
}


w<- c(0.5,0.2,0.3)
x2<-c(0.7,0.6,0.3)
WPM(x2,w)