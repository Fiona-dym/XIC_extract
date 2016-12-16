## install mzxml parsing package
install.packages("readMzXmlData")
library(readMzXmlData)
library(ggplot2)

## ????????????
input<-read.delim("mz_rt_list",header = T)
exampleDirectory<-"/Users/dengyamei/Desktop/"
spec <- readMzXmlFile(file.path(exampleDirectory, "TTE-75-1-01-3.mzXML"))
#len <- length(spec)


## main function
RT_intensity <- function(mz=NULL,rt=NULL,spec=NULL,rt_tol=5,mz_tol=0.05){
  rt_x<-c()
  int_y<-c()
  mz_range_min <- (mz - mz_tol)
  mz_range_max <- (mz + mz_tol)
  message(mz)
  message(rt)
  for(i in seq(along=spec)){
    if(spec[[i]]$metaData$msLevel == 1 ){
      rt_exp <- spec[[i]]$metaData$retentionTime / 60
      if(abs(rt_exp - rt) <= rt_tol){
        int_sum <- 0
        for(k in 1:length(spec[[i]]$spectrum$mass)){
          if( (spec[[i]]$spectrum$mass[k] >=  mz_range_min) &&(spec[[i]]$spectrum$mass[k] <=  mz_range_max)){
            int_sum <- int_sum+spec[[i]]$spectrum$intensity[k]
          }
        }
        rt_x <- c(rt_x,rt_exp)
        int_y <- c(int_y,int_sum)
      }
    }
  }
  res<-data.frame(rt=rt_x,intensity=int_y)
  return(res)
}


## get XIC information
dim(input)[1]
test1 <- RT_intensity(mz=input$m.z[1],rt=input$RT.min.[1],spec=spec)
test2 <- RT_intensity(mz=input$m.z[2],rt=input$RT.min.[2],spec=spec)
test3 <- RT_intensity(mz=input$m.z[3],rt=input$RT.min.[3],spec=spec)
test4 <- RT_intensity(mz=input$m.z[4],rt=input$RT.min.[4],spec=spec)
test5 <- RT_intensity(mz=input$m.z[5],rt=input$RT.min.[5],spec=spec)
test6 <- RT_intensity(mz=input$m.z[6],rt=input$RT.min.[6],spec=spec)
test7 <- RT_intensity(mz=input$m.z[7],rt=input$RT.min.[7],spec=spec)
test8 <- RT_intensity(mz=input$m.z[8],rt=input$RT.min.[8],spec=spec)
test9 <- RT_intensity(mz=input$m.z[9],rt=input$RT.min.[9],spec=spec)
test10 <- RT_intensity(mz=input$m.z[10],rt=input$RT.min.[10],spec=spec)


## ??????
g1<-ggplot(test1,aes(rt,intensity))+geom_line(color="red")+theme_classic()+xlab("RT (min)")+ylab("intensity")+theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())+theme(axis.text.x = element_text(face="bold",size=8),axis.title.y=element_text(face="bold", colour="darkred",size=14),axis.title.x=element_text(face="bold", colour="darkred",size=14))
ggsave(g1,file="xic-1.png",width = 4,height = 4)
g2<-ggplot(test2,aes(rt,intensity))+geom_line(color="red")+theme_classic()+xlab("RT (min)")+ylab("intensity")+theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())+theme(axis.text.x = element_text(face="bold",size=8),axis.title.y=element_text(face="bold", colour="darkred",size=14),axis.title.x=element_text(face="bold", colour="darkred",size=14))
ggsave(g2,file="xic-2.png",width = 4,height = 4)
g3<-ggplot(test3,aes(rt,intensity))+geom_line(color="red")+theme_classic()+xlab("RT (min)")+ylab("intensity")+theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())+theme(axis.text.x = element_text(face="bold",size=8),axis.title.y=element_text(face="bold", colour="darkred",size=14),axis.title.x=element_text(face="bold", colour="darkred",size=14))
ggsave(g3,file="xic-3.png",width = 4,height = 4)
g4<-ggplot(test4,aes(rt,intensity))+geom_line(color="red")+theme_classic()+xlab("RT (min)")+ylab("intensity")+theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())+theme(axis.text.x = element_text(face="bold",size=8),axis.title.y=element_text(face="bold", colour="darkred",size=14),axis.title.x=element_text(face="bold", colour="darkred",size=14))
ggsave(g4,file="xic-4.png",width = 4,height = 4)
g5<-ggplot(test5,aes(rt,intensity))+geom_line(color="red")+theme_classic()+xlab("RT (min)")+ylab("intensity")+theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())+theme(axis.text.x = element_text(face="bold",size=8),axis.title.y=element_text(face="bold", colour="darkred",size=14),axis.title.x=element_text(face="bold", colour="darkred",size=14))
ggsave(g5,file="xic-5.png",width = 4,height = 4)
g6<-ggplot(test6,aes(rt,intensity))+geom_line(color="red")+theme_classic()+xlab("RT (min)")+ylab("intensity")+theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())+theme(axis.text.x = element_text(face="bold",size=8),axis.title.y=element_text(face="bold", colour="darkred",size=14),axis.title.x=element_text(face="bold", colour="darkred",size=14))
ggsave(g6,file="xic-6.png",width = 4,height = 4)
g7<-ggplot(test7,aes(rt,intensity))+geom_line(color="red")+theme_classic()+xlab("RT (min)")+ylab("intensity")+theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())+theme(axis.text.x = element_text(face="bold",size=8),axis.title.y=element_text(face="bold", colour="darkred",size=14),axis.title.x=element_text(face="bold", colour="darkred",size=14))
ggsave(g7,file="xic-7.png",width = 4,height = 4)
g8<-ggplot(test8,aes(rt,intensity))+geom_line(color="red")+theme_classic()+xlab("RT (min)")+ylab("intensity")+theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())+theme(axis.text.x = element_text(face="bold",size=8),axis.title.y=element_text(face="bold", colour="darkred",size=14),axis.title.x=element_text(face="bold", colour="darkred",size=14))
ggsave(g8,file="xic-8.png",width = 4,height = 4)
g9<-ggplot(test9,aes(rt,intensity))+geom_line(color="red")+theme_classic()+xlab("RT (min)")+ylab("intensity")+theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())+theme(axis.text.x = element_text(face="bold",size=8),axis.title.y=element_text(face="bold", colour="darkred",size=14),axis.title.x=element_text(face="bold", colour="darkred",size=14))
ggsave(g9,file="xic-9.png",width = 4,height = 4)
g10<-ggplot(test10,aes(rt,intensity))+geom_line(color="red")+theme_classic()+xlab("RT (min)")+ylab("intensity")+theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())+theme(axis.text.x = element_text(face="bold",size=8),axis.title.y=element_text(face="bold", colour="darkred",size=14),axis.title.x=element_text(face="bold", colour="darkred",size=14))
ggsave(g10,file="xic-10.png",width = 4,height = 4)

