complete<-function(directory,id=1:332){
    for(i in id){
        if (i<10)
            tmp<-read.csv(paste(directory,"/","00",i,".csv",sep = ""))
        else if (i<100)
            tmp<-read.csv(paste(directory,"/","0",i,".csv",sep = ""))
        else tmp<-read.csv(paste(directory,"/",i,".csv",sep = ""))
        if (i==id[1]){
            p<-dim(tmp[complete.cases(tmp),])
            res<-data.frame(i,p[1])
        }
        else {
            p<-dim(tmp[complete.cases(tmp),])
            res<-rbind(res,data.frame(i,p[1]))
        }
    }
    res
}