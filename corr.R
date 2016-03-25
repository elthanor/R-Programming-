corr<-function(directory,threshold=0){
    frm<-data.frame(sulfate=numeric(),nitrate=numeric())
    res<-numeric()
    for(i in 1:332){
        if (i<10)tmp<-read.csv(paste(directory,"/","00",i,".csv",sep = ""))
        else if (i<100)tmp<-read.csv(paste(directory,"/","0",i,".csv",sep = ""))
        else tmp<-read.csv(paste(directory,"/",i,".csv",sep = ""))
        if(nrow(tmp[complete.cases(tmp),])>=threshold){
            frm<-tmp[complete.cases(tmp),2:3]
            res<-append(res,cor(frm[,1],frm[,2]))
        }
    }
    res
}