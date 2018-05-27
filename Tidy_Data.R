tidy_data<-function(x){
    sub_train<-fread("subject_train.txt")
    sub_test<-fread("subject_test.txt")
    x_train<-fread("X_train.txt")
    x_test<-fread("X_test.txt")
    y_train<-fread("y_train.txt")
    y_test<-fread("y_test.txt")
    feat<-fread("features.txt")
    act_labs<-fread("activity_labels.txt")
    
    DT1<-cbind(sub_train, x_train, y_train)
    DT2<-cbind(sub_test, x_test, y_test)
    DT3<-rbind(DT1, DT2)
    dim(DT3)
    feat<-within(feat, features<-paste(V1, V2, sep="_"))
    feat$V1<-NULL
    feat$V2<-NULL
    feat_vect<-feat[, features]
    feat_vect<-append("Subject", feat_vect)
    feat_vect<-append(feat_vect, "Type_of_Exersize")
    names(DT3)<-feat_vect
        DT<-DT3 %>%
        select(matches('Mean|mean|std|Std|Subject|Exersize'))
        DT$Type_of_Exersize[DT$Type_of_Exersize==1] <- "Walking"
        DT$Type_of_Exersize[DT$Type_of_Exersize==2] <- "Walking_Upstairs"
        DT$Type_of_Exersize[DT$Type_of_Exersize==3] <- "Walking_Downstairs"
        DT$Type_of_Exersize[DT$Type_of_Exersize==4] <- "Sitting"
        DT$Type_of_Exersize[DT$Type_of_Exersize==5] <- "Standing"
        DT$Type_of_Exersize[DT$Type_of_Exersize==6] <- "Laying"
        
        DT$Subject<-as.factor(DT$Subject)
        DT$Type_of_Exersize<-as.factor(DT$Type_of_Exersize)
        write.csv(DT, file = "tidy_data1.csv")
        return(DT)
        
}

tidy_data2<-function(DT){
    DT_means<-DT %>%
        group_by(Subject, Type_of_Exersize) %>%
        summarise_all(funs(mean))
    write.csv(DT_means, file="tidy_data.csv")
    return(DT_means)
}