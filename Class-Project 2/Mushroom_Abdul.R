library(aod)
library(ggplot2)
library(psych)
library(gridExtra)

mr<-read.table(file.path("agaricus-lepiota.data"),sep=",", header=FALSE)
str(mr)
mr<- as.data.frame(mr)
names(mr)<-c("class", "cshape", "csurface", "ccolor", "bruises", "odor", "gattach", "gspace", "gsize", "gcolor", "sshape", "sroot", "ssabove", "ssbelow", "scabove", "scbelow", "vtype", "vcolor", "rnumber", "rtype", "spcolor", "popnum", "habitat")

head(mr)
summary(mr) 
sapply(mr, sd) #two-way contingency table of categorical outcome and predictors we want to make sure there are not 0 cells

mr[,1] <- sapply(mr[,1],switch,"p"=0,"e"=1)
mr[,2] <- sapply(mr[,2],switch,"b"=1,"c"=2,"x"=3,"f"=4,"k"=5,"s"=6)
mr[,3] <- sapply(mr[,3],switch,"f"=1,"g"=2,"y"=3,"s"=4)
mr[,4] <- sapply(mr[,4],switch,"n"=1,"y"=2,"w"=3,"g"=4,"e"=5,"p"=6,"b"=7,"u"=8,"c"=9,"r"=10)
mr[,5] <- sapply(mr[,5],switch,"t"=1,"f"=2)
mr[,6] <- sapply(mr[,6],switch,"p"=1,"a"=2,"l"=3,"n"=4,"f"=5,"c"=6,"y"=7,"s"=8,"m"=9)
mr[,7] <- sapply(mr[,7],switch,"f"=1,"a"=2)
mr[,8] <- sapply(mr[,8],switch,"c"=1,"w"=2)
mr[,9] <- sapply(mr[,9],switch,"n"=1,"b"=2)
mr[,10] <- sapply(mr[,10],switch,"k"=1,"n"=2,"g"=3,"p"=4,"w"=5,"h"=6,"u"=7,"e"=8,"b"=9,"r"=10,"y"=11,"o"=12)
mr[,11] <- sapply(mr[,11],switch,"e"=1,"t"=2)
mr[,12] <- sapply(mr[,12],switch,"e"=1,"c"=2,"b"=3,"r"=4,"?"=5)
mr[,13] <- sapply(mr[,13],switch,"s"=1,"f"=2,"k"=3,"y"=4)
mr[,14] <- sapply(mr[,14],switch,"s"=1,"f"=2,"k"=3,"y"=4)
mr[,15] <- sapply(mr[,15],switch,"w"=1,"g"=2,"p"=3,"n"=4,"b"=5,"e"=6,"o"=7,"c"=8,"y"=9)
mr[,16] <- sapply(mr[,16],switch,"w"=1,"p"=2,"g"=3,"n"=4,"b"=5,"e"=6,"o"=7,"c"=8,"y"=9)
mr[,17] <- sapply(mr[,17],switch,"p"=1)
mr[,18] <- sapply(mr[,18],switch,"w"=1,"n"=2,"a"=3,"y"=4)
mr[,19] <- sapply(mr[,19],switch,"a"=1,"t"=2,"n"=3)
mr[,20] <- sapply(mr[,20],switch,"p"=1,"e"=2,"l"=3,"f"=4,"n"=5)
mr[,21] <- sapply(mr[,21],switch,"k"=1,"n"=2,"u"=3,"h"=4,"w"=5,"r"=6,"a"=7,"y"=8,"b"=9)
mr[,22] <- sapply(mr[,22],switch,"s"=1,"n"=2,"a"=3,"v"=4,"y"=5,"c"=6)
mr[,23] <- sapply(mr[,23],switch,"u"=1,"g"=2,"m"=3,"d"=4,"p"=5,"w"=6,"l"=7)

#Plotting
pairs(class ~ cshape + csurface + ccolor, data=mr)

#odor of 6 (c), 4, 5, 1(p) is entirely edible
#odor of 2,3,7,8,9 is entirely poisonous
mosaic(class ~ odor, data=mr)

#color 3 and 4 mostly edible. 1,5,6,7,9 entirely edible
#color 2 almost entirely poisonous 8 is mostly poisonous
mosaic(class ~ spcolor, data=mr)

#gcolor of 1 entirely poisonous
mosaic(class ~ gcolor,data=mr)

#gsize of 2 almost entirely poisonous
mosaic(class ~ gsize,data=mr)

#ssabove of 2 almost entirely poisonous
mosaic(class ~ ssabove,data=mr)

#ssbelow of 2 almost entirely poisonous
mosaic(class ~ ssbelow,data=mr)

#scbelow of 1 entirely poisonous. of 5 almost entirely poisonous
#scbelow of 2 entirely edible
mosaic(class ~ scbelow,data=mr)

#scabove of 1 entirely poisonous. of 5 almost entirely poisonous
mosaic(class ~ scabove,data=mr)

#rnumber of 1 entirely poisonous
mosaic(class ~ rnumber,data=mr)

#rtype of 3 entirely poisonous
mosaic(class ~ rtype,data=mr)

#habitat of 5 is mostly poisonous 
mosaic(class ~ habitat, data=mr)

#sroot with 5 (?) value is entirely edible
#sroot with 3 (b) value is mostly edible
mosaic(class ~ sroot, data=mr)

#vcolor 1 and 2 entirely edible
mosaic(class ~ vcolor,data=mr)

#subset of data to these features:

features <- subset(mr, select = c(odor,spcolor,gsize,gcolor,ssabove,ssbelow,scabove,scbelow,vcolor,rnumber,rtype,habitat,sroot))

#only posinous indications
features2 <- subset(mr, select = c(gcolor,gsize,rnumber,ssabove,ssbelow,rtype))

#performance between 80-83%. not bad
kmeans(features2, 4)

#training and test

#Reasses this
mush = c(mr$edible,mr$`cap-shape`,mr$`cap-surface`)
mush
kmeans(mush, 3)


irisCluster <- kmeans(mush, 3)
irisCluster$cluster <- as.factor(irisCluster$cluster)
ggplot(mush, aes(mr$V1,mr$V2,mr$V3, color = iris$cluster)) + geom_point()

irisCluster$cluster <- as.factor(irisCluster$cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$cluster)) + geom_point()

