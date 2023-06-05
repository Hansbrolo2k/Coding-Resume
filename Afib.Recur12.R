#####################################################
#Summary statistics and Preliminary Analysis (recurr)
#####################################################

summary(Afib_data)


#naniar
install.packages("naniar")
library(naniar)

#aresenal
install.packages("arsenal")
library(arsenal)


#Table.by
table(Afib_data$ABL_type,Afib_data$Recur12)

#Mosaic plot
mp1<-mosaicplot(Afib_data$ABL_type~Afib_data$Recur12,col=c("#E69F00","#0072B2"),main = "Recurrence by Ablation Type",xlab = "Ablation Type", ylab = "Recurrence of Afib (1=recurrence)")

#Baseline variable table.by
Afib_base_vars<- Afib_data[,c(31,2:4,6:24,26,126)]

controls1 <- tableby.control(
  test = F,
  total = T,
  numeric.test = "kwt", cat.test = "fe",
  numeric.stats = c("meansd", "medianq1q3"),
  cat.stats = c("countpct"),
  stats.labels = list(
    meansd = "Mean (SD)",
    medianq1q3 = "Median (Q1, Q3)"
  )
)

CHIP1 <- Afib_data[,c(31,2:4,6:24,26,126)]


tab1 <-tableby(Recur12~ ., 
         data   = CHIP1,
	   control =controls1) %>% 
  summary()
tab1
 
write2word(tab1, "C:/Users/brody/Desktop/School/Work/Internships/INOVA/Afib/Table1.docx")



#select columns for Corrplot
Afib_biomarkers<- Afib_data[,c(31,36:125)]
#View(Afib_biomarkers)

#cleaning data
#Afib_biomarkers<-na.omit(Afib_biomarkers)
Afib_biomarkers[,1]<-as.numeric(Afib_biomarkers[,1])-1
Afib_biomarkers<-as.matrix(Afib_biomarkers)


####Corrrelations####
#which variables have |correlation with Afib| >.15?
Afib_biomarkers_sig<- abs(cor(Afib_biomarkers)[,1]) >.15

y<-abs(cor(Afib_biomarkers)[,1])
y=y[-1]
stem(y)


Afib_biomarkers.15<-Afib_biomarkers[,Afib_biomarkers_sig]
ncol(Afib_biomarkers.15)
#.2: SELP, CNTN1, EGFR, Apn, PON3
#.15:ALCAM, SELP, BLM_HsaD, CNTN1, CCL24, CXCL16, IGFBP1, TrAP, Apn, TNFSF13B, EGFR, PON3, KLK6, IGFBP2

pairs(Afib_biomarkers.15, panel=panel.smooth)

corrplot(cor(Afib_biomarkers.15), method = 'number')
corrplot(cor(Afib_biomarkers.15))

#Assess for non-linear relationships
jpeg("C:/Users/brody/Desktop/School/Work/Internships/INOVA/Afib/pairs1.jpeg",width = 2000, height = 2000)
pairs(Afib_biomarkers[,c(1:30)], panel=panel.smooth)
dev.off()

jpeg("C:/Users/brody/Desktop/School/Work/Internships/INOVA/Afib/pairs2.jpeg",width = 2000, height = 2000)
pairs(Afib_biomarkers[,c(1,31:60)], panel=panel.smooth)
dev.off()

jpeg("C:/Users/brody/Desktop/School/Work/Internships/INOVA/Afib/pairs3.jpeg",width = 2000, height = 2000)
pairs(Afib_biomarkers[,c(1,61:length(Afib_biomarkers))], panel=panel.smooth)
dev.off()

#Scatterplots and Histograms
Afib_biomarkers<-as.data.frame(Afib_biomarkers)

corrplot(cor(Afib_biomarkers), method = 'circle')

Afib_biomarkers %>%
  gather(-Recur12, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = Recur12)) +
    geom_point() +
    facet_wrap(~ var, scales = "free") +
    theme_bw()

#stemleaf plot
stem(abs(cor(Afib_biomarkers.15[,1])))


####SIS####
SIS_Afib_response<-Afib_biomarkers[,1]
SIS_Afib_preds<-Afib_biomarkers[,2:ncol(Afib_biomarkers)]
SIS_Afib_preds<-as.matrix(SIS_Afib_preds)
SIS(x=SIS_Afib_preds,y=SIS_Afib_response,family = "binomial")
colnames(SIS_Afib_preds[,c(24, 47, 73, 78)])
#CNTN1, EGFR, Apn, PON3

####Initial Random Forest####
Afib_biomarkers<-as.data.frame(Afib_biomarkers)
Afib_biomarkers$Recur12<-as.factor(Afib_biomarkers$Recur12)

set.seed(123)
ind <- sample(2, nrow(Afib_biomarkers), replace = TRUE, prob = c(0.75, 0.25))
train <- Afib_biomarkers[ind==1,]
test <- Afib_biomarkers[ind==2,]
rf <- randomForest(Recur12~., data=train, ntree=1000,importance=TRUE,mtry=6)
print(rf)
mtry <- tuneRF(Afib_biomarkers[-1],Afib_biomarkers$Recur12, ntreeTry=50,
               stepFactor=0.5,improve=0.05, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(best.m)
importance(rf)
plot(rf)
abline(v=50,col="dark green",lty=2)

t <- tuneRF(Afib_biomarkers[-1],Afib_biomarkers$Recur12,
       stepFactor = 0.5,
       plot = TRUE,
       ntreeTry = 50,
       trace = TRUE,
       improve = 0.05)

#mtry = 4,9,18

set.seed(123)
ind <- sample(2, nrow(Afib_biomarkers), replace = TRUE, prob = c(0.75, 0.25))
train <- Afib_biomarkers[ind==1,]
test <- Afib_biomarkers[ind==2,]
rf <- randomForest(Recur12~., data=train, ntree=50,importance=TRUE,mtry=4)
print(rf)
mtry <- tuneRF(Afib_biomarkers[-1],Afib_biomarkers$Recur12, ntreeTry=50,
               stepFactor=0.5,improve=0.05, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(best.m)
importance(rf)
plot(rf)
abline(v=50,col="dark green",lty=2)

t <- tuneRF(Afib_biomarkers[-1],Afib_biomarkers$Recur12,
       stepFactor = 0.5,
       plot = TRUE,
       ntreeTry = 50,
       trace = TRUE,
       improve = 0.05)

#mtry = 4,9,18

rf <- randomForest(Recur12~., data=train, ntree=9,importance=TRUE,mtry=4)
print(rf)

hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")
#Variable Importance
varImpPlot(rf,
           sort = T,
           n.var = 15,
           main = "Top 15 - Variable Importance")
#GDF15,GRN,SELP,vWF,CNTN1 (Accuracy)
#PDGFsubA,LDL_rec,TNFr2,FABP4,CSTB (Gini)

## This function carries out random forest with statified sampling
## ssize is a vector of sample sizes
## Variable importance graph will be save in working directory
#Note, because we only have a binary variable here, we no longer have strata
#    rf1 <- randomForest(as.factor(Recur12) ~ ., data = mydt,
#                        ntree = 5000, sampsize = ssize, importance = TRUE, strata = mydt$Recur12,
#                        mtry = mtry)
#    imp.equal <- rf1$importance
#    imp.equal <- apply(imp.equal, 2, function(x) x - min(x))
#    imp.equal <- apply(imp.equal, 2, function(x) x / max(x))
#    imp.equal <- as.data.frame(melt(imp.equal))
#    imp.equal$value <- imp.equal$value * 100
#    names(imp.equal)[1] <- "var"
#    names(imp.equal)[2] <- "type"
#    a <- ggplot(imp.equal, aes(x = var, y = value)) +
#        geom_bar(stat = "identity", width = 0.04, color = "blue") +
#            facet_wrap(~ type, ncol = 6) + coord_flip() +
#                ggtitle(paste0("Sampling Size: ", paste(ssize, collapse = ","))) +
#                    theme(axis.text.y = element_text(size = 13, color = "black"))+
#                        geom_point(size = 3, color = "blue") + ylab("importance")
#    png(filename = paste0("c2imp.", paste(ssize, collapse = "."), ".png"),
#        units = "in", res = 200, width = 16, height = 10)
#    myplot <- suppressWarnings(grid.arrange(a, tableGrob(round(rf1$confusion,2)), nrow = 2))
#    print(myplot)
#    dev.off()
#    return(rf1)

#Size<-c(72)

#x<-plotImp(Size, Afib_biomarkers, mtry = 18)

##################################
#recur vs baseline characteristics
##################################
#select columns for Corrplot
Afib_base_vars<- Afib_data[,c(31,2:4,6:24,26,126)]
Afib_base_vars[,c(1,4,6:22,25)]<- sapply(Afib_base_vars[,c(1,4,6:22,25)], as.numeric)
Afib_base_vars_num<- Afib_data[,c(31,4,7:23,126)]
Afib_base_vars_num<- sapply(Afib_base_vars_num, as.numeric)

#cleaning data
Afib_base_vars_num[,1]<-Afib_base_vars_num[,1]-1
Afib_base_vars[,1]<- Afib_base_vars_num[,1]

corrplot(cor(Afib_base_vars_num), method = 'number')

pairs(Afib_base_vars_num, panel=panel.smooth)

#View(Afib_biomarkers)


#which variables have |correlation with Afib| >.15?
Afib_base_vars_num_sig<- abs(cor(Afib_base_vars_num)[,1]) >.15
Afib_base_vars_num.15<-Afib_base_vars_num[,Afib_base_vars_num_sig]
Afib_base_vars_num.15
ncol(Afib_base_vars_num.15)
#VHD Hx_CABG

#View categorical variable 2x2 tables
table(Afib_base_vars$sex,Afib_base_vars$Recur12)
prop.table(table(Afib_base_vars$sex,Afib_base_vars$Recur12))

table(Afib_base_vars$race,Afib_base_vars$Recur12)
prop.table(table(Afib_base_vars$race,Afib_base_vars$Recur12))

table(Afib_base_vars$afib_type,Afib_base_vars$Recur12)
prop.table(table(Afib_base_vars$afib_type,Afib_base_vars$Recur12))

table(Afib_base_vars$SMK,Afib_base_vars$Recur12)
prop.table(table(Afib_base_vars$SMK,Afib_base_vars$Recur12))

table(Afib_base_vars$ABL_type,Afib_base_vars$Recur12)
prop.table(table(Afib_base_vars$ABL_type,Afib_base_vars$Recur12))

#Convert back to factor variables
Afib_base_vars<- Afib_data[,c(31,2:4,6:24,26,126)]

for(i in c(1,2,3,5:13,15:17,19:24)) Afib_base_vars[,i] <- as.factor(Afib_base_vars[,i]); rm(i)

#Making contingency plots of Recur12 vs factor vars
cat_vars<-Afib_base_vars[,c(1,2,3,5:13,15:17,19:24)]

#initialize vector of tables
vectorOfTables <- vector(mode = "list", length = length(cat_vars)-1)

#make initial tables
for (i in 2:length(cat_vars)){
	vectorOfTables[[i-1]] <- table(cat_vars$Recur12, cat_vars[,i], dnn = c("Recurrence in 12 Months", names(cat_vars[i])))
	vectorOfTables[[i-1]]<-addmargins(vectorOfTables[[i-1]], margin = c(1, 2))
	}

#Pretty tables (run 1 at a time so it doesn't overlap)
table<-tableGrob(vectorOfTables[[20]])
title <- textGrob(paste0("Recurrence vs ", names(cat_vars[20+1])))
#,gp = gpar(fontsize = 8)
padding <- unit(1,"line")
table <- gtable_add_rows(
  table, heights = grobHeight(title) + padding, pos = 0
)
table <- gtable_add_grob(
  table, list(title),
  t = 1, l = 1, r = ncol(table)
)
grid.newpage()
grid.draw(table)

#######################
#Overall Recur analysis
#######################
Afib_vars<- Afib_data[,c(31,2:4,6:24,26,126,36:125)]
#head(Afib_vars)
Afib_num_vars<- Afib_data[,c(4,15,19,126,36:125)]
summary(Afib_num_vars)

#summary for clinitians
mysummary=function(x){c(summary(x),sd=sd(x))}   

mysummary(Afib_num_vars)

for (i in 1:length(Afib_num_vars)){
	print(names(Afib_num_vars[i]))
	print(mysummary(Afib_num_vars[,i]))
}

print(t(apply(Afib_num_vars,2,mysummary)),d=3)

#Ablation type affect AFEQT? - Mosaic plot
ABL_type_data<-Afib_data[,c(31,26)]
ABL_type_data %>%
  group_by(ABL_type) %>%
  mutate(percent = (sum(as.numeric(Recur12))/length(Recur12))-1)

lab <- c("46%","54%","37%","63%")

p<-ggplot(data=ABL_type_data) +
  geom_mosaic(aes(x =ABL_type, fill=Recur12)) +
  ggtitle("Afib Recurrence vs Ablation Type") +
  labs(x = "Ablation Type", y = "Recur12") +
  theme_mosaic() 

###RF
set.seed(123)
train<-Afib_vars
#ind <- sample(2, nrow(Afib_vars), replace = TRUE, prob = c(0.75, 0.25))
#train <- Afib_vars[ind==1,]
#test <- Afib_vars[ind==2,]
rf <- randomForest(Recur12~., data=train, ntree=250,importance=TRUE,mtry=6,classwt=c(.95,.05))
print(rf)
mtry <- tuneRF(Afib_vars[-1],Afib_vars$Recur12, ntreeTry=50,
               stepFactor=0.5,improve=0.05, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(best.m)
importance(rf)
plot(rf)
abline(v=50,col="dark green",lty=2)

t <- tuneRF(Afib_vars[-1],Afib_vars$Recur12,
       stepFactor = 0.5,
       plot = TRUE,
       ntreeTry = 250,
       trace = TRUE,
       improve = 0.05)

#mtry = 5,10

rf <- randomForest(Recur12~., data=train, ntree=250,importance=TRUE,mtry=5,classwt=c(.95,.05))
print(rf)
plot(rf)
abline(v=11,col="dark green",lty=2)

hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")
t <- tuneRF(Afib_vars[-1],Afib_vars$Recur12,
       stepFactor = 0.5,
       plot = TRUE,
       ntreeTry = 11,
       trace = TRUE,
       improve = 0.05)

rf <- randomForest(Recur12~., data=train, ntree=100,importance=TRUE,mtry=10,classwt=c(.95,.05))
print(rf)
plot(rf)
abline(v=11,col="dark green",lty=2)

#Variable Importance
varImpPlot(rf,
           sort = T,
           n.var = 15,
           main = "Top 15 - Variable Importance")

#Accuracy
#Gini



plotImp <- function(ssize, mydt, mtry = 3){
    rf1 <- randomForest(Recur12 ~ ., data = mydt,
                        ntree = 11, sampsize = ssize,
                        strata = mydt$Recur12, importance = TRUE,
                        mtry = mtry)
    imp.equal <- rf1$importance
    imp.equal <- apply(imp.equal, 2, function(x) x - min(x))
    imp.equal <- apply(imp.equal, 2, function(x) x / max(x))
    imp.equal <- as.data.frame(melt(imp.equal))
    imp.equal$value <- imp.equal$value * 100
    names(imp.equal)[1] <- "var"
    names(imp.equal)[2] <- "type"
    a <- ggplot(imp.equal, aes(x = var, y = value)) +
        geom_bar(stat = "identity", width = 0.1, color = "blue") +
            facet_wrap(~ type, ncol = 6) + coord_flip() +
                ggtitle(paste0("Sampling Size: ", paste(ssize, collapse = ","))) +
                    theme(axis.text.y = element_text(size = 3, color = "black"))+
                        geom_point(size = 3, color = "blue") + ylab("importance")
    png(filename = paste0("c2imp.", paste(ssize, collapse = "."), ".png"),
        units = "in", res = 200, width = 16, height = 10)
    myplot <- suppressWarnings(grid.arrange(a, tableGrob(round(rf1$confusion,2)), nrow = 2))
    print(myplot)
    dev.off()
    return(rf1)
}

Size<-c(72)

x<-plotImp(Size, Afib_vars, mtry = 5)
summary(Afib_vars)

###RF Final
library(reshape2)
install.packages("tidytext")
library(tidytext)
library(gridExtra)
library(randomForest)
install.packages("caret")
library(caret)

library(tidyverse)
biomarker <- Afib_vars

Afib_biomarker<- Afib_data[,c(36:125)]


Afib_biomarker_nam<-names(Afib_biomarker)
#x<-model.matrix(Recur12~.-1, Afib_vars)
#x<-x[,-1]

x<-as.data.frame(Afib_vars[-1])
summary(x)


# 1. randomforest
#x <- as.matrix(biomarker[-1])
y <- factor(biomarker$Recur12, labels = c("no", "yes"), )
params_tune <- expand.grid(mtry = c(3, 6, 8, 10, 12, 15, 18))

rf_ctrl <- trainControl(method = "repeatedcv", 
                        number = 10, # k-fold cv,
                        repeats = 3, # repeat k-fold cv ... times
                        classProbs = TRUE, # Estimate class probabilities,
                        summaryFunction = twoClassSummary,
                        allowParallel = TRUE
)

rf_obj <- train(x = x, y = y, 
                method = "rf", 
                metric = "ROC", 
                trControl = rf_ctrl, 
                tuneGrid = params_tune, 
                verbose = FALSE, 
                verbosity = 0
                )

rf_obj$results

# 2. importance.plot

rf1 <- randomForest(factor(Recur12) ~ ., data = biomarker,
                    ntree = 5000, 
                    strata = mydt$Recur12, importance = TRUE,
                    mtry = 3,classwt=c(.95,.05))

imp.equal <- rf1$importance
imp.equal <- apply(imp.equal, 2, function(x) x - min(x))
imp.equal <- apply(imp.equal, 2, function(x) x / max(x))
imp.equal <- as.data.frame(melt(imp.equal))

install.packages("tree")
library(tree)
t1 <- tree(Recur12~., data = Afib_vars, split = "gini")
summary(t1)

plot(t1)
text(t1, pretty = 0)

cv_t1 <- cv.tree(t1, FUN = prune.misclass)
par(mfrow = c(1, 2))
plot(cv_t1$size, cv_t1$dev, type = "b")
plot(cv_t1$k, cv_t1$dev, type = "b")

par(mfrow = c(1, 1))
pr_t1 <- prune.misclass(t1, best = 4)
plot(pr_t1)
text(pr_t1)

# mark different colors for base info and biomarker
vars <- c("sex", "BMI", "race", "afib_type", "Hx_ablt", "antiarr_class1c", "antiarr_class3",
          "antiarr_AMIO", "DM", "HTN", "CAD", "OSA", "HF", "VHD", "STR", "Hx_CABG",
          "Hx_MVS", "Hx_AVS", "Hx_PM", "SMK", "ABL_type")
imp.equal$cat <- "biomarker"
imp.equal[imp.equal$Var1 %in% vars, "cat"] <- "base"

imp.equal$value <- imp.equal$value * 100
names(imp.equal)[1] <- "var"
names(imp.equal)[2] <- "type"



imp.equal %>% 
  mutate(type_level = relevel(type, "MeanDecreaseAccuracy")) %>%  # control which order
  mutate(var = fct_reorder2(var, desc(type_level), desc(value))) %>%  # desc by value 
  ggplot(aes(x = var, y = value,color = cat)) +
  geom_bar(stat = "identity", width = 0.04) +
  facet_wrap(~ type, ncol = 6) + coord_flip() +
  #ggtitle(paste0("Sampling Size: ", paste(ssize, collapse = ","))) +
  theme(axis.text.y = element_text(size = 13, color = "black"))+
  geom_point(size = 3) + ylab("importance") + 
  theme(axis.text.y = element_text(size = 4))+ # control text size 
  scale_color_manual(values=c("red","blue"))
  
#Select subset based on Mean Decrease Gini
vars<-subset(imp.equal, type == "MeanDecreaseGini")
newdata <- vars[order(-vars$value),]
var_list<- c()
for (i in 1:17){
	var_list<-append(var_list, as.character(newdata[c(1:17),1][i]))
	}
var_list

#Select subset based on Mean Decrease Gini
vars<-subset(imp.equal, type == "MeanDecreaseAccuracy")
newdata <- vars[order(-vars$value),]
var_list2<- c()
for (i in 1:11){
	var_list2<-append(var_list2, as.character(newdata[c(1:11),1][i]))
	}
var_list2
pairs(Afib_base_vars_num, panel=panel.smooth)

#Select subset based on 0
vars<-subset(imp.equal, type == "0")
newdata <- vars[order(-vars$value),]
var_list3<- c()
for (i in 1:7){
	var_list3<-append(var_list3, as.character(newdata[c(1:7),1][i]))
	}
var_list3

#Select subset based on 1
vars<-subset(imp.equal, type == "1")
newdata <- vars[order(-vars$value),]
var_list4<- c()
for (i in 1:10){
	var_list4<-append(var_list4, as.character(newdata[c(1:10),1][i]))
	}
var_list4

RFlist1<-union(var_list4,var_list3)

RFlist2<-union(var_list2,var_list)

RFlist3<-union(RFlist1,RFlist2)


##################################
#Trees
##################################
afib_vars<- Afib_data[,c(31,2:4,6:24,26,126,36:125)]
#head(afib_vars)
SIS_vars<- c("Apn","CNTN1","EGFR","PON3")
gen_import<-c("BMI","AfibAge","STR","HF","race")
corr_vars<-c("Apn","CNTN1","EGFR","PON3","SELP")

#Merge importance lists and make dataframe of important variables
#gen_vars <- union(ars_glm,var_list)
gen_vars2 <- union(var_list,var_list2)
gen_vars3 <- union(gen_vars2,var_list3)
gen_vars4 <- union(gen_vars3,var_list4)
gen_vars5 <- union(gen_vars4,SIS_vars)
gen_vars6 <- union(gen_vars5,gen_import)
gen_vars7 <- union(gen_vars6,corr_vars)
import_vars<-gen_vars7

import_vars <- c("Recur12", import_vars)	
view(import_vars)

x_import<-x[, names(x) %in% import_vars]

summary(x_import)

names(x_import)

x_import$Recur12<-Afib_vars[,1]

names(x_import)



# tree
library(rpart)
library(rpart.plot)

#T2
bg_info <- as.data.frame(x_import)
afib_vars<- Afib_data[,c(31,2:4,6:24,26,126,36:125)]
bg_info <- as.data.frame(afib_vars)
bg_only_gini <- rpart(Recur12~., data = bg_info, parms = list(split = "gini"))

plotcp(bg_only_gini)
minCP<-bg_only_gini$cp[which(min(bg_only_gini$cp[,3])==bg_only_gini$cp[,3]),1]
bg_tree <- prune.rpart(bg_only_gini, cp = .15)

#bg_tree <- prune.rpart(bg_only_gini, cp = .01) 

prp(bg_tree, type = 4, fallen.leaves = FALSE, 
    box.palette = "Grays", tweak = 1.1,
    extra = 106, under = TRUE, yesno = 2, gap = 0, space = 0)
rpart.rules(bg_tree, cover = TRUE)
# conduct tree ananysis only with background info 
# and compare with the full model.
summary(bg_tree)
printcp(bg_tree)
table(bg_info$Recur12, predict(bg_tree, type = "class"))
summary(bg_info$Recur12)

bg_info <- as.data.frame(x_import)
afib_vars<- Afib_data[,c(31,2:4,6:24,26,126,36:125)]
bg_info <- as.data.frame(afib_vars)
bg_only_gini <- rpart(Recur12~., data = bg_info, parms = list(split = "gini"))

plotcp(bg_only_gini)
minCP<-bg_only_gini$cp[which(min(bg_only_gini$cp[,3])==bg_only_gini$cp[,3]),1]
bg_tree <- prune.rpart(bg_only_gini, cp = minCP*(1+.04))

#bg_tree <- prune.rpart(bg_only_gini, cp = .05) 

prp(bg_tree, type = 4, fallen.leaves = FALSE, 
    box.palette = "Grays", tweak = 1.1,
    extra = 106, under = TRUE, yesno = 2, gap = 0, space = 0)
rpart.rules(bg_tree, cover = TRUE)
# conduct tree ananysis only with background info 
# and compare with the full model.
summary(bg_tree)
printcp(bg_tree)
table(bg_info$Recur12, predict(bg_tree, type = "class"))
summary(bg_info$Recur12)

#Leave-One-Out Cross-Validation
afib_vars<- Afib_data[,c(31,2:4,6:24,26,126,36:125)]
bg_info <- as.data.frame(afib_vars)
w=matrix(0, ncol=1,nrow=length(x_import[,1]))
for (i in 1:length(bg_info[,1])) {bg_only_gini1 <- rpart(Recur12~., data = bg_info[-i,], 
                      parms = list(split = "gini"))
				#Prune by .05 or min of xerror
				minCP<-bg_only_gini1$cp[which(min(bg_only_gini1$cp[,3])==bg_only_gini1$cp[,3]),1]
				bg_tree1 <- prune.rpart(bg_only_gini1, cp = minCP*(1+.04)) 
				print(rpart.rules(bg_tree1, cover = TRUE))
				#Predict
				pred<-as.numeric(predict(bg_tree1, newdata = bg_info[i,-1])[2]>.5)
				#Accurate? + Store accurate predictions in w
				w[i,1]<-as.numeric(pred==bg_info[i,1])
}

w
apply(w,2,mean)

10/72

g<-rpart(Recur12~., data = bg_info[-1,], 
                      parms = list(prior = c(0.5, 0.5), split = "gini"))
                        c(g$dev,g$aic,g$accuracy)
names(bg_tree)
summary(bg_tree)

###################################
###GLM Probit (Logistic Regression)
###################################

#All
g<-glm(Recur12~., family = binomial(link="probit"), data=Afib_vars)
#g<-glm(Recur12~., family = binomial, data=Afib_vars)
g
p<-step(g)

#All + interaction
#g<-glm(Recur12~.*., family = binomial(link="probit"), data=Afib_vars)
#step(g)
#Failed

#Union of GLM, Random Forest, and Lasso
p<-step(g)
names(p)
p$coefficients
summary(p)
ars_glm <- names(which(abs(p$coefficients)>0.00)[-1])

SIS_vars<- c("Apn","CNTN1","EGFR","PON3")
gen_import<-c("BMI","AfibAge","STR","HF","race")

#Merge importance lists and make dataframe of important variables
#gen_vars <- union(ars_glm,var_list)
gen_vars2 <- union(var_list,var_list2)
gen_vars3 <- union(gen_vars2,var_list3)
gen_vars4 <- union(gen_vars3,var_list4)
gen_vars5 <- union(gen_vars4,SIS_vars)
gen_vars6 <- union(gen_vars5,gen_import)
import_vars<-gen_vars6

import_vars <- c("Recur12", import_vars)	
view(import_vars)
x_import<-x[, names(x) %in% import_vars]

summary(x_import)

x_import$Recur12<-Afib_vars[,1]

names(x_import)

#Only filtered base variables
attach(x_import)
baseg<-glm(Recur12~race+BMI+LVEF+HF+STR+AfibAge,family = binomial)
baseg
summary(baseg)
basep<-step(baseg)
summary(basep)
detach(x_import)

table(x_import$Recur12,x_import$HF)
#Throw out VHD and CABG
#x_import<-x_import[,-c(8:9)]

#Union GLM
g2<-glm(Recur12~., family = binomial(link="probit"), data=x_import)
summary(g2)
p2<-step(g2)
summary(p2)

#Only significant variables
final_import<- x_import[,c("Recur12","BMI","LDL_rec","MCP1","Notch3","IGFBP1","EpCAM","Apn","MB","Gal4","IL1RT2","ST2","SCGB3A2")]

#Final pairs of most important variables
pairs(final_import,panel=panel.smooth)

#Plus interaction:
g3<-glm(Recur12~.*., family = binomial(link="probit"), data=x_import)
ars_glm3 <- names(which(abs(g3$coefficients)>0.00)[-1])
ars_glm3
summary(g3)
p3<-step(g3)
summary(p3)

#Only significant vars with interaction
final_import<- x_import[,c("Recur12","race","BMI","STR","LDL_rec","MCP1","Notch3","IGFBP1","EpCAM","Apn","MB","Gal4","IL1RT2","ST2","SCGB3A2")]
g4<-glm(Recur12~.*., family = binomial(link="probit"), data=final_import)
ars_glm3 <- names(which(abs(g3$coefficients)>0.00)[-1])
ars_glm3
summary(g4)
p4<-step(g4)
summary(p4)

final_import<- x_import[,c("Recur12","race","BMI","STR","LDL_rec","MCP1","Notch3","IGFBP1","EpCAM","Apn","MB","Gal4","IL1RT2","ST2","SCGB3A2")]
g4<-glm(Recur12~.*., family = binomial(link="probit"), data=final_import)

#Only significant of those interactions:
g5<-glm(Recur12~.+race:EpCAM+BMI:Notch3+BMI:IL1RT2+LDL_rec:MCP1+LDL_rec:Apn+LDL_rec:IL1RT2+MCP1:Apn+Notch3:MB+Notch3:IL1RT2, family = binomial(link="probit"), data=final_import)
summary(g5)
p5<-step(g5)
summary(p5)

#Models with Dr Sun:
g6<-glm(Recur12~., family = binomial, data=final_import)
summary(g6)
p6<-step(g6)
summary(p6)
p6

g7<-glm(Recur12~.^2, family = binomial, data=final_import)
summary(g7)
p7<-step(g7)
summary(p7)
p7<-step(g7,dir="ba")
p7
f6<-update(g6,Recur12~.+STR:EpCAM+MCP1:EpCAM+Notch3:MB+Notch3:IL1RT2+BMI:Notch3)
summary(f6)
fp6<-step(f6)
summary(fp6)
summary(p7)

summary(final_import$BMI)
table(final_import$Recur12, final_import$race)

#Default with Biomarkers
g<-glm(Recur12~., family = binomial(link="probit"), data=Afib_vars)
#g<-glm(Recur12~., family = binomial, data=Afib_vars)
g
p<-step(g)
summary(p)

#Default without biomarkers
Afib_base_vars<- Afib_data[,c(31,2:4,6:24,26,126)]
summary(Afib_base_vars)
g<-glm(Recur12~., family = binomial(link="probit"), data=Afib_base_vars)
#g<-glm(Recur12~., family = binomial, data=Afib_vars)
g
p<-step(g)
summary(p)




#Leave-one-out CV

final_import<- x_import[,c("Recur12","race","BMI","STR","LDL_rec","MCP1","Notch3","IGFBP1","EpCAM","Apn","MB","Gal4","IL1RT2","ST2","SCGB3A2")]

#w/o interaction
g6<-glm(Recur12~., family = binomial, data=final_import)
summary(g6)
p6<-step(g6)
summary(p6)
p6



#Union of GLM, Random Forest, and Lasso
p<-step(g)
names(p)
p$coefficients
summary(p)
ars_glm <- names(which(abs(p$coefficients)>0.00)[-1])

SIS_vars<- c("Apn","CNTN1","EGFR","PON3")
gen_import<-c("BMI","AfibAge","STR","HF","race")

#Merge importance lists and make dataframe of important variables
#gen_vars <- union(ars_glm,var_list)
gen_vars2 <- union(var_list,var_list2)
gen_vars3 <- union(gen_vars2,var_list3)
gen_vars4 <- union(gen_vars3,var_list4)
gen_vars5 <- union(gen_vars4,SIS_vars)
gen_vars6 <- union(gen_vars5,gen_import)
import_vars<-gen_vars6

import_vars <- c("Recur12", import_vars)	
view(import_vars)
x_import<-x[, names(x) %in% import_vars]

summary(x_import)

x_import$Recur12<-Afib_vars[,1]
final_import<- x_import[,c("Recur12","race","BMI","STR","LDL_rec","MCP1","Notch3","IGFBP1","EpCAM","Apn","MB","Gal4","IL1RT2","ST2","SCGB3A2")]

final_import<- x_import[,c("Recur12","race","BMI","BLM_HsaD","MCP1","IGFBP1","Apn","Gal4","ST2","SCGB3A2","HF")]

v=matrix(0, ncol=3,nrow=length(final_import[,1]))
for (i in 1:length(final_import[,1])) {g= glm(Recur12~., family = binomial(link="probit"),data=final_import[-i,])
				v[i,1]=g$dev
				v[i,2]=g$aic
				pred<-as.numeric(predict(g, newdata = final_import[i,-1])>.5)
				v[i,3]=as.numeric(pred==final_import[i,1])
}

v
apply(v,2,mean)

#############
#Volcano Plot
#############
BiocManager::install("EnhancedVolcano")
library("EnhancedVolcano")

Afib_biomarkers<- Afib_data[,c(31,36:125)]

Volcdf<- data.frame(matrix(ncol = 3, nrow = (length(Afib_biomarkers)-1)))
x <- c("Biomarker", "Statistic", "p.value")
colnames(Volcdf) <- x

for (i in 2:length(Afib_biomarkers)){
	Volcdf[i-1,1]<-names(Afib_biomarkers)[i]
	x<-t.test(Afib_biomarkers[,i]~Recur12, data= Afib_biomarkers)
	Volcdf[i-1,2]<-x$statistic
	Volcdf[i-1,3]<-x$p.value
}

#?EnhancedVolcano
jpeg(file="C:/Users/brody/Desktop/School/Work/Internships/INOVA/Afib/Volcano.jpeg")
EnhancedVolcano(Volcdf,
                lab = Volcdf$Biomarker,
                x = "Statistic",
                y = "p.value",
                pCutoff = 0.1, #y-axis cut-off line
                FCcutoff = 1, #X-axis cut-off lines
                ylim = c(0, 1.5),
                selectLab = c('SELP','CNTN1','Apn','EGFR','PON3'),
                boxedLabels = TRUE,
                #pointSize = c(ifelse(rdres$corr>0.3, 8, 1)),
                xlim = c(-2.2,2.2),
                drawConnectors = TRUE,
                labSize = 4,
                #shape = c(6, 6, 19, 16),
                title = "Recurrence Volcano Plot",
                subtitle = "",
                #caption = bquote(~Log[2]~ "fold change cutoff, 2; p-value cutoff, 10e-4"),
                #legendPosition = "right",
                #legendLabSize = 14,
                #colAlpha = 0.9,
                #colGradient = c('red3', 'royalblue'),
                #drawConnectors = TRUE,
                #hline = c(10e-8),
                widthConnectors = 0.5)
dev.off()
