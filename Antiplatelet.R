#################
#install packages
#################
#readxl
install.packages("readxl")
library(readxl)

#openxlsx
install.packages("openxlsx")
library(openxlsx)

#dplyr
install.packages("dplyr")
library(dplyr)

#naniar
install.packages("naniar")
library(naniar)

#aresenal
install.packages("arsenal")
library(arsenal)

#############
#Read in Data
#############

Antiplatelet1 <- read.xlsx("C:/Users/brody/Desktop/School/Work/Internships/INOVA/Antiplatelet/Antiplatelet.xlsx",sheet=1,rows=c(1,7,21,25,29,30,34,36,41,44,45,47,49,52,53,59,61,67,68,72,77,79,82,83,84,89,92,97,98,99,101,106,107,109,110,112,113,116,118,119,121,123,127,137,145,153,157,161,171,179,180,181,186,187,190,191,192,197,199,200,209,212,234,235,257,264,266,274,275,280,290,298,300,315,326,327,330,335,338,345,348))

Antiplatelet1<-as.data.frame(Antiplatelet1)

summary(Antiplatelet1)


Antiplatelet2 <- read.xlsx("C:/Users/brody/Desktop/School/Work/Internships/INOVA/Antiplatelet/Antiplatelet.xlsx",sheet=2,rows=c(1,3:80),cols=c(1:47,54:55))

Antiplatelet2<-as.data.frame(Antiplatelet2)

summary(Antiplatelet2)


Antiplatelet3 <- read.xlsx("C:/Users/brody/Desktop/School/Work/Internships/INOVA/Antiplatelet/Antiplatelet.xlsx",sheet=3,rows=c(1,3:80))

Antiplatelet3<-as.data.frame(Antiplatelet3)

summary(Antiplatelet3)


Antiplatelet4 <- read.xlsx("C:/Users/brody/Desktop/School/Work/Internships/INOVA/Antiplatelet/Antiplatelet.xlsx",sheet=4,rows=c(1,3:80))

Antiplatelet4<-as.data.frame(Antiplatelet4)

summary(Antiplatelet4)


#Merge datasets
Antiplatelet<-merge(
x= Antiplatelet1, # Dataset1
y= Antiplatelet2,  # Dataset2
by="Patient.Number")

Antiplatelet<-merge(
x= Antiplatelet, # Dataset1
y= Antiplatelet3,  # Dataset2
by="Patient.Number")


Antiplatelet<-merge(
x= Antiplatelet, # Dataset1
y= Antiplatelet4,  # Dataset2
by="Patient.Number")

summary(Antiplatelet)

names(Antiplatelet)

###############
#Final Cleaning
###############

na_strings <- c("NA", "N A", "N / A", "N/A", "N/ A", "Not Available", "NOt available","Not documented","Unknown","Unavailable")
Antiplatelet<-as.data.frame(Antiplatelet%>%
  replace_with_na_all(condition = ~.x %in% na_strings))

#Combine Treatments 2,3,4,5 in new column
Antiplatelet[,59]<-as.factor(Antiplatelet[,59])
Antiplatelet$Initial.Antiplatelet.Strategy<- as.factor(recode(Antiplatelet[,59], '0'=0,'1'=1,'2'=2,'3' = 2,'4'=2,'5'=2))
#Antiplatelet$"Anticoagulation.Loading.(units.of.heparin)"

#Remove patient who died before treatment
Antiplatelet<-Antiplatelet[-14,]
#Patient 18 Missing for PCI and beyond
Antiplatelet<-Antiplatelet[-77,]

#########################################
#Table Creation
#.0 = summary with all treatments
#.5 = hypothesis test with reduced groups
#########################################

#Table 1 - BASELINE CHARACTERISTICS 

for (i in c(4,5,11,12,13,14,15,16,17)){
	Antiplatelet[,i]<-as.factor(Antiplatelet[,i])
}

summary(Antiplatelet[,c(59,3,4,5,6,7,8,11,12,13,14,15,16,17)])

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

CHIP1 <- Antiplatelet[,c(59,3,4,5,6,7,8,11,12,13,14,15,16,17)]

tab1 <-tableby(Initial.Antiplatelet.Strategy.for.Analysis~ ., 
         data   = CHIP1,
	   control =controls1) %>% 
  summary()
tab1
 
write2word(tab1, "C:/Users/brody/Desktop/School/Work/Internships/INOVA/Antiplatelet/Table1.docx")

controls1.5 <- tableby.control(
  test = T,
  total = F,
  numeric.test = "kwt", cat.test = "fe",
  numeric.stats = c("meansd", "medianq1q3"),
  cat.stats = c("countpct"),
  stats.labels = list(
    meansd = "Mean (SD)",
    medianq1q3 = "Median (Q1, Q3)"
  )
)


CHIP1.5 <- Antiplatelet[,c(105,3,4,5,6,7,8,11,12,13,14,15,16,17)]

#Antiplatelet[14,]

tab1.5 <-tableby(Initial.Antiplatelet.Strategy~ ., 
         data   = CHIP1.5,
	   control =controls1.5) %>% 
  summary()
tab1.5
 
write2word(tab1.5, "C:/Users/brody/Desktop/School/Work/Internships/INOVA/Antiplatelet/Table1.5.docx")



#Table 2 - PRE ADMISSION MEDS
for (i in c(21,22,23,24,25,26)){
	Antiplatelet[,i]<-as.factor(Antiplatelet[,i])
}
summary(Antiplatelet[,c(21,22,23,24,25,26)])

controls2 <- tableby.control(
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

CHIP2 <- Antiplatelet[,c(59,21,22,23,24,25,26)]

tab2 <-tableby(Initial.Antiplatelet.Strategy.for.Analysis~ ., 
         data   = CHIP2,
	   control =controls2) %>% 
  summary()
tab2
 
write2word(tab2, "C:/Users/brody/Desktop/School/Work/Internships/INOVA/Antiplatelet/Table2.docx")

controls2.5 <- tableby.control(
  test = T,
  total = F,
  numeric.test = "kwt", cat.test = "fe",
  numeric.stats = c("meansd", "medianq1q3"),
  cat.stats = c("countpct"),
  stats.labels = list(
    meansd = "Mean (SD)",
    medianq1q3 = "Median (Q1, Q3)"
  )
)


CHIP2.5 <- Antiplatelet[,c(105,21,22,23,24,25,26)]

tab2.5 <-tableby(Initial.Antiplatelet.Strategy ~ ., 
         data   = CHIP2.5,
	   control =controls2.5) %>% 
  summary()
tab2.5
 
write2word(tab2.5, "C:/Users/brody/Desktop/School/Work/Internships/INOVA/Antiplatelet/Table2.5.docx")


#Table 3 - PRESENTATION
for (i in c(9,10,27,28,29)){
	Antiplatelet[,i]<-as.factor(Antiplatelet[,i])
}
summary(Antiplatelet[,c(9,10,27,28,29)])

controls3 <- tableby.control(
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

CHIP3 <- Antiplatelet[,c(59,9,10,27,28,29)]

tab3 <-tableby(Initial.Antiplatelet.Strategy.for.Analysis~ ., 
         data   = CHIP3,
	   control =controls3) %>% 
  summary()
tab3
 
write2word(tab3, "C:/Users/brody/Desktop/School/Work/Internships/INOVA/Antiplatelet/Table3.docx")

controls3.5 <- tableby.control(
  test = T,
  total = F,
  numeric.test = "kwt", cat.test = "fe",
  numeric.stats = c("meansd", "medianq1q3"),
  cat.stats = c("countpct"),
  stats.labels = list(
    meansd = "Mean (SD)",
    medianq1q3 = "Median (Q1, Q3)"
  )
)


CHIP3.5 <- Antiplatelet[,c(105,9,10,27,28,29)]

tab3.5 <-tableby(Initial.Antiplatelet.Strategy ~ ., 
         data   = CHIP3.5,
	   control =controls3.5) %>% 
  summary()
tab3.5
 
write2word(tab3.5, "C:/Users/brody/Desktop/School/Work/Internships/INOVA/Antiplatelet/Table3.5.docx")


#Table 4 - ECMO COURSE (In intra-ECMO shhet)
for (i in c(32,43,44,45)){
	Antiplatelet[,i]<-as.factor(Antiplatelet[,i])
}
summary(Antiplatelet[,c(32,35,43,44,45)])

controls4 <- tableby.control(
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

CHIP4 <- Antiplatelet[,c(59,32,35,43,44,45)]

tab4 <-tableby(Initial.Antiplatelet.Strategy.for.Analysis~ ., 
         data   = CHIP4,
	   control =controls4) %>% 
  summary()
tab4
 
write2word(tab4, "C:/Users/brody/Desktop/School/Work/Internships/INOVA/Antiplatelet/Table4.docx")

controls4.5 <- tableby.control(
  test = T,
  total = F,
  numeric.test = "kwt", cat.test = "fe",
  numeric.stats = c("meansd", "medianq1q3"),
  cat.stats = c("countpct"),
  stats.labels = list(
    meansd = "Mean (SD)",
    medianq1q3 = "Median (Q1, Q3)"
  )
)


CHIP4.5 <- Antiplatelet[,c(105,32,35,43,44,45)]

tab4.5 <-tableby(Initial.Antiplatelet.Strategy ~ ., 
         data   = CHIP4.5,
	   control =controls4.5) %>% 
  summary()
tab4.5
 
write2word(tab4.5, "C:/Users/brody/Desktop/School/Work/Internships/INOVA/Antiplatelet/Table4.5.docx")


#Table 5 - BLEEDING COMPLICATIONS (in intra-ECMO sheet)

#Create Major bleeding composite (headbleed + Hemotosis)
Antiplatelet[32,66]<-'No'

#Create Minor bleeding composite 
for (i in c(63,66,65,67,68,69,70)){
	Antiplatelet[,i]<-as.factor(Antiplatelet[,i])
}
#Major Bleeding
#Antiplatelet[,c(63,66,67)]
Antiplatelet$Major.Bleed<- as.numeric((as.numeric(Antiplatelet[,63])-1+as.numeric(Antiplatelet[,66])-1+as.numeric(Antiplatelet[,67])-1)>0)
Antiplatelet$Major.Bleed<-as.factor(Antiplatelet$Major.Bleed)
Antiplatelet[32,66]<-NA

#Create Minor bleeding composite 
minor.dat = data.frame(matrix(nrow = length(Antiplatelet[,65]), ncol = 0)) 
minor.dat$Nasal.Bleed<- as.numeric(Antiplatelet[,65])-1
minor.dat$Hematuria<- as.numeric(Antiplatelet[,68])-1
minor.dat$Cannulation.Site.Bleeding<- as.numeric(Antiplatelet[,69])-1
minor.dat$Bleeding.NOS<- as.numeric(Antiplatelet[,70])-1
View(minor.dat)
Antiplatelet$Minor.Bleed<-as.numeric(apply(minor.dat,1,sum,na.rm=TRUE)>0)
Antiplatelet$Minor.Bleed<-as.factor(Antiplatelet$Minor.Bleed)

summary(Antiplatelet[,c(106,63,66,107,65,67,68,69,70)])

controls5 <- tableby.control(
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

CHIP5 <- Antiplatelet[,c(59,106,63,66,67,107,65,68,69,70)]

tab5 <-tableby(Initial.Antiplatelet.Strategy.for.Analysis~ ., 
         data   = CHIP5,
	   control =controls5) %>% 
  summary()
tab5
 
write2word(tab5, "C:/Users/brody/Desktop/School/Work/Internships/INOVA/Antiplatelet/Table5.docx")

controls5.5 <- tableby.control(
  test = T,
  total = F,
  numeric.test = "kwt", cat.test = "fe",
  numeric.stats = c("meansd", "medianq1q3"),
  cat.stats = c("countpct"),
  stats.labels = list(
    meansd = "Mean (SD)",
    medianq1q3 = "Median (Q1, Q3)"
  )
)


CHIP5.5 <- Antiplatelet[,c(105,106,63,66,67,107,65,68,69,70)]

tab5.5 <-tableby(Initial.Antiplatelet.Strategy ~ ., 
         data   = CHIP5.5,
	   control =controls5.5) %>% 
  summary()
tab5.5
 
write2word(tab5.5, "C:/Users/brody/Desktop/School/Work/Internships/INOVA/Antiplatelet/Table5.5.docx")



#Table 6 - THROMBOTIC COMPLICATIONS
for (i in c(77,76,79)){
	Antiplatelet[,i]<-as.factor(Antiplatelet[,i])
}
summary(Antiplatelet[,c(77,76,79,89)])


controls6 <- tableby.control(
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

CHIP6 <- Antiplatelet[,c(59,77,76,79,89)]

tab6 <-tableby(Initial.Antiplatelet.Strategy.for.Analysis~ ., 
         data   = CHIP6,
	   control =controls6) %>% 
  summary()
tab6
 
write2word(tab6, "C:/Users/brody/Desktop/School/Work/Internships/INOVA/Antiplatelet/Table6.docx")

controls6.5 <- tableby.control(
  test = T,
  total = F,
  numeric.test = "kwt", cat.test = "fe",
  numeric.stats = c("meansd", "medianq1q3"),
  cat.stats = c("countpct"),
  stats.labels = list(
    meansd = "Mean (SD)",
    medianq1q3 = "Median (Q1, Q3)"
  )
)


CHIP6.5 <- Antiplatelet[,c(105,77,76,79,89)]

tab6.5 <-tableby(Initial.Antiplatelet.Strategy ~ ., 
         data   = CHIP6.5,
	   control =controls6.5) %>% 
  summary()
tab6.5
 
write2word(tab6.5, "C:/Users/brody/Desktop/School/Work/Internships/INOVA/Antiplatelet/Table6.5.docx")


#Table 7 - PCI
#Need to catch NAs and make numeric (92)

Antiplatelet[,c(95)]<-droplevels(Antiplatelet[,c(95)])

for (i in c(92,95,102)){
	Antiplatelet[,i]<-as.factor(Antiplatelet[,i])
}
summary(Antiplatelet[,c(91,92,94,95,102)])
summary(Antiplatelet[,c(92,95)])
Antiplatelet[,92]<-as.numeric(Antiplatelet[,92])

controls7 <- tableby.control(
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

CHIP7 <- Antiplatelet[,c(59,91,92,94,95,102)]

tab7 <-tableby(Initial.Antiplatelet.Strategy.for.Analysis~ ., 
         data   = CHIP7,
	   control =controls7) %>% 
  summary()
tab7
 
write2word(tab7, "C:/Users/brody/Desktop/School/Work/Internships/INOVA/Antiplatelet/Table7.docx")

controls7.5 <- tableby.control(
  test = T,
  total = F,
  numeric.test = "kwt", cat.test = "fe",
  numeric.stats = c("meansd", "medianq1q3"),
  cat.stats = c("countpct"),
  stats.labels = list(
    meansd = "Mean (SD)",
    medianq1q3 = "Median (Q1, Q3)"
  )
)


CHIP7.5 <- Antiplatelet[,c(105,91,92,94,95,102)]

tab7.5 <-tableby(Initial.Antiplatelet.Strategy ~ ., 
         data   = CHIP7.5,
	   control =controls7.5) %>% 
  summary()
tab7.5
 
write2word(tab7.5, "C:/Users/brody/Desktop/School/Work/Internships/INOVA/Antiplatelet/Table7.5.docx")



