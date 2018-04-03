rm()
getwd()
#cannot name : **

dat_tab <- read.csv("C:/Users/pratha/Dropbox (**)/**/**.csv", header = TRUE)

#using ggplot for visualization

#install.packages("ggplot2")
library("ggplot2")

#GPA distribution of converted

#filter out GPA of converted students

#dplyr
library("dplyr")
dat_tab2 <- filter(dat_tab, Converted > 0, Last.Institution.GPA < 5) 
class(dat_tab2$Last.Institution.GPA)
#pipline
dat_tab3 <- dat_tab2 %>% mutate(ID = row_number())
dat_tab4 <- dat_tab3 %>% group_by(Last.Institution.GPA) 

dat_frame<-data.frame(dat_tab2$Last.Institution.GPA, dat_tab2$Converted)
#head(dat_tab)
#View(dat_tab)
plot(dat_frame$Last.Institution.GPA, dat_tab2$Converted)
#ggplot(data = dat_frame, aes(x=dat_tab2$Converted, y=dat_tab2$Last.Institution.GPA))
#c2 <- ggplot(dat_tab4)
#c2+geom_qq(aes(sample=dat_frame$dat_tab2.Last.Institution.GPA))

g <- ggplot(dat_tab4, aes(Last.Institution.GPA, ID))
g + geom_count()

-------------------
#creating new file for tableau : steps
  #1. identifying important variables from the file
  #2. cleaning missing values and outliers not to be considered
  #3. Creating range for gpa - number of students 
  #4. writing new file for tableau
  
View(dat_tab)

# keeping : one unique ID column - leadID, campaign.name, city, Lead.State.or.Province, Ethnicity, enrollment.term, 
#last.institution.attended, GPA, converted 
#
unique(dat_tab$Campaign.Name)
unique(dat_tab$Country)  
unique(dat_tab$Last.Institution.GPA)  

#unique(is.na(dat_tab$))

convtab <- data.frame(dat_tab$Lead.ID, dat_tab$Campaign.Name, dat_tab$City, dat_tab$Lead.State.or.Province, dat_tab$Ethnicity,
                      dat_tab$Enrollment.Term..Term.Name, dat_tab$Last.Institution.Attended..Account.Name, dat_tab$Last.Institution.GPA
                      , dat_tab$Converted)
write.csv(convtab, file="usetableau.csv")



convtab[convtab==""] <- NA

unique(is.na(convtab$dat_tab.Ethnicity))

#to see how many rows have NA
row.has.na <- apply(convtab, 1, function(convtab){any(is.na(convtab))})
sum(row.has.na)
#drop the rows
convtab2 <- convtab[!row.has.na,]
write.csv(convtab2, file="usetableau.csv")

#drop higher GPA
convtab2

convtab3 <- filter(convtab2,  dat_tab.Last.Institution.GPA < 5)
write.csv(convtab3, file="usetableau.csv")
