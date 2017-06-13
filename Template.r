############################### PROJECT ###########################################

# set working directory:
setwd("I:\\SM-AVD\\4119 TRiO\\WP 6 Synthesis\\ACI_ Analysis")

###########install packages (just once)##########
#install packages (just once)
#install.packages("haven") 
#install.packages("foreign")
#install.packages("ggplot2") 
#install.packages("Hmisc")
#install.packages("readstata13")
#install.packages("plyr")
#install.packages("gridExtra")
#install.packages("gmodels")
#install.packages("stargazer")

#call packages you are going to use
library("haven")
library("Hmisc")
library("ggplot2")
library("plyr")
library(scales)
library(gridExtra)
library("gmodels")
library(stargazer)

#one way of importing dta from stata 13 or newer
library("readstata13")
N2 <- read.dta13(".\\Stata\\dta\\November.dta",
                 convert.factors = TRUE,
                 convert.dates = TRUE,)

#Another way of importing dataset into a dataframe 
N <- read_dta(".\\Stata\\dta\\November2.dta")
N<-zap_missing(N) #converting stata missing to R missing

#check if it is a dataframe and visualize it
is.data.frame(N) #yes
View(N)
str(N) # will tell you what kind of variables we have in N
#####################exploring data#####################
names(N)
nrow(N) #number of rows
ncol(N)
dim(N) #number of both rows and col
#see more https://swcarpentry.github.io/r-novice-gapminder/05-data-structures-part2/

######################describe data######################
#tab cathegorical variables, it only gives us the number of unique values (if less than 20 values)
table(N$kjonn) 
#this gives the percentage of cathegorical variables
prop.table(table(N$kjonn)
#Cathegorical: you can do both with 
describe(N$kjonn)
#if there are missing values you can exclude them (TRUE) or include them (FALSE)
describe(N$kjonn, exclude.missing=TRUE) #IT DOES NOT WORK!!!!

#2way table
mytable<-table(N$job, N$gender)
prop.table(mytable, 2) #col percentages
#oppure 
prop.table(table(N$job, N$gender), 2)
prop.table(mytable, 1) #row percentages

#dealing with missing!!
prop.table(table(N$utrygg, exclude=NULL)) #this shows the missing
#oppure generate a factor variable with missing
N$unsafe <- factor(N$utrygg, exclude=NULL)
prop.table(table(N$unsafe))

#see more here http://www.statmethods.net/stats/frequencies.html

#For continous Variables (alder it isnot coninous, but we dont have it in this dataset)
summary(N$alder)
sum(N$untrygg, na.rm = TRUE) #if a variable had missing the sum become missing, but you can discard them


###################factor variables#############
describe(N$kjonn)
N$gender<-factor(N$kjonn,levels = c(1,2), labels = c("men", "women" ) )

N$unsafe <- factor(N$utrygg,levels = c(1,2), labels = c("unsafe", "safe" ))

#########selecting variables and subsets
summary(which(N$unsafe=="unsafe")) #this is only selecting observations
prop.table(table(N$unsafe))
prop.table(table(which(N$unsafe=="unsafe")))

Nown <- subset(N, subset=unsafe=="unsafe", 
	select=c(RespondentID:unsafe2)) #this is selecting both obs and variables


####################adding columns/rows to a data frame#######################
#column of a data frame are vectors
#it has to have the same lenght!!!
age <- c(4,5,8)
N <- cbind(N, age)
N

#rows of a dataframe are lists
newRow <- list("newfactor", 3.3, TRUE, 9)
N <- rbind(N, newRow)
#if you are adding a  new factor to a factor variable  you have to:

levels(N$age) <- c(levels(N$age), 'newfactor')
N <- rbind(N, list("newfactor", 3.3, TRUE, 9))


###################dropping rows##########################################
N[-3,] #drops row number 3
na.omit(N) #drops ALL the rows with NA values

#if you want to rename the row names (Number 1 2 3 4 5....)
rownames(N)<- NULL # and R will rename it 1 2 3 4 5.... for you


########################graph########################
#simple histogram and bar plot
describe(N$date)
p1<- ggplot(N, aes(x=date) ) + 
  geom_bar() +theme_bw() +
  ggtitle("Histogram for Date of Survey") +
  labs(x="", y="Count") 
  scale_x_discrete("") 
ggsave("IM\\Date.png") #save a graph

p2<-ggplot(N, aes(gender, fill=gender) ) +
  geom_bar() + theme_bw() +
  ggtitle("Gender share in the sample") +
  labs(x="", y="Count") 

####combine graphs
grid.arrange(p1, p2, nrow=1)

#bar graph bydel#job for male and female
ggplot(N, aes(x=gender, fill=job) ) +
  geom_bar() +theme_bw() +
  scale_y_continuous("Count",limits=c(0,600),breaks=seq(0, 600, 100)) + 
  scale_x_discrete("Gender" )
  ggtitle( "Occupation by Gender" ) 
ggsave("IM\\jobbygender.png")


#percentages instead of counts
ggplot(N, aes(x = gender , fill=gender)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + theme_bw() +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.25, color="black") +
  scale_y_continuous(labels = percent, "Percent") +
  ggtitle("Gender Share in the Sample") 

  #specifying y in the geom_text makes the label start at some y value

#counts on the y axis and labels as percentages!!!
 ggplot(N, aes(x=gender, fill=gender) ) +
  geom_bar() + theme_bw() +
  ggtitle("Gender Share in the Sample") +
  theme(legend.position="none")+
  geom_text(aes( label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", position= position_dodge(width=0.9), vjust = -0.25, color="black") +
    scale_y_continuous("Count",limits=c(0,600),breaks=seq(0, 600, 100)) + 
  scale_x_discrete("" )

#OR create a value variable: H2$value<- apply(Nown2[,8:16], MARGIN = 2, FUN = sum, na.rm = TRUE)/nrow(Nown2)
#and then give it as label geom_text(aes( label =scales::percent(round(value,2))), 


 # geom_bar(position="dodge") #if you do not want it stacked


#+  scale_fill_grey(start=0, end=1) #if want it in grey scale
###labels of the bars 
#label=round(..count..,2) # to reduce the number of decimals


#########legend: change title and labels  
#scale_fill_discrete(name ="Engine", labels=c("V-engine", "Straight engine"))
#put the legend on the bottom horizontally
#theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) 

#######################save a function that makes the same graphs####################
my_plot <- function(title, variable){
p<-ggplot(data.frame(variable), aes(x=variable) ) + geom_bar() + coord_flip()+ ggtitle(title) 
return(p)
}

test <- my_plot("Yrke", job)
print(test)

###########################################################

#################REGRESSIONS-LOGIT##################

l_gender <- glm(unsafe ~  gender , data = N, family = "binomial")
summary(l_gender) #women feel less safe, but it is weird since, the dep variable is feeling "UNSAFE", so a negative value imply more safety.
#iterpret it as: if from women we go to men the probability reduces...

#IS THIS RIGHT??

l_area <- glm(unsafe ~  area , data = N, family = "binomial")
summary(l_area) 


l1 <- glm(unsafe ~ gender +area + job + age , data = N, family = "binomial")
summary(l1)

#on those who marked 2 areas 
l2 <- glm(unsafe2 ~ job + gender + age +area , data = N, family = "binomial")
summary(l2)



#making tables for latex
#http://jakeruss.com/cheatsheets/stargazer.html#html-formatting
stargazer(l_gender, l_area, l1, title="Results", align=TRUE, dep.var.labels=c("Overall Rating","High Rating"),
          covariate.labels=c("Handling of Complaints","No Special Privileges",
                             "Opportunity to Learn","Performance-Based Raises","Too Critical","Advancement"),
          omit.stat=c("LL","ser","f"), no.space=TRUE)



#equivalente di collapse, by in stata per tutte le variabili che iniziano per tmp

by_ctry <- group_by(t_mean80, country)
t_mean80 <- by_ctry %>% 
 summarise_at(vars(starts_with("tmp")), funs(mean_pre=mean(., na.rm=TRUE)))


#mutate_each to rename variables



 #do something over multiple variables:
 #using col number

colnum<-8:16 #give the range
for(i in 1:9){
  X$s[i]<-table(Nown[,colnum[i] ])/nrow(Nown[,colnum[i]])
} 

# oppure piu' semplicemente

X$value<- apply(Nown[,8:16], MARGIN = 2, FUN = sum, na.rm = TRUE)/nrow(Nown)
