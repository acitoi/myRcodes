##################     FUNCTIONS                 #####################



#df= dataframe
#string= the beginning of the name of all the variables to select
new_small_df<- function(df,string,labels)
{
  #how many values are there is the variables identified by "string"
  L<-length(grep(x = colnames(df), pattern = string))
  value<- rep(0,L) #genera una colonna di zeri lunga L
  type<- rep(1:L) #genera numeri da 1 a L
  X<-data.frame(value, type) #makes it dataframe
  #find the right columns in the df 
  j <- colnames(df) #take all names in "df"
  a <-j[grep(string,j)]  #select those who start with "string"
  b<-df[,a]   #select the actual columns in "df"
  #create the percentage 
  X$value<- apply(
    				b, 
    				MARGIN = 2, #means coloumnsgrad
    				FUN = sum, na.rm = TRUE
   					)/nrow(df)
  X$number<- apply(
    				b, 
    				MARGIN = 2, 
    				FUN = sum, na.rm = TRUE)
  X$type<- factor(X$type, labels= labels)
  return(X)
}


