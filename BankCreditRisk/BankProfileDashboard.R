library(dplyr)
library(tidyr)
library(readxl)

bank_credit_risk_data <- read_excel("Bank Credit Risk Data.xlsx" ,skip=2)
BD <- bank_credit_risk_data

#Customer Profile dashboard
#we are interested in information like genders, marital status, age, savings
#categorical variable, pie  charts (since there isnt too many categories)

GenderFreq <- BD %>% count(Gender)
gen.piepercent <- 100*round(GenderFreq$n/sum(GenderFreq$n),2)
label <- GenderFreq$Gender
label <- paste(label,",",sep="")
label <- paste(label,gen.piepercent)
label <- paste(label,"%")
pie(GenderFreq$n, labels=label, col=c("blue","cyan"), radius=2 ,main="Customer Gender")
hist(BD$Age, main="Customer's Age", xlab="Age", ylab="Frequency", xlim=c(10,80), ylim=c(0,100), labels=TRUE)

#Loan profile dashboard 
#purpose:to understand the frequency of loan taken by customers 
#categorical variable: bar charts 

plot(BD$Age,BD$Savings,main = "Scatterplot of Savings to Age", xlab="Age", ylab= "Savings")

#Loan Customer Analyses Dashboard
#compare loan types across different customers (e.g. gender) 
#clustered bar plots

LoanFreq <- BD %>% count(`Loan Purpose`)
#las=1 is telling us the labels should be horizontal(used in horiz plot), las=2 means labels is vertical
barplot(LoanFreq$n, names.arg=LoanFreq$`Loan Purpose`, cex.names =0.8, horiz=TRUE, las=1)

BD2 <- BD %>% group_by(`Loan Purpose`,Gender) %>% tally()
#we cant plot a barplot using frequency table, the x input has to be a matrix
BD2.spread <- BD2 %>% spread(key=Gender, value=n)
BD2.spread[is.na(BD2.spread)] <- 0 
barmatrix <- as.matrix(BD2.spread[,c(2,3)])
col_bar <- c("red","orange","yellow","green","blue","gray","pink","brown","cyan","violet")
barplot(barmatrix, names.arg=c("Female","Male"),col=col_bar, beside=TRUE)
legend("topleft",cex=0.5, fill=col_bar, BD2.spread$`Loan Purpose`)

#Savings the customers have 
#conduct pareto analysis to see if a small proportion of customers with significant amount of saving
#frequency table with cumulative percentage of total saving
#has to arrange cum % in descending order

library(rpivotTable)
rpivotTable(BD, rows="Loan Purpose", cols=c("Gender","Marital Status"))
#** rpivotTable does not return any value, meaning you cannot use the table to plot a graph
#*# if you want to plot a graph using a dataframe, we have to use other function

BD.sav <- BD %>% select("Savings") %>% arrange(desc(Savings))
BD.sav$percent <- BD.sav$Savings/sum(BD.sav$Savings)
BD.sav$cumsum <- cumsum(BD.sav$percent)
BD.sav$Cumulative.cus <- as.numeric(rownames(BD))/nrow(BD)

which(BD.sav$cumsum > 0.8)[1] #1 returns the length
86/nrow(BD)
#so this tells us that the top 20% of the customers are holding on to 80% of the total savings
