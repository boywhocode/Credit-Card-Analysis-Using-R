Customer<- read.csv("C:/Users/Abhijit Rudra/Documents/R/Credit card/Customer Acqusition.csv")
Repayment<-read.csv("C:/Users/Abhijit Rudra/Documents/R/Credit card/Repayment.csv")
Spend<-read.csv("C:/Users/Abhijit Rudra/Documents/R/Credit card/spend.csv")
View(Customer)

#1.	In the above dataset,
#a.	Incase age is less than 18, replace it with mean of age values.
mean=mean(Customer$Age)
mean
Customer$Age=ifelse(Customer$Age<18,mean,Customer$Age)
View(Spend)
View(Customer)
#b.	Incase spend amount is more than the limit, replace it with 
#50% of that customer's limit. #(customer's limit provided in 
#acquisition table is the per transaction limit on his card)
Customer_Spend=merge(Customer,Spend)
View(Customer_Spend)
Customer_Spend$Amount=ifelse(Customer_Spend$Amount>Customer_Spend$Limit,
(Customer_Spend$Amount=.5*Customer_Spend$Limit),Customer_Spend$Amount)

View(Customer_Spend)

#c.	Incase the repayment amount is more than the limit, 
#replace the repayment with the limit.
Customer_Repayment=merge(Customer,Repayment)
View(Customer_Repayment)
Customer_Repayment$Amount=ifelse(Customer_Repayment$Amount>
      Customer_Repayment$Limit,(Customer_Repayment$Amount
      =Customer_Repayment$Limit),Customer_Repayment$Amount)

#2.	From the above dataset create the following summaries:
#a.	How many distinct customers exist?
View(Customer)
distinct(Customer,Customer)

#b.	How many distinct categories exist?
distinct(Customer,Product)

#c.	What is the average monthly spend by customers?
X_format=as.Date(Spend$Month,format="%d-%m-%Y")
class(X_format)
X_month=format(X_format,"%m")
View(Spend)
summarise(group_by(Spend,Customer,Month,Amount),mean(Spend$Amount))

#d.	What is the average monthly repayment by customers?
View(Repayment)
Y_format=as.Date(Repayment$Month,format="%d-%m-%Y")
class(Y_format)
Y_month=format(Y_format,"%m")
summarise(group_by(Repayment,Customer,Month,Amount)
          ,mean(Repayment$Amount))

#f.	What are the top 5 product types?
head(Spend$Type,5)

#g.	Which city is having maximum spend?
View(Customer_Spend)
summarise(group_by(Customer_Spend,City),max(Repayment$Amount))

#h.	Which age group is spending more money
X=summarise(group_by(Customer_Spend,Age),max(Customer_Spend$Amount))
View(X)

#i.	Who are the top 10 customers in terms of repayment?
T=head(Repayment$Amount,10)
T

#3.Calculate the city wise spend on each product on yearly basis.
#Also include a graphical representation for the same.

Cus_Spend=merge(Customer,Spend)
View(Cus_Spend)
V=summarise(group_by(Cus_Spend,City),Total=sum(Cus_Spend$Amount))
V
plot(x=Cus_Spend$City, y=Cus_Spend$Type,main="scatter plot", col="blue")
table(Cus_Spend$City)
count_city=table(Cus_Spend$City)
barplot(count_city)
count_city_type=table(Cus_Spend$City,Cus_Spend$Type)
barplot(count_city_type, main="count city type",xlab="City",ylab="Type")

#4.	Create graphs for
#a.	Monthly comparison of total spends, city wise
plot(x=Cus_Spend$City, y=Cus_Spend$Amount, main="Monthly Spend",Col="blue")

#b.	Comparison of yearly spend on air tickets

