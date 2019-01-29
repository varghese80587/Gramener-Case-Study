#Loading the Required packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

#Loading the data file to be analysed
loan<-read.csv("loan.csv",stringsAsFactors = F, na.strings=c("", " ", "NA", "N/A"))
#Structure of loan data file
str(loan)
#View loan data file
View(loan)

#Cleaning the data
#There are many columns with only NA,0 or 1 which are not required.Removing the columns with NA,0 or 1
loan <-loan[vapply(loan, function(x) length(unique(x)) > 1, logical(1L))]
#Url,desc,title columns are not required for analysis,hence removing those
loan <- select(loan,-c(url,desc,title))
#Removing columns with only 0 and NA as it cannot be used for any analysis
#1.collections_12_mths_ex_med
#2.chargeoff_within_12_mths
#3.tax_liens
loan <- select(loan,-c(collections_12_mths_ex_med,chargeoff_within_12_mths,tax_liens))
#Checking for duplicate entries
loan$id[duplicated(loan$id)]
#As all loans in the data base are induvidual,memberid can be removed.Also Id is not required 
#in the analysis,hence removing this also 
loan <-select(loan,-c(id,member_id))

#Attributes selected for analysis

#Categorical Variable
#1.term 2.grade 3.emp_length
#4.home_ownership 5.verification status
#6.loan_status 7.purpose 

#Continuous Variable
#1.loan_amnt 2.funded_amnt 3.funded_amnt_inv
#4.dti 5.revol_bal 6.annual_inc
loan_final<-select(loan,c(loan_amnt,funded_amnt,funded_amnt_inv,term,grade,emp_length,home_ownership,verification_status,annual_inc,loan_status,purpose,dti,revol_bal))
str(loan_final)
#summary of data set
summary(loan_final)

#character varaible, we will first convert them into factor variable for better analysis

chr_to_factor <-
  function(x) {
    for (i in 1:ncol(x)) {
      if (typeof(x[, i]) == "character") {
        x[, i] <- as.factor(x[, i])
      }
    } 
    return(x)
  }

loan_final <- chr_to_factor(loan_final)


#loan_status have three options:
#1.Fully paid
#2.Charged Off
#3.Current
# We are only interested in fully paid and charged off 
#for our analysis,hence remove current
#Converting the loan_status contents to uppercase for uniformity in data
loan_final$loan_status<-toupper(loan$loan_status)
#Removing loan_status=CURRENT
loan_final<-filter(loan_final,loan_status!="CURRENT")
#Filtering Charged off data 
loan_final_ch<-filter(loan_final,loan_status=="CHARGED OFF")

#Uni-variate analysis
#Chargedoff Applicants 
#Categorical Variable

#1.Home-OWnership
ggplot(loan_final_ch, aes(x= home_ownership,  group=loan_status)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(x="Home Ownership",y = "Percentage", title="Home Ownership of Applicants") +
  facet_grid(~loan_status) +
  scale_y_continuous(labels = scales::percent)

#2.Grade
ggplot(loan_final_ch, aes(x=grade,  group=loan_status)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(x="Grade",y = "Percentage", title="Grade of Applicants") +
  facet_grid(~loan_status) +
  scale_y_continuous(labels = scales::percent)
#3.Purpose
ggplot(loan_final_ch, aes(x=purpose,  group=loan_status)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(x="Loan purpose",y = "Percentage", title="Loan Purpose of Applicants") +
  facet_grid(~loan_status) +
  scale_y_continuous(labels = scales::percent)

#4.Verification Status
 ggplot(loan_final_ch, aes(x=verification_status,  group=loan_status)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(x="Verification Status",y = "Percentage", title="Verification status of Applicants") +
  facet_grid(~loan_status) +
  scale_y_continuous(labels = scales::percent)

 #5.term
 ggplot(loan_final_ch, aes(x=term,  group=loan_status)) + 
   geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
   geom_text(aes( label = scales::percent(..prop..),
                  y= ..prop.. ), stat= "count", vjust = -.5) +
   labs(x="Loan Term",y = "Percentage", title="Loan term of Applicants") +
   facet_grid(~loan_status) +
   scale_y_continuous(labels = scales::percent)
 
#Derived Matrices
#6.Employment Length
#Deriving metrices: emp_length_grp from emp
#Grouping the employment length as
 #1. 0 to 4 years "Junior"
 #2. 5 to 8 years "Mid-Level"
 #3. 9 years and above "Senior
 
loan_final_ch$emp_len_grp <-
  ifelse(
    loan_final_ch$emp_length %in% c("< 1 year", "1 year", "2 years", "3 years", "4 years"),
    "Junior",
    ifelse(
      loan_final_ch$emp_length %in% c("5 years", "6 years", "7 years", "8 years") ,
      "Mid-Level",
      ifelse(
        loan_final_ch$emp_length %in% c("9 years", "10+ years"),
        "Senior",
        "NA"
      )
    )
  )
 
 ggplot(loan_final_ch, aes(x=emp_len_grp,  group=loan_status)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(x="Length of Employment",y = "Percentage", title="Length of Employment of Applicants") +
  facet_grid(~loan_status) +
  scale_y_continuous(labels = scales::percent)


 

#Continuous variable 

#1.Loan Amount 
ggplot(loan_final_ch, aes(x=loan_amnt,fill=loan_status)) +geom_density()

#2.funded Amount
ggplot(loan_final_ch, aes(x=funded_amnt,fill=loan_status)) +geom_density()

#3.funded Amount_inv 
ggplot(loan_final_ch, aes(x=funded_amnt_inv,fill=loan_status)) +geom_density()

#4.Annual Income
ggplot(loan_final_ch, aes(x=annual_inc,fill=loan_status)) +geom_dotplot()+facet_grid(~loan_status)

#Bivariate Analysis
#Derived Metrices

# Derived Dti
# Dti is Debt to Income Ratio.Revolving balance is the portion  
# that goes unpaid at the end of a billing cycle.so including revol_bal in dti
# derived_dti=(revol_bal/annual_inc)+dti
# Checking for NAs
 which(is.na(loan_final$dti))
 which(is.na(loan_final$annual_inc))
 which(is.na(loan_final$revol_bal))
 
loan_final$derived_dti=(loan_final$revol_bal/loan_final$annual_inc)+loan_final$dti
ggplot(loan_final, aes(x=derived_dti,fill=loan_status)) +geom_density()





