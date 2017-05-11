install.packages("dplyr")
library(dplyr)
#0 Read CSV file
refine <- read.csv("C:\\Nilanjana\\Springboard\\refine_original.csv", header =  TRUE)

#1 standardize company name
refine_clean <- refine %>% mutate(company = tolower(company)) %>%
  mutate(company = gsub(" ","",company)) %>%
  mutate(company= gsub(".*ps$","philips",company))%>%
  mutate(company = gsub("^ak.*","akzo",company)) %>%
  mutate(company = gsub("^van.*","van houten", company)) %>%
  mutate(company = gsub("^uni.*","unilever",company)) %>%
  
  #2 separate product code and number
  mutate(product_code = gsub("-.*$","",Product.code...number)) %>%
  mutate(product_number = gsub("^*.-","",Product.code...number)) %>%
  
  
  #3 Add product categories
  mutate(product_category = product_code)%>%
  mutate(product_category = replace(product_category,product_category == "p","Smartphone"))  %>%
  mutate(product_category = replace(product_category,product_category=="v","TV")) %>%
  mutate(product_category = replace(product_category,product_category=="x","Laptop")) %>%
  mutate(product_category = replace(product_category,product_category == "q","Tablet")) %>%
  
  #4 Add full address for geocoding  
  mutate(full_address = paste(address,",",city,",",country))%>%
  
  #5 Create dummy variables for comapany and product category
  mutate(company_philips = ifelse(company=="philips",1,0)) %>%
  mutate(company_akzo = ifelse(company == "akzo",1,0))%>%
  mutate(company_van_houten = ifelse(company == "van houten",1,0))%>%
  mutate(company_unilever = ifelse(company == "unilever",1,0)) %>%
  mutate(product_smartphone = ifelse(product_category == "Smartphone",1,0)) %>%
  mutate(product_tv = ifelse(product_category == "TV",1,0)) %>%
  mutate(product_laptop = ifelse(product_category == "Laptop",1,0))%>%
  mutate(product_tablet = ifelse(product_category == "Tablet",1,0))

write.csv(refine_clean,file = "C:\\Nilanjana\\Springboard\\refine_clean.csv")


