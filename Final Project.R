lend = read.csv("lending_club_loan.csv", header = TRUE)
library(tidyr)
lend = drop_na(lend)
nrow(lend)
dim(lend)
library(DescTools)
Desc(lend)
summary(lend)
head(lend)
colnames(lend)
library(pastecs)
library(dplyr)
lendsub = select(lend, c('loan_amnt','int_rate','installment','annual_inc',
                         'dti','open_acc','pub_rec','revol_bal','revol_util','total_acc',
                         'mort_acc','pub_rec_bankruptcies'))
stat.desc(lendsub)

library(ggplot2)
ggplot(lend, aes(x = loan_amnt), color = "blue", xlab = "loan amount") + geom_histogram()

qplot(lend$loan_amnt,
      geom="histogram",  
      main="Histogram for loan Amount", 
      xlab="Loan Amount",
      fill=I("lightblue"),
      col=I("black"),binwidth= 1000)

par(mfrow = c(2,2))
#specify the margin
par(mar = rep(2, 4))

hist(lend$int_rate, col = 'lightblue', main = 'Interest Rate')
hist(lend$installment, col = 'lightblue', main = 'Installment')
plot(lend$term, ylim = c(0,60), col = 'lightblue', main = 'Loan Term')
plot(lend$loan_status, col = 'lightblue', main = 'Loan Status')
summary(lend$term)
desc(lend$term)

ggplot(data = lend, aes(x = loan_amnt)) + geom_histogram(binwidth = 1000)


boxplot(lend$loan_amnt, data = lend, xlab = "Number of Cylinders", ylab = "Miles Per Gallon", main = "Mileage Data")
# Save the file. dev.off()

ggplot(lend, aes(x = grade, y = loan_amnt)) + 
  geom_boxplot(fill = "steelblue3", colour = "black", 
  outlier.colour = "black", outlier.shape = 1) +
  labs(title="Loan Amount by Grade", x = "Grade", y = "Loan Amount \n")


ggplot(lend, aes(grade, int_rate)) +
  geom_boxplot(fill = "steelblue3", colour = "black", 
               outlier.colour = "black", outlier.shape = 1) +
  labs(title ="Interest Rate by Grade", x = "Grade", y = "Interest Rate \n")

ggplot(lend, aes(home_ownership, int_rate)) +
  geom_boxplot(fill = "steelblue3", colour = "black", 
               outlier.colour = "black", outlier.shape = 1) +
  labs(title="Interest Rate by Home Ownership", x = "Home Ownership", y = "Interest Rate \n")


ggplot(lend, aes(term, loan_amnt)) +
  geom_boxplot(fill = "steelblue3", colour = "black", 
               outlier.colour = "black", outlier.shape = 1) +
  labs(title="Loan Amount by Term", x = "Term", y = "Loan Amount \n")


library(ggcorrplot)
data(lend, package = "mosaicData")
df <- dplyr::select_if(lend, is.numeric)
r <- cor(df, use="complete.obs")
round(r,2)
ggcorrplot(r,
           hc.order = TRUE,
           type = "lower", lab = TRUE)

library(plotly)
plot_ly(type = "pie", 
        labels = loan_status, 
        values = installment, 
        hole = 0.5,
        marker = list(colors = c("#f2f0f7","#dadaeb","#bcbddc","#9e9ac8","#807dba","#6a51a3","#4a1486"),
                      line = list(width = 1, color = "rgb(52, 110, 165)")),
        sort = F,
        direction = "counterclockwise",
        rotation = 90,
        textinfo = "label+percent",
        textfont = list(size = 14),
        text = paste("Default rates: ", charged),
        textposition = "outside") %>%
  layout(title = 'LOAN ISSUED GROUPED BY STATUS<br>(Hover for breakdown)',
         height = 500, width = 1400, autosize = T, 
         legend = list(font = list(size = 16), x = 1, y = 1, traceorder = "normal"))

pie(lend$loan_amnt, labels = lend$term)
a = table(lend$purpose)
pie(a)


pie(lend$purpose, labels= c(), main = "State pie chart", col = rainbow(length(x)))

write.csv(lend, file = "x.csv")write.csv(lend, blues9file = "x.csv")





inpu <- lend[,c("loan_status","loan_amnt","annual_inc","int_rate","installment", "revol_bal", "revol_util")]
which(is.na(inpu))
input <- na.omit(inpu)
input
which(is.na(input))
print(head(input))
dim(input)
train <- input[1:286411,]
test <- input[286412:358014,]
train_glm<-lm(formula = input$loan_amnt ~ input$loan_status + 
                 input$annual_inc + input$int_rate + input$installment + input$revol_bal + 
                 input$revol_util,data =train)
summary(train_glm)

train_glm<-lm(formula = input$loan_amnt~ input$revol_bal+ input$annual_inc + input$installment, data =train)
summary(train_glm)


data(SaratogaHouses, package="mosaicData") 
houses_lm <- lm(price ~ lotSize + age + landValue +
                                                             livingArea + bedrooms + bathrooms + waterfront, data = SaratogaHouses)
library(pastecs) 
options(scipen = 100) 
options(digits = 2) 
summary(houses_lm)


str(train)
str(test)


par(mfrow = c(2,2))
par(mar = rep(2, 4))

plot(lend$term, col = "blue")
plot(lend$home_ownership, lend$int_rate) 
plot(lend$loan_status)
plot(lend$grade, lend$annual_inc)
# restore the original settings

dev.off

input <- lend[,c("loan_status","loan_amnt","annual_inc","int_rate","installment", "revol_bal", "revol_util")]
dim(input)
train <- input[1:286411,]
test <- input[286412:358014,]
lend_glm<-glm(formula = input$loan_status ~ input$loan_amnt + 
                input$annual_inc + input$int_rate + input$installment + input$revol_bal + 
                input$revol_util, family = binomial, data = train)
summary(lend_glm)

i = lend[,c('loan_status','loan_amnt')]
i
input <- mtcars[,c("am","cyl","hp","wt")]
input
am.data = glm(formula = am ~ cyl + hp + wt, data = input, family = binomial) 
print(summary(am.data))


lend<-read.csv("/Users/mengfeixiong/Documents/lending_club_loan_two.csv")
print(head(lend))

table(lend$loan_status)
a <- sub("Fully Paid",1,lend$loan_status)
b <- sub("Charged Off",0,a)
lend$loan_status <- a                         
lend$loan_status <-b
as.numeric(lend$loan_status)

print(head(lend))
lend$loan_status <- as.numeric(lend$loan_status)
str(lend)

input <- lend[,c("loan_status","loan_amnt","annual_inc","int_rate","installment", "revol_bal", "revol_util")]
print(head(input))
train_glm<-glm(formula = input$loan_status ~ loan_amnt + annual_inc + int_rate + installment + revol_bal + revol_util,data = input[,-1],family = binomial)
summary(train_glm)


train_prob<-predict(train_glm,newdata =input,type = 'response')
print(train_prob)


library(ggplot2)
library(dplyr)

# Create Data
data <- data.frame(
  group=LETTERS[1:5],
  value=c(13,7,9,21,2)
)

# Compute the position of labels
data <- data %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

# Basic piechart
x = lend[,'purpose']
table(x)

ggplot(info, aes(x="", y=, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  
  geom_text(aes(y = ypos, label = group), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")

table(lend$purpose)
install.packages("plotrix")
library(plotrix)
info = c(4697, 83019, 234507, 257, 24030, 2201, 8790, 4196, 2854, 21185, 329, 5701, 2452, 1812)
names = c("car", "credit_card", "debt_consolidation", "educational", "home_improvement", "house", "major_purchase", "medical", "moving", "other", "renewable_energy", "small_business", "vacation", "wedding")
pie(info, labels=names, main = "Purpose")
legend("topright", names, cex=0.5, fill = rainbow(length(info)))

