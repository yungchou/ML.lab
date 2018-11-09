rm(list=ls())

library(dplyr)

orders <- read.csv('data/orders.csv', header = TRUE)
returns <- read.csv('data/returns.csv', header = TRUE)

str(orders)
orders$Profit <- as.numeric(orders$Profit); class(orders$Profit)
orders$Sales <- as.numeric(orders$Sales); class(orders$Sales)

aggr(orders)

orders$Order.Date <- as.Date(as.character(orders$Order.Date), '%m/%d/%y')

trend <- orders %>%
  mutate(month = format(orders$Order.Date, "%m"), year = format(orders$Order.Date, "%Y")) %>%
  group_by(year,month) %>%
  summarise(total = n())

#library(plot3D)
#scatter3D(trend$year, trend$month, trend$total)

trend$cat <- unique(orders$Category)

i=1
for (c in trend$cat){
  trend$cat[which(trend$cat==i)] <- c
  i=i+1
}
trend$cat <- as.factor(trend$cat)

library(plotly)
p3d <- plot_ly(trend, type = 'scatter3d',
               mode='lines', # comment out for scatter plot
               y = ~trend$year, x = ~trend$month, z = ~trend$total,
               color = ~trend$cat,
               colors = c('#BF382A', '#0C4B8E'),
               #surfaceaxis=1, # add a surface axis ('1' refers to axes[1] i.e. the y-axis)
               #surfacecolor=c('#BF382A', '#0C4B8E'),
               marker = list( opacity = 0.5, size=3,
                              line = list(color = '#262424',width = 0.1))) %>%
  layout(scene = list(
    xaxis = list(title = 'month(x)'),
    yaxis = list(title = 'year(y)'),
    zaxis = list(title = 'quantity(z)')))
p3d

# RETURNS
ret <- returns %>%
  group_by(returns$Region) %>%
  summarise(total=n())

ret$total <- order(ret$total)

par(mai=c(1.8,1,1,1)) # margin
barplot(ret$total, cex.names=0.8,
        las=2, names.arg=ret$`returns$Region`,
        xlab="",
        ylab="Returns", ylim=c(0,25),
        main="Returns by Region"
        )
mtext("Region", side=1, line=7, las=1)

# Loss DUE TO RETURNS EACH YEAR

m <- merge(orders, returns, by='Order.ID')

m1 <- m %>%
  select(Order.Date,Profit) %>%
  mutate(year = format(Order.Date, "%Y")) %>%
  select(year,Profit) %>% mutate(year=as.factor(year))

sum(m1$Profit)
plot(m1)

#---
c1 <- m %>% group_by(Customer.ID) %>% summarise(total=n())
c5 <- c1 %>% filter(total>=5)
cat('customer returned order at least once = ',nrow(c1))
cat('cuatomer retuened order more than 5 times = ',nrow(c5))

#----

re5 <- m %>% group_by(Region.x) %>%
  summarise(total=n()) %>% top_n(5)

print('top 5 regions on returning orders')
re5

sc5 <- m %>% group_by(Sub.Category) %>%
  summarise(total=n()) %>% top_n(5)

print('top 5 subcategory on returning orders')
sc5

#-----------


