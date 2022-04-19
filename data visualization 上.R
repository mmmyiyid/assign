library (reshape2)
library(ggplot2)
GDP <- read.csv("Total GDP.csv")
names(GDP) <- c("Country", "2010","2011","2012","2013","2014","2015")
GDP_Long_Format <- melt(GDP, id="Country") 
names(GDP_Long_Format) <- c("Country", "Year","GDP_USD_Trillion")
ggplot(GDP_Long_Format, aes(x=Year, y=GDP_USD_Trillion, group=Country)) + geom_line(aes(colour=Country)) +
  geom_point(aes(colour=Country),size = 5) +
  labs(title="Gross Domestic Product - Top 10 Countries",
       x="Year", y="GDP (in trillion USD)") + 
  theme(legend.title=element_text(size=20),
  legend.text=element_text(face ="italic",size=15), 
  plot.title=element_text(face="bold", size=20), 
  axis.title.x=element_text(face="bold", size=12), 
  axis.title.y=element_text(face="bold", size=12))

salary <- read.csv("salaries.csv")
ggplot(salary, aes(x=rank, fill=sex)) +
  geom_bar(position="stack") + labs(title='position="stack"') 
ggplot(salary, aes(x=rank, fill=sex)) +
  geom_bar(position="dodge") + labs(title='position="dodge"') 
ggplot(salary, aes(x=rank, fill=sex)) +
  geom_bar(position="fill") + labs(title='position="fill"') + 
  labs(y="% of count")

ggplot(salary, aes(x=rank, y=salary, fill=sex)) +
  geom_col(position="stack") + labs(title='position="stack"') 
ggplot(salary, aes(x=rank, y=salary, fill=sex)) +
  geom_col(position="dodge") + labs(title='position="dodge"') 
ggplot(salary, aes(x=rank, y=salary, fill=sex)) +
  geom_col(position="fill") + labs(title='position="fill"') + 
  labs(y="% of salary total")

ggplot(salary, aes(x=rank, y=salary, fill=sex)) +
  geom_col(position="stack") + labs(title='position="stack"') + 
  coord_flip()


library(dplyr)
library(ggplot2)
select(salary, c(rank,sex,salary)) %>% group_by(rank,sex) %>%
  summarise(av_salary=round(mean(salary),0)) %>%
  ggplot(aes(x=rank, y=av_salary, fill=sex)) +
  geom_col(position="stack") +
  labs(y="Average salary", title='Average salary - Stacked Column Chart') + 
  geom_text(aes(label=av_salary),
  vjust=0.5, position=position_stack(), size=3.5) + 
  annotate("text", x=2, y=250000, label="vjust = 1")















