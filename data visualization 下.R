library(dplyr)
library(reshape2)
library(ggplot2)
Population <- read.csv("Population All Year.csv") %>%
  melt(id = "Country", variable.name="Year", value.name="Pop_Billion") %>% 
  mutate(Year=substr(Year, 2,length(Year))) %>% 
  filter(Country %in% c('India','China'))

ggplot(Population, aes(Pop_Billion, fill = Country)) +
  geom_histogram(alpha=0.5,col="black", binwidth=0.04) + 
  labs(title="Population (in Billion): Histogram",
       x="Population (in Billion)", y="Frequency") +
  annotate("text", x=0.9, y=7.5, label="alpha = 0.5")

ggplot(Population, aes(Pop_Billion, fill = Country)) +
  geom_density(alpha = 0.8, col="black") + 
  labs(title="Population (in Billion): Histogram",
       x="Population (in Billion)", y="Frequency") +
  annotate("text", x=0.9, y=1.5, label="alpha = 0.8")

GCD_China <- read.csv("China - USD - Percentage.csv") %>%
  melt(id = "Sector", variable.name="Income_Group", 
       value.name="Perc_Cont") %>% # melt by sector
  dcast(Sector~.,value.var=c('Perc_Cont'), mean) %>%
  rename(Avg_Perc_Cont = ".")
ggplot(data=GCD_China, aes(x="", y=Avg_Perc_Cont, fill = Sector)) + 
  geom_col() + coord_polar(theta="y", start = 0) 

GCD_China <- read.csv("China - USD - Percentage.csv") %>%
  melt(id = "Sector", variable.name="Income_Group", 
       value.name="Perc_Cont")
ggplot(data=GCD_China, aes(x="", y=Perc_Cont, fill = Sector)) + 
  geom_col() +
  coord_polar(theta="y", start = 0) + 
  facet_grid(cols=vars(Income_Group)) + 
  scale_fill_brewer(palette="Set3") +
  labs(title="China - Percentage share of each sector by consumption segment", 
       x="Population (in Billion)", y="Frequency", fill="Sector")


library(corrplot)
correlation_world <- read.csv("Correlation Data.csv") 
corrplot(cor(correlation_world[,2:6],method = "pearson"),diag = FALSE,
         method = "ellipse", tl.cex = 0.7, tl.col = "black", cl.ratio = 0.2)

bc<- read.delim("BubbleChart_Data.txt") %>%
  filter(continent != "Oceania", year==2007) %>% 
  droplevels()
ggplot(bc, aes(x = gdpPercap, y = lifeExp, fill=continent)) + scale_x_log10() +
  geom_point(aes(size = sqrt(pop/pi)), pch = 21, show.legend = FALSE) + scale_size_continuous(range=c(1,40)) +
  facet_wrap(~ continent, ncol=2) +
  scale_fill_manual(values = c("#FAB25B", "#276419", "#529624", "#C6E79C")) + 
  theme(text=element_text(size=12),
  title=element_text(size=14,face="bold")) + 
  labs(title="Bubble Chart - GDP Per Captita Vs Life Expectency",
       x="GDP Per Capita(in US $", y="Life Expectancy(in years)")
                                                                                                                                    x="GDP Per Capita(in US $", y="Life Expectancy(in years)")
library(ggrepel)
time_series <- read.csv("timeseries.csv",header=TRUE) %>%
  melt(id = c("Year"), variable.name="Country", value.name="GDP_Growth") %>% 
  mutate(Date=as.Date(Year,format="%d/%m/%Y"))
ggplot(data=time_series,aes(x=Date,y=GDP_Growth)) + 
  geom_line(aes(color=Country),size=1.5)

time_series2 <- time_series %>%
  filter(Country %in% c("India","United.States","United.Kingdom") & 
           (Date > as.Date("2005-01-01") &
            Date < as.Date("2011-01-01"))) %>% 
  mutate(GDP_Growth=round(GDP_Growth,2))
ggplot(data=time_series2,aes(x=Date,y=GDP_Growth)) + 
  geom_line(aes(color=Country),size=1.5) + 
  geom_text_repel(aes(label=GDP_Growth))

cohort <- read.csv("cohort.csv",header=TRUE) %>%
  melt(id = "Credit_Issued", variable.name="Year_Active",
       value.name="Active_Num") %>% 
  mutate(Credit_Issued=factor(Credit_Issued))
blues <- colorRampPalette(c('darkblue', 'lightblue'))
ggplot(cohort, aes(x=Year_Active, y=Active_Num, fill=Credit_Issued,
                   group=Credit_Issued)) + geom_area() +
  scale_fill_manual(values = blues(nlevels(cohort$Year_Active))) + 
  labs(title='Active Credit Cards Volume')






