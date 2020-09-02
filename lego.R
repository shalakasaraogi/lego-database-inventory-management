#Loading pacakges
library(ggplot2) 
library(readr) 
library(tidyverse,warn.conflicts = FALSE)
library(RColorBrewer)

#Understanding the colors dataset
read.csv("D:/lego/colors.csv") -> colors

#
head(colors)
num_colors<-length(unique(colors$name))
paste("Number of Unique colors in Lego sets =",num_colors) 

#Adding '#' to rgb values
colors<-colors%>%mutate(rgb=paste0("#",str_trim(rgb)))
my_color<-colors$rgb
names(my_color)<-my_color

#Count of colors which are transparent/not
colors %>%group_by(is_trans)%>%summarize(c=n())%>%
  ggplot(aes(x=is_trans,y=c,fill=is_trans))+geom_bar(stat="identity")
---------------------------------------------------------------------------
#Understanding the sets dataset
read.csv("D:/lego/sets.csv") -> sets

head(sets)
str(sets)

#Visualizing the avg. number of parts/year

part_year<-sets%>%select(year,num_parts)%>%group_by(year)%>%summarise(avg_parts=mean(num_parts))

ggplot(data = part_year,aes(x=year,y=avg_parts))+
  geom_line(col="palegreen4",size=1)+
  geom_point(col="darkgoldenrod",size=2) + 
  labs(title = "Average Parts per year")

#Visualizing the avg. number of themes/year

theme_year<-sets %>%select(year,theme_id)%>%group_by(year)%>%summarise(theme_count=length(unique(theme_id)))

ggplot(data = theme_year,aes(x=year,y=theme_count))+
  geom_line(col="palegreen4",size=1)+
  geom_point(col="darkgoldenrod",size=2) + 
  labs(title = "Themes used per year")

---------------------------------------------------
#Understanding 'parts' & 'part-categories' dataset
read.csv("D:/lego/part_categories.csv") -> part_categories    
read.csv("D:/lego/parts.csv") -> parts

str(part_categories)

#changing column names
colnames(part_categories)[2]<-"part_category_name"
colnames(parts)[2]<-"part_name"
colnames(colors)[2]<-"color_name"

#Joining 'part_categories' & 'parts' datasets
part_cat<-part_categories %>%left_join(parts,by=c("id"="part_cat_id"))
head(part_cat)

#Visualizing the number of parts under each category
part_cat %>%select(part_category_name,part_num)%>%group_by(part_category_name)%>%summarise(parts_under_cat=length(unique(part_num)))%>%
  arrange(desc(parts_under_cat)) -> parts_per_cat

ggplot(data = parts_per_cat,aes(x=reorder(part_category_name,parts_under_cat),y=parts_under_cat,fill=part_category_name)) +coord_flip() +geom_bar(stat="identity")+theme(legend.position = "none")+
  labs(title="Parts under Category",x="Category",y="No of Parts")

#Joining 'part_cat' with 'inventory_parts' & 'colors'

read.csv("D:/lego/inventory_parts.csv") -> inventory_parts

part_color<-part_cat%>%left_join(inventory_parts,by="part_num")%>%
  left_join(colors,by=c("color_id"="id"))

partsp_col<-part_color%>%select(color_name,rgb,part_name)%>%group_by(color_name,rgb)%>%summarise(part_per_color=length(unique(part_name)))%>%arrange(desc(part_per_color))%>%head(100)
head(part_color)

-------------------------------------------------------------------
#number of parts/lego set  
library(ggrepel)  

read.csv("D:/lego/themes.csv") -> themes

#changing column names
colnames(themes)[2]<-"theme_name"
colnames(sets)[2]<-"set_name"
themes<-themes[,-3]

#Joining 'sets' & 'themes' datasets
set_themes<-themes %>%left_join(sets,by=c("id"="theme_id"))

#Getting parts_count for each set
set_count<-set_themes%>%
  group_by(set_name)%>%
  summarise(parts_count=sum(num_parts))%>%
  arrange(desc(parts_count))

head(set_count,10) %>% 
  ggplot(aes(x=reorder(set_name,parts_count),y=parts_count,fill=set_name)) + 
  geom_bar(stat = "identity")+coord_flip() + theme(legend.position = "none") +
  labs(title = "Top 10 logo sets with maximum part counts",x="Parts count",y="Set Name")
-----------------------------------------------------------------------
#Top 50 Themes based on set counts
  
  themes_per_set<-set_themes%>%
  select(theme_name,set_num)%>%
  group_by(theme_name)%>%
  summarise(set_cnt=length(unique(set_num)))%>%
  arrange(desc(set_cnt))%>%head(50)  

#Building a treemap
treemap(themes_per_set,
        index="theme_name",
        vSize="set_cnt",
        type="index",
        fontsize.labels=7,
        palette=my_color,
        title="Lego Themes Based on Set Counts"
)
---------------------------------------------------------------------------
  