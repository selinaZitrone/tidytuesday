library(data.table)
library(ggplot2)
library(magrittr)

# data --------------------------------------------------------------------

wildlife_impacts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-23/wildlife_impacts.csv")
wildlife_impacts <- data.table(wildlife_impacts)
states <- readr::read_csv("https://scottontechnology.com/wp-content/uploads/2014/08/50_us_states_all_data.csv", col_names =F) %>% 
  data.table() %>% .[,c("X1","X4"):=NULL] %>% setnames(c("X2","X3"),c("region","state"))

us <- map_data("state")


# model to see what makes sense to plot -----------------------------------

wildlife_impacts[,.N, by=.(incident_month, operator, state, incident_month, height, speed, sky, precip, time_of_day)] %>% 
  glm(N~incident_month + operator + state + incident_month + height + speed + sky + precip + time_of_day, family="poisson", data = .) %>% 
  anova(test="Chisq")

# let's plot operator, state, height, speed, sky, time of day

#add column with full state names to wildlife_impacts
merge(wildlife_impacts, states, by="state") %>% 
  .[,.N,by="region"] %>% 
  .[,region:=tolower(region)] %>%
  merge(us,by="region", all.y = T) %>% 
  .[order(order)] %>% 
  ggplot(aes(long, lat)) +
  geom_polygon(aes(group = group, fill = N)) +
  coord_map("albers", at0 = 45.5, lat1 = 29.5)+
  scale_fill_viridis_c()+
  theme_void()+
  labs(title="Number of wildlife impacts by state", caption = "data from tidytuesday")+
  theme(# Change plot and panel background
    plot.background=element_rect(fill = "darkgrey"),
    panel.background = element_rect(fill = 'darkgrey'),
    text = element_text(color="white"))


ggplot(wildlife_impacts, aes(x=operator, fill = factor(num_engs)))+geom_bar()




# ideas -------------------------------------------------------------------

# operator: barchart with N and type engine, num engines--> does not make sense because we don't know how common those engines are
# sky

