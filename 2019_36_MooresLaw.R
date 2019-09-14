library(data.table)
library(ggplot2)
library(magrittr)
library(gganimate)
library(viridis)

# function to delete na rows only if the na is in a certain column

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, ..desiredCols]) #what does the .. mean?
  return(data[completeVec, ])
}

cpu <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/cpu.csv") %>% data.table()

gpu <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/gpu.csv") %>% data.table()

ram <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/ram.csv") %>% data.table()

cpu_sub <- cpu[,.(transistor_count, date_of_introduction,designer,area)][,type := "cpu"]
gpu_sub <- gpu[,.(transistor_count,date_of_introduction, designer_s, area)][,type := "gpu"] %>% setnames("designer_s","designer")
ram_sub <- ram[,.(transistor_count, date_of_introduction, manufacturer_s,area)][,type := "ram"] %>% setnames("manufacturer_s", "designer")

all <- rbindlist(list(cpu_sub,gpu_sub,ram_sub)) %>% completeFun(c("transistor_count", "date_of_introduction"))

mean_all <- all[,mean(transistor_count),by=.(type,date_of_introduction)] %>% setnames("V1","transistor_count") %>% 
  merge(data.table(date_of_introduction=rep(1963:2019,3), type=c(rep("cpu", length(1963:2019)),rep("gpu", length(1963:2019)),rep("ram", length(1963:2019)))),
        by=c("type","date_of_introduction"), all.x = T, all.y = T)

mean_all <- completeFun(mean_all, "transistor_count")
                   
minimum <- mean_all[,.(min(date_of_introduction), min(transistor_count)),by=type] %>% setnames(c("V1","V2"), c("min_year","start_value"))

mean_all <- mean_all %>% merge(minimum, by=c("type"), all.x = T,all.y=T)

mean_all[,moore_pred:=start_value*2^((date_of_introduction-min_year)/2)]

mean_all[,gganimateDate := date_of_introduction]
# Moore prediction function:
breaks = 10**(1:11)



plot<-ggplot(all,aes(x=date_of_introduction,y=transistor_count, color=type, fill=type))+
  geom_point(alpha = 0.2 )+
  geom_point(data=mean_all, aes(x=gganimateDate))+geom_line(data=mean_all, aes(x=gganimateDate))+
  geom_line(data=mean_all,aes(y=moore_pred), linetype= "dashed")+
  scale_y_log10(breaks = breaks, labels = scales::comma(breaks), limits = c(10**0, 5*10**11))+
  scale_x_continuous(breaks = seq(1960,2020,10))+
  facet_wrap(~type)+
  ggdark::dark_theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "none",
        text = element_text(color = "grey30", face = "bold"),
        axis.text = element_text(color = "grey30"))+
  scale_color_manual(values=c("#DFCD57","#8184AD","#5CC38C"))+
  geom_text(aes(label = toupper(type), x = 1961, y = 1.5*10^11),   # from cédric
            color = "grey16",
            size = 27,
            hjust = 0
            )+
  labs(x= "Year of introduction", y = "log10 transistor count",title = "MOORE'S LAW", 
       subtitle = "Moore's law states that the given number of transistors in a chip will double every 2 years.",
       caption = "Visualization by Selina Baldauf | datasource: wikipedia")
  
animation <- plot+
  transition_reveal(gganimateDate)

  # Saving out the annimation

options(gganimate.dev_args = list(width = 1000, height = 500))
animate(animation, nframes = 200, fps = 10, end_pause = 10) %>% 

anim_save(filename = "moores_law.gif",
          path = "C:/Users/Selina/Google Drive/tidytuesday", animation = last_animation())
  
  


                     