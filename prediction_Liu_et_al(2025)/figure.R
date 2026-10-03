if (!requireNamespace("pacman", quietly = TRUE)) { 
  install.packages("pacman")
} 
library("pacman")

p_load(tidyverse, viridis, patchwork)

#In the current paper, we only present the predictions for the mid-evet and the edge-event sets 
#under the ties-allowed condition 

#data 
edge_events <- read.csv("nonlinear_edge_0.csv")
edge_events_with_d <- read.csv("nonlinear_edge_03.csv")
mid_events <- read.csv("nonlinear_mid_0.csv")
mid_events_with_d <- read.csv("nonlinear_mid_03.csv")

df1 <- rbind(edge_events, edge_events_with_d, mid_events, mid_events_with_d) |> 
  pivot_longer(cols = c(mean_correct, mean_type1, mean_type2, mean_type3),
               names_to = "Response category",
               values_to = "value")

df1$`Response category`[df1$`Response category` == "mean_type1"] <- "Stacked-illogical"
df1$`Response category`[df1$`Response category` == "mean_type2"] <- "Interlaced-illogical"
df1$`Response category`[df1$`Response category` == "mean_type3"] <- "Other-illogical"
df1$`Response category`[df1$`Response category` == "mean_correct"] <- "Logical"
df1$`Response category` <- factor(df1$`Response category`, 
                                  levels = c("Other-illogical", "Interlaced-illogical", "Stacked-illogical", "Logical"))
df1$event_types[df1$event_types == "edge event sets"] <- "Edge-event set"
df1$event_types[df1$event_types == "middle event sets"] <- "Mid-event set"


##### area plot 
p1_area <- ggplot(df1, aes(x=sample_size, y=value, fill=`Response category`)) +
  geom_area(alpha=0.5 , linewidth=.5, colour="black") + 
  facet_grid(noise_term ~ event_types, labeller = label_bquote(row = italic("d")~"="~.(noise_term))) +
  xlab(expression(paste("Sample size ", italic(N)))) +
  ylab(expression(paste("Predicted response probability")))  + 
  theme(text = element_text(size = 14)) + 
  scale_fill_manual(values=c( "#0072B2",  "#CC79A7","#F0E442","#009E73"))
p1_area

##### sliced line plot 
df2 <- rbind(edge_events, edge_events_with_d, mid_events, mid_events_with_d) |>  
  mutate(con_type3 = mean_type3 / (1 - mean_correct),
         con_type1 = mean_type1 / (1- mean_type3 - mean_correct))  |>  
  pivot_longer(cols = c(mean_correct, con_type1, con_type3),
               names_to = "Response category",
               values_to = "error_value")  |>  
  select(sample_size, noise_term, event_types, 
         `Response category`, error_value)

df2$`Response category`[df2$`Response category` == "con_type1"] <- "Stacked-illogical"
df2$`Response category`[df2$`Response category` == "con_type2"] <- "Interlaced-illogical"
df2$`Response category`[df2$`Response category` == "con_type3"] <- "Other-illogical"
df2$`Response category`[df2$`Response category` == "mean_correct"] <- "Logical"
df2$`Response category` <- factor(df2$`Response category`, 
                                  levels = c("Logical", "Other-illogical",
                                             "Stacked-illogical", "Interlaced-illogical"))
df2$event_types[df2$event_types == "edge event sets"] <- "Edge-event set"
df2$event_types[df2$event_types == "middle event sets"] <- "Mid-event set"

levels(df2$noise_term) <- c("0" = "0",
                            "0.3" = "0.3")

p2 <- ggplot(df2, aes(sample_size, error_value)) +
  geom_line(aes(linetype =  event_types))  +
  scale_y_continuous(breaks = seq(0, 1, 0.5)) +
  scale_linetype_manual(values=c("solid", "dotted", "dotdash")) +
  labs(linetype = "Event set type") +
  xlab(expression(paste("Sample size ", italic(N)))) +
  ylab(expression(paste("Predicted (conditional) probability"))) +
  facet_grid(noise_term ~ `Response category`, labeller = label_bquote(row = italic("d")~"="~.(noise_term))) + 
  theme(legend.position = "right",
        text = element_text(size = 14)) 

p2

#combine the plots
theme_apa <- theme_bw(base_size = 10) +
  theme(
    panel.grid            = element_blank(),
    strip.background      = element_rect(fill = "white", colour = "black"),
    strip.text            = element_text(size = 9),
    legend.key            = element_blank(),
    legend.title          = element_text(size = 10, vjust = 1),
    legend.title.position = "left",
    legend.text           = element_text(size = 9),
    legend.position       = "bottom",
    legend.direction      = "vertical",
    legend.justification  = c(0.5, 1),   # centered under its panel, top-aligned
    legend.key.width      = unit(1.5, "lines"),
    plot.tag              = element_text(size = 12, face = "bold")
  )

fig1 <- (p1_area | p2) +
  plot_layout(widths = c(2, 3)) +        # no guides = "collect"
  plot_annotation(tag_levels = "A") &
  theme_apa

fig1

ggsave("sampling_prediction.pdf", fig1, width = 9, height = 6)



