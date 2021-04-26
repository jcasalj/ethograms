extras <-
  c("tidyverse")
if (length(setdiff(extras, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(extras, rownames(installed.packages())))
}

lapply(extras, require, character.only = TRUE)

library(readxl)

data <- read_excel("dest.xlsm")
  
summ_data <- data %>% map(function(x) tibble(behaviour = x[-length(x)], followed_by = x[-1])) %>% bind_rows()  %>% 
  group_by(behaviour, followed_by) %>% 
  filter(behaviour != followed_by) %>% 
  summarize(N = n()) %>% 
  mutate(freq = N / sum(N))

ggplot(summ_data, aes(x = behaviour, y = followed_by)) + 
  geom_point(aes(size =freq, color = N)) + 
  scale_y_continuous(limits = c(0.5, 8.5), breaks = seq(1, 8, by = 1)) + 
  scale_x_continuous(limits = c(0.5, 8.5), breaks = seq(1, 8, by = 1)) +
  scale_color_gradient(low = "pink", high =  "green") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  scale_size(breaks=seq(0.1,1.0, by = 0.2), limits=c(0,1)) +
  labs(title="Next behaviour", subtitle="Excluding itself")