extras <-
  c("tidyverse", "readxl")
if (length(setdiff(extras, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(extras, rownames(installed.packages())))
}

lapply(extras, require, character.only = TRUE)

data <- read_excel("dest.xlsm")
  
summ_data <- data %>% map(function(x) tibble(behaviour = x[-length(x)], followed_by = x[-1])) %>% bind_rows()  %>% 
  group_by(behaviour, followed_by) %>% 
  filter(behaviour != followed_by) %>% 
  summarize(N = n()) %>% 
  mutate(freq = N / sum(N)) %>%
  mutate_at(c(1,2), as.numeric)


ggplot(summ_data, aes(x = behaviour, y = followed_by)) + 
  geom_point(aes(size =freq, colour=cut(summ_data$freq, c(0, 0.2, 0.4, 0.6, 0.8, 1)))) + 
  scale_y_continuous(limits = c(0.5, 8.5), breaks = seq(1, 8, by = 1)) + 
  scale_x_continuous(limits = c(0.5, 8.5), breaks = seq(1, 8, by = 1)) +
  guides(size = FALSE, color = guide_legend(override.aes = list(size=5))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), text = element_text(size=20)) +
  scale_size(range=c(2,30),breaks=seq(0.1,1.0, by = 0.2), limits=c(0,1)) +
  labs(title="Next behaviour", subtitle="Excluding itself") +
  scale_color_discrete(name = "Frequency", labels = c("0.0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8"))