### Indian preprint submission trend bio+MedarXiv#####
## bioRxiv and MedarXiv preprint submission growth trend
library(ggplot2)
library(dplyr)


biorxiv_data <- data.frame(
  Year = 2013:2024,
  Submissions = c(1, 12, 25, 88, 150, 386, 620, 1007, 1000, 930, 1011, 1013),
  Server = "bioRxiv"
)

medrxiv_data <- data.frame(
  Year = 2019:2024,
  Submissions = c(21, 567, 257, 183, 237, 200),
  Server = "medRxiv"
)


df <- bind_rows(biorxiv_data, medrxiv_data)


ggplot(df, aes(x = Year, y = Submissions, fill = Server)) +

  annotate("rect", xmin = 2019.6, xmax = 2020.4, ymin = 0, ymax = 1100, 
           fill = "yellow", alpha = 0.1, color = "black", linetype = "dotted") +

  annotate("text", x = 2020, y = 1130, label = "COVID-19", 
           fontface = "italic", size = 3.5) +

  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.6) +
  
  geom_text(aes(label = Submissions), 
            position = position_dodge(width = 0.6), 
            vjust = -0.5, size = 2.8) +

  scale_fill_manual(values = c("bioRxiv" = "#4A79B7", "medRxiv" = "#B74A4A")) +
 
  scale_x_continuous(breaks = 2013:2024) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, 1200)) +
  labs(
    title = "Preprint Submissions: bioRxiv vs medRxiv (2013–2024)",
    x = "Year",
    y = "Number of Preprints",
    fill = "Preprint Server"
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    legend.justification = "center",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12, margin = margin(b = 20)),
    axis.line = element_line(color = "black"),
    panel.grid.major.y = element_line(color = "grey95")
  )


################
#######################
###############################Script for Reagion wise preprint submission count 2013 - 2024 (USA,Europe,India,China)##########################

library(ggplot2)
library(dplyr)
library(tidyr)

regional_data <- data.frame(
  Year = 2013:2024,
  USA = c(36, 319, 582, 1525, 3716, 6551, 9624, 15453, 15629, 15015, 16849, 10540),
  India = c(1, 12, 25, 88, 150, 386, 641, 1574, 1257, 1113, 1248, 1213),
  China = c(3, 13, 43, 96, 284, 872, 1294, 3102, 2323, 2382, 2781, 1877),
  Europe = c(15, 204, 435, 1183, 3092, 6004, 9343, 16293, 16456, 15376, 16500, 10359)
)

df_long <- regional_data %>%
  pivot_longer(cols = -Year, names_to = "Region", values_to = "Submissions")

df_long$Region <- factor(df_long$Region, levels = c("India", "China", "USA", "Europe"))

bar_width <- 0.6
dodge_width <- 0.7

ggplot(df_long, aes(x = Year, y = Submissions, fill = Region)) +

  annotate("rect", xmin = 2019.5, xmax = 2020.5, ymin = 0, ymax = 19000, 
           fill = "yellow", alpha = 0.1, color = "black", linetype = "dotted") +

  annotate("text", x = 2020, y = 19500, label = "COVID-19", 
           fontface = "italic", size = 3.5) +

  geom_bar(stat = "identity", position = position_dodge(width = dodge_width), width = bar_width) +

  geom_text(aes(label = scales::comma(Submissions)), 
            position = position_dodge(width = dodge_width), 
            angle = 90, 
            hjust = -0.1,  
            vjust = 0.5,   
            size = 1.8) +  

  scale_fill_manual(values = c(
    "India"  = "#E68A5C", 
    "China"  = "#C75D5D", 
    "USA"    = "#4A79B7", 
    "Europe" = "#79B74A"
  )) +

  scale_x_continuous(breaks = 2013:2024) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 22000)) + 
  labs(
    title = "Regional Preprint Submissions on bioRxiv (2013–2024)",
    x = "Year",
    y = "Number of Preprints",
    fill = "Region"
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    legend.justification = "center",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.line = element_line(color = "black"),
    panel.grid.major.y = element_line(color = "grey95")
  )


################
#######################
###############################Script for Reagion wise preprint submission count percentage 2013 - 2024 (USA,Europe,India,China)##########################

library(ggplot2)
library(dplyr)
library(tidyr)

pct_data <- data.frame(
  Year = 2013:2024,
  USA = c(46.75, 40.23, 36.65, 36.61, 36.24, 33.31, 33.65, 30.24, 31.39, 32.42, 32.94, 32.89),
  India = c(1.30, 1.51, 1.57, 2.11, 1.46, 1.96, 2.24, 3.08, 2.53, 2.40, 2.44, 3.79),
  China = c(3.90, 1.64, 2.71, 2.30, 2.77, 4.43, 4.52, 6.07, 4.67, 5.14, 5.44, 5.86),
  Europe = c(19.48, 25.73, 27.39, 28.40, 30.16, 30.53, 32.67, 31.88, 33.06, 33.20, 32.26, 32.33)
)

df_long <- pct_data %>%
  pivot_longer(cols = -Year, names_to = "Region", values_to = "Percentage")

df_long$Region <- factor(df_long$Region, levels = c("India", "China", "USA", "Europe"))

ggplot(df_long, aes(x = Year, y = Percentage, fill = Region)) +

  annotate("rect", xmin = 2019.5, xmax = 2020.5, ymin = 0, ymax = 50, 
           fill = "yellow", alpha = 0.1, color = "black", linetype = "dotted") +
  annotate("text", x = 2020, y = 52, label = "COVID-19", 
           fontface = "italic", size = 3) +

  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +

  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_dodge(width = 0.7), 
            angle = 90, hjust = -0.2, size = 2) +

  scale_fill_manual(values = c(
    "India"  = "#E68A5C", 
    "China"  = "#C75D5D", 
    "USA"    = "#4A79B7", 
    "Europe" = "#79B74A"
  )) +

  scale_x_continuous(breaks = 2013:2024) +
  scale_y_continuous(limits = c(0, 55)) +
  labs(
    title = "Growth Trend: Regional Percentage Share of Global Submissions",
    x = "Year",
    y = "Percentage Share (%)",
    fill = "Region"
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    legend.justification = "center",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.line = element_line(color = "black")
  )
