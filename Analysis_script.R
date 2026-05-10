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











######
  #########
  ########################## Designation wise submission ######################
  library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)
library(patchwork)

male <- data.frame(
  Year      = 2014:2024,
  Assistant = c(3,  5, 12, 24, 60, 90, 296, 191, 201, 269, 203),
  Associate = c(3,  7, 18, 21, 66,117, 307, 229, 198, 217, 235),
  Professor = c(5, 11, 33, 62,129,235, 557, 464, 433, 369, 419)
) %>% mutate(Gender = "Male")

female <- data.frame(
  Year      = 2014:2024,
  Assistant = c(1,  1,  2, 13, 29, 25, 89, 65, 59, 85, 81),
  Associate = c(0,  0,  3,  7, 21, 30, 61, 56, 39, 67, 49),
  Professor = c(0,  0,  8,  4, 40, 78,134,120,102,117,114)
) %>% mutate(Gender = "Female")

bar_colors <- c(
  "Assistant Professor" = "#2166AC",
  "Associate Professor" = "#F4A582",
  "Professor"           = "#B2182B"
)
line_colors <- c(
  "Assistant Professor" = "#053061",
  "Associate Professor" = "#B35806",
  "Professor"           = "#67001F"
)

theme_journal_pub <- function(base_size = 10) {
  theme_classic(base_size = base_size, base_family = "serif") +
    theme(
    
      axis.line         = element_line(color = "black", linewidth = 0.5),
      axis.ticks        = element_line(color = "black", linewidth = 0.4),
      axis.ticks.length = unit(0.15, "cm"),
      axis.text         = element_text(color = "black", size = base_size - 1,
                                       family = "serif"),
      axis.text.x       = element_text(angle = 45, hjust = 1, vjust = 1),
      axis.title        = element_text(color = "black", size = base_size,
                                       face = "bold", family = "serif"),
      axis.title.y.left = element_text(margin = margin(r = 6)),
      axis.title.y.right= element_text(color = "grey35", size = base_size - 1,
                                       face = "bold", angle = 90,
                                       margin = margin(l = 6)),
      axis.text.y.right = element_text(color = "grey35", size = base_size - 2),
      axis.line.y.right = element_line(color = "grey60", linewidth = 0.4),
      
      
      panel.border      = element_rect(color = "black", fill = NA,
                                       linewidth = 0.6),
      panel.grid.major.y= element_line(color = "grey92", linewidth = 0.35,
                                       linetype = "solid"),
      panel.grid.major.x= element_blank(),
      panel.grid.minor  = element_blank(),
      panel.background  = element_rect(fill = "white", color = NA),
      
      legend.position   = "bottom",
      legend.box        = "horizontal",
      legend.title      = element_text(face = "bold", size = base_size - 1,
                                       family = "serif"),
      legend.text       = element_text(size = base_size - 1, family = "serif"),
      legend.key.size   = unit(0.4, "cm"),
      legend.key        = element_rect(fill = NA, color = NA),
      legend.background = element_blank(),
      legend.margin     = margin(t = 4),
      
      plot.title        = element_text(face = "bold", size = base_size + 1,
                                       hjust = 0, family = "serif",
                                       margin = margin(b = 4)),
      plot.subtitle     = element_text(size = base_size - 1, hjust = 0,
                                       color = "grey30", family = "serif",
                                       margin = margin(b = 6)),
      plot.background   = element_rect(fill = "white", color = NA),
      plot.margin       = margin(10, 12, 8, 10)
    )
}


make_panel <- function(data, panel_label, count_max, yoy_max = 12) {
  
  data <- data %>%
    mutate(Total = Assistant + Associate + Professor)
  
  df_yoy <- data %>%
    arrange(Year) %>%
    mutate(
      Asst_YoY  = round((Assistant / lag(Assistant)) * 100 / 100, 2),
      Assoc_YoY = round((Associate / lag(replace(Associate, Associate == 0, NA))) * 100 / 100, 2),
      Prof_YoY  = round((Professor / lag(replace(Professor, Professor == 0, NA))) * 100 / 100, 2)
    ) %>%
    filter(!is.na(Asst_YoY)) %>%
    pivot_longer(cols = c(Asst_YoY, Assoc_YoY, Prof_YoY),
                 names_to = "Designation", values_to = "YoY") %>%
    mutate(
      Designation = recode(Designation,
                           Asst_YoY  = "Assistant Professor",
                           Assoc_YoY = "Associate Professor",
                           Prof_YoY  = "Professor"),
      Designation = factor(Designation,
                           levels = c("Professor",
                                      "Associate Professor",
                                      "Assistant Professor")),
      Label = as.character(round(YoY, 2))
    ) %>%
    filter(!is.na(YoY) & !is.infinite(YoY) & YoY > 0 & YoY < 15)
  
  scale_yoy   <- function(x) x / yoy_max * count_max
  unscale_yoy <- function(x) x * yoy_max / count_max
  df_yoy <- df_yoy %>% mutate(YoY_scaled = scale_yoy(YoY))
  
  df_bar <- data %>%
    pivot_longer(cols = c(Assistant, Associate, Professor),
                 names_to = "Designation", values_to = "Count") %>%
    mutate(
      Designation = recode(Designation,
                           Assistant = "Assistant Professor",
                           Associate = "Associate Professor",
                           Professor = "Professor"),
      Designation = factor(Designation,
                           levels = c("Professor",
                                      "Associate Professor",
                                      "Assistant Professor"))
    )
  
  df_lbl <- data %>%
    mutate(
      Asst_y  = Assistant / 2,
      Assoc_y = Assistant + Associate / 2,
      Prof_y  = Assistant + Associate + Professor / 2
    )
  
  min_seg <- count_max * 0.04
  
  df_yoy_dot  <- df_yoy %>% filter(Year <= 2019)
  df_yoy_solid <- df_yoy %>% filter(Year >= 2019)
  
  ggplot() +
    
  
    geom_col(data = df_bar,
             aes(x = factor(Year), y = Count, fill = Designation),
             position = "stack", width = 0.65,
             color = "black", linewidth = 0.2, alpha = 0.9) +
    
    geom_text(data = df_lbl %>% filter(Assistant >= min_seg),
              aes(x = factor(Year), y = Asst_y, label = Assistant),
              color = "white", fontface = "bold",
              size = 2.5, family = "serif") +
    geom_text(data = df_lbl %>% filter(Associate >= min_seg),
              aes(x = factor(Year), y = Assoc_y, label = Associate),
              color = "grey20", fontface = "bold",
              size = 2.5, family = "serif") +
    geom_text(data = df_lbl %>% filter(Professor >= min_seg),
              aes(x = factor(Year), y = Prof_y, label = Professor),
              color = "white", fontface = "bold",
              size = 2.5, family = "serif") +

    geom_text(data = df_lbl,
              aes(x = factor(Year), y = Total + count_max * 0.025,
                  label = Total),
              color = "black", fontface = "bold",
              size = 2.6, family = "serif") +
    
    geom_line(data = df_yoy_dot,
              aes(x = factor(Year), y = YoY_scaled,
                  color = Designation, group = Designation),
              linewidth = 1.0, linetype = "dotted", lineend = "round") +
    
    geom_line(data = df_yoy_solid,
              aes(x = factor(Year), y = YoY_scaled,
                  color = Designation, group = Designation),
              linewidth = 1.0, linetype = "solid", lineend = "round") +
    
    geom_point(data = df_yoy,
               aes(x = factor(Year), y = YoY_scaled,
                   color = Designation, shape = Designation),
               size = 2.8, stroke = 1.0, fill = "white") +
    
    geom_text(data = df_yoy,
              aes(x = factor(Year), y = YoY_scaled,
                  color = Designation, label = Label),
              vjust = -0.9, size = 2.2,
              family = "serif", fontface = "bold",
              show.legend = FALSE) +
    
    scale_fill_manual(values = bar_colors,    name = "Designation (Bar)") +
    scale_color_manual(values = line_colors,  name = "YoY Ratio (Line)") +
    scale_shape_manual(values = c(21, 22, 23),name = "YoY Ratio (Line)") +
    
    scale_y_continuous(
      name   = "Number of Authors",
      limits = c(0, count_max * 1.1),
      breaks = seq(0, count_max, ifelse(count_max > 500, 200, 50)),
      labels = comma,
      expand = c(0, 0),
      sec.axis = sec_axis(
        transform = ~ unscale_yoy(.),
        name      = "YoY Growth Ratio",
        breaks    = seq(0, yoy_max, 2),
        labels    = function(x) x
      )
    ) +
    
    scale_x_discrete(name = NULL) +
    labs(title = panel_label) +
    theme_journal_pub() +
    
    guides(
      fill  = guide_legend(order = 1, nrow = 1,
                           override.aes = list(alpha = 0.9, color = "black",
                                               linewidth = 0.3)),
      color = guide_legend(order = 2, nrow = 1,
                           override.aes = list(linewidth = 1.2)),
      shape = guide_legend(order = 2, nrow = 1)
    )
}

p_male   <- make_panel(male,   "(a)  Male Authors",   count_max = 1300)
p_female <- make_panel(female, "(b)  Female Authors", count_max = 350)
final <- (p_male | p_female) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.box      = "horizontal")

final <- final +
  plot_annotation(
    title   = "Temporal Distribution of Authors by Academic Designation and Gender (2014–2024)",
    caption = "YoY Ratio = Current year count ÷ Previous year count. Values > 1 indicate growth; < 1 indicate decline.",
    theme   = theme(
      plot.title   = element_text(face = "bold", size = 12, hjust = 0.5,
                                  family = "serif", margin = margin(b = 6)),
      plot.caption = element_text(size = 8, color = "grey40", hjust = 0,
                                  family = "serif", margin = margin(t = 6)),
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

ggsave("designation_journal_pub.png",
       plot   = final,
       width  = 16,
       height = 7,
       dpi    = 600,
       bg     = "white")

cat("✓ Saved: designation_journal_pub.png (600 dpi — journal quality)\n")


ggsave("designation_journal_pub.tiff",
       plot   = final,
       width  = 16,
       height = 7,
       dpi    = 600,
       compression = "lzw",
       bg     = "white")

cat("designation.tiff (600 dpi, LZW compressed)\n")



male %>%
  mutate(Total     = Assistant + Associate + Professor,
         Asst_YoY  = round((Assistant / lag(Assistant)) * 100 / 100, 2),
         Assoc_YoY = round((Associate / lag(Associate)) * 100 / 100, 2),
         Prof_YoY  = round((Professor / lag(Professor)) * 100 / 100, 2)) %>%
  select(Year, Total, Assistant, Asst_YoY,
         Associate, Assoc_YoY, Professor, Prof_YoY) %>%
  as.data.frame() %>% print()

cat("\n── Female ───────────────────────────────────────────\n")
female %>%
  mutate(Total     = Assistant + Associate + Professor,
         Asst_YoY  = round((Assistant / lag(Assistant)) * 100 / 100, 2),
         Assoc_YoY = round((Associate / lag(Associate)) * 100 / 100, 2),
         Prof_YoY  = round((Professor / lag(Professor)) * 100 / 100, 2)) %>%
  select(Year, Total, Assistant, Asst_YoY,
         Associate, Assoc_YoY, Professor, Prof_YoY) %>%
  as.data.frame() %>% print()v






#####
###################license_data
library(ggplot2)
library(patchwork)
biorxiv <- data.frame(
  license = c("cc_no", "cc_by_nc_nd", "cc_by", "cc_by_nd", "cc_by_nc", "0", "cc0", "cc0_ng"),
  count   = c(2644, 2189, 690, 375, 274, 14, 9, 3)
)
medrxiv <- data.frame(
  license = c("cc_by_nc_nd", "cc_no", "cc_by_nd", "cc_by", "cc_by_nc", "cc0_ng", "cc0"),
  count   = c(582, 459, 166, 158, 91, 6, 3)
)
palette <- c(
  "cc_no"       = "#2166ac",
  "cc_by_nc_nd" = "#1a7a3a",
  "cc_by"       = "#8b0000",
  "cc_by_nd"    = "#d6604d",
  "cc_by_nc"    = "#f4a582",
  "0"           = "#d1cce6",
  "cc0"         = "#e8e4f0",
  "cc0_ng"      = "#b0b0b0"
)

make_pie <- function(df, title) {
  total  <- sum(df$count)
  df$pct <- df$count / total * 100
  df$legend_label <- paste0(df$license, " — ", df$count, " (", round(df$pct, 1), "%)")
  df$license <- factor(df$license, levels = df$license)

  ggplot(df, aes(x = "", y = count, fill = license)) +
    geom_col(width = 1, color = "white", linewidth = 0.5) +
    coord_polar(theta = "y", start = 0) +
    scale_fill_manual(
      values = palette,
      labels = df$legend_label,
      name   = "License Type"
    ) +
    geom_text(
      aes(label = paste0(round(pct, 1), "%")),
      position = position_stack(vjust = 0.5),
      color = "white", size = 2.5,       
      fontface = "bold",
      data = function(x) subset(x, pct >= 4)
    ) +
    labs(
      title    = title,
      subtitle = paste0("Total = ", format(total, big.mark = ","))
    ) +
    theme_void() +
    theme(
      plot.title      = element_text(face = "bold", size = 13, hjust = 0.5),
      plot.subtitle   = element_text(color = "gray50", size = 10, hjust = 0.5),
      legend.title    = element_text(face = "bold", size = 9),
      legend.text     = element_text(size = 8),
      legend.key.size = unit(0.45, "cm"),
      plot.margin     = margin(10, 10, 10, 10)
    )
}

p1 <- make_pie(biorxiv, "bioRxiv — Submission License Types")
p2 <- make_pie(medrxiv, "medRxiv — Submission License Types")

p1 + p2 + plot_layout(widths = c(1, 1))






#########
################
################################################ License pie chart all regions####################

library(ggplot2)
library(dplyr)
library(scales)

df <- data.frame(
  Country = rep(c("India", "China", "USA", "Europe"), each = 8),
  License = rep(c("blank/other", "cc_by", "cc_by_nc", "cc_by_nc_nd",
                  "cc_by_nd", "cc_no", "cc0", "cc0_ng"), times = 4),
  Pct = c(
    
    0,    11.0,  0,    36.0,   7.0,  40.4,  0,    0,
    
    0,    15.2,  7.0,  33.3,   0,    43.0,  0,    0,

    0,    15.1,  7.5,  37.5,   6.0,  31.8,  0,    0,
    
    1.0,  21.2,  7.5,  37.0,   6.2,  27.3,  0,    0
  )
)

df <- df %>%
  mutate(
    Ring = case_when(
      Country == "India"  ~ 1,
      Country == "China"  ~ 2,
      Country == "USA"    ~ 3,
      Country == "Europe" ~ 4
    ),
    Country = factor(Country, levels = c("India","China","USA","Europe"))
  ) %>%
  filter(Pct > 0)  

license_colors <- c(
  "blank/other" = "#B0BEC5",   
  "cc_by"       = "#43A047",   
  "cc_by_nc"    = "#1E88E5",  
  "cc_by_nc_nd" = "#FB8C00",   
  "cc_by_nd"    = "#8E24AA",   
  "cc_no"       = "#E53935",   
  "cc0"         = "#FDD835",   
  "cc0_ng"      = "#00ACC1"    
)

ring_inner <- c(1.5, 2.6, 3.7, 4.8)   
ring_outer <- c(2.4, 3.5, 4.6, 5.7) 
ring_gap   <- 0.15                      

arc_data <- list()

for (i in 1:4) {
  country_name <- c("India","China","USA","Europe")[i]
  sub <- df %>% filter(Country == country_name) %>%
    arrange(License)

  sub <- sub %>%
    mutate(
      Frac     = Pct / sum(Pct),
      AngleEnd = cumsum(Frac) * 2 * pi,
      AngleStart = lag(AngleEnd, default = 0)
    )

  for (j in seq_len(nrow(sub))) {
    angles <- seq(sub$AngleStart[j], sub$AngleEnd[j], length.out = 50)
    r_in   <- ring_inner[i] + ring_gap / 2
    r_out  <- ring_outer[i] - ring_gap / 2

    seg_df <- data.frame(
      x       = c(r_in  * cos(angles), rev(r_out * cos(angles))),
      y       = c(r_in  * sin(angles), rev(r_out * sin(angles))),
      License = sub$License[j],
      Country = country_name,
      Pct     = sub$Pct[j],
      Ring    = i,
      Group   = paste0(country_name, "_", sub$License[j])
    )
    arc_data[[length(arc_data) + 1]] <- seg_df
  }
}

arc_df <- bind_rows(arc_data) %>%
  mutate(License = factor(License, levels = names(license_colors)))

label_data <- list()

for (i in 1:4) {
  country_name <- c("India","China","USA","Europe")[i]
  sub <- df %>% filter(Country == country_name) %>%
    arrange(License) %>%
    mutate(
      Frac       = Pct / sum(Pct),
      AngleEnd   = cumsum(Frac) * 2 * pi,
      AngleStart = lag(AngleEnd, default = 0),
      AngleMid   = (AngleStart + AngleEnd) / 2,
      r_mid      = (ring_inner[i] + ring_outer[i]) / 2,
      lx         = r_mid * cos(AngleMid),
      ly         = r_mid * sin(AngleMid),
      Label      = paste0(Pct, "%")
    ) %>%
    filter(Pct >= 5)  

  label_data[[i]] <- sub
}

label_df <- bind_rows(label_data)

country_labels <- data.frame(
  Country = c("India","China","USA","Europe"),
  r       = ring_outer + 0.25,
  angle   = pi / 2   
) %>%
  mutate(
    r     = c(ring_outer[1]+0.25, ring_outer[2]+0.25,
              ring_outer[3]+0.25, ring_outer[4]+0.25),
    lx    = r * cos(angle - 0.05),
    ly    = r * sin(angle - 0.05)
  )

p <- ggplot() +


  geom_polygon(data = arc_df,
               aes(x = x, y = y, group = Group, fill = License),
               color = "white", linewidth = 0.4) +


  geom_text(data = label_df,
            aes(x = lx, y = ly, label = Label),
            color = "white", fontface = "bold",
            size = 3.2, family = "serif") +


  geom_text(data = country_labels,
            aes(x = lx, y = ly, label = Country),
            color = "black", fontface = "bold",
            size = 4.0, family = "serif", hjust = 0) +

 
  scale_fill_manual(values = license_colors, name = "License type") +


  coord_equal() +


  xlim(-6.5, 8) +
  ylim(-6.5, 6.5) +

  labs(
    title   = "License Type Distribution by Country",
    caption = "Source: bioRxiv & medRxiv"
  ) +

  theme_void(base_family = "serif") +
  theme(
    plot.title      = element_text(face = "bold", size = 14, hjust = 0.5,
                                   margin = margin(b = 10)),
    plot.caption    = element_text(size = 9, color = "grey50",
                                   hjust = 0.5, margin = margin(t = 10)),
    legend.position = "right",
    legend.title    = element_text(face = "bold", size = 11),
    legend.text     = element_text(size = 10),
    legend.key.size = unit(0.5, "cm"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin     = margin(15, 10, 15, 10)
  )


ggsave("license_donut_chart.png",
       plot   = p,
       width  = 10,
       height = 8,
       dpi    = 300,
       bg     = "white")

cat("✓ Saved: license_donut_chart.png\n")

cat("\n── License % by Country ─────────────────────────────\n")
df %>%
  select(Country, License, Pct) %>%
  tidyr::pivot_wider(names_from = Country, values_from = Pct, values_fill = 0) %>%
  as.data.frame() %>%
  print()
