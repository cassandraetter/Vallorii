## National Accounts 
# OECD Annual Household final consumption expenditure by purpose (COICOP)
## Unit of Measure - national currency


C# Process the OECD data
library(tidyverse)

coicop_data <- read_csv("~/Documents/GitHub/Vallorii/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE5_T501,1.0+A.CHE+ITA+ESP+NOR+USA+GBR+DEU+FRA+CAN+AUS.S14.....CP022+CP011+CP125+CP042+CP082+CP08+CP083+CP07+CP06+CP021+CP04+CP044+CP045.XDC.V...csv") %>%
        filter(EXPENDITURE %in% c("CP045", "CP07", "CP08", "CP04", "CP044", "CP022", "CP06", "CP022", "CP083")) %>%
        # Create category labels
        mutate(
                Category = case_when(
                        EXPENDITURE == "CP045" ~ "Electricity, gas, and other fuel",
                        EXPENDITURE == "CP07" ~ "Transport",
                        EXPENDITURE == "CP08" ~ "Communications",
                        EXPENDITURE == "CP042" ~ "Housing",
                        EXPENDITURE == "CP044" ~"Water", 
                        EXPENDITURE == "CP022" ~ "Alcohol",
                        EXPENDITURE == "CP06" ~ "Health", 
                        EXPENDITURE == "CP022" ~ "Tobacco",
                        EXPENDITURE == "CP083" ~ "Telephone Services",
                ),
                Value_Billions = OBS_VALUE * (10^(UNIT_MULT-9))
        ) %>%
        group_by(REF_AREA, Category) %>%
        mutate(
                Index = Value_Billions / first(Value_Billions) * 100
        ) %>%
        ungroup()
   
# Create index plot to see relativbe changes in spending patterns, without having to normalize currencies --- all relative to 2014 for own country. 
# Filter out health and alcohol, and improve aesthetics
min_value <- coicop_data %>% 
        filter(!Category %in% c("Health", "Alcohol")) %>%
        filter(!is.na(Category), !is.na(Index)) %>%  # Remove NAs
        pull(Index) %>%
        min() %>%
        floor() # Round down to nearest whole number

p1 <- ggplot(coicop_data %>% 
                     filter(!Category %in% c("Health", "Alcohol")) %>%
                     filter(!is.na(Category), !is.na(Index)), 
             aes(x = TIME_PERIOD, y = Index, color = Category, group = Category)) +
        geom_line(size = 1.2, alpha = 0.8) +
        facet_wrap(~REF_AREA, ncol = 5) +
        scale_y_continuous(
                labels = scales::label_number(suffix = ""),
                limits = c(min_value, max(coicop_data$Index, na.rm = TRUE) + 10),
                breaks = seq(min_value, max(coicop_data$Index, na.rm = TRUE) + 10, 
                             by = 25),
                expand = expansion(mult = c(0.02, 0.02))
        ) +
        scale_x_continuous(breaks = c(2014, 2018, 2022)) +
        scale_color_brewer(palette = "Set2") +
        geom_hline(yintercept = 100, linetype = "dashed", color = "gray70", alpha = 0.5) +
        theme_minimal() +
        theme(
                plot.title = element_text(face = "bold", size = 16, margin = margin(b = 20)),
                plot.subtitle = element_text(size = 12, color = "grey40", margin = margin(b = 20)),
                axis.title = element_text(size = 12, face = "bold"),
                axis.text = element_text(size = 9),
                axis.text.x = element_text(angle = 45, hjust = 1),
                legend.title = element_text(size = 12, face = "bold"),
                legend.text = element_text(size = 10),
                legend.position = "bottom",
                legend.box = "horizontal",
                legend.margin = margin(t = 15),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_line(color = "gray90"),
                panel.grid.major.x = element_blank(),
                strip.text = element_text(size = 11, face = "bold"),
                strip.background = element_rect(fill = "gray95", color = NA),
                plot.margin = margin(t = 25, r = 25, b = 25, l = 25),
                panel.spacing = unit(1.5, "lines")
        ) +
        labs(
                title = "Household Expenditure Growth by Category",
                subtitle = paste0("Index values (2014 = 100) showing relative change in spending patterns"),
                x = "Year",
                y = "Index Value",
                color = "Expenditure Category"
        )




ggsave("expenditure_growth.png", p1, width = 20, height = 8)


