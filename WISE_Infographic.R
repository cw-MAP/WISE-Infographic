
##################################################
##                                              ##
##        Water Insecurity Experiences          ##    
##                (WISE) Scales                 ##
##                                              ##
##          PDF Infographic Generator           ##
##                                              ##
##                                              ##
##            Author: Scott Miller              ##
##        scott.miller@charitywater.org         ##
##                                              ##
##           Last Updated: 9/27/2024            ##
##                                              ##
##################################################

# Copyright (c) 2024 Scott M. Miller 
# This code is licensed under the MIT License.
# For full license text, see the LICENSE file in the repository root.


# ----------------------------------
# Install relevant packages
# ----------------------------------

# install.packages(c("readxl", "dplyr", "data.table", "ggplot2", "grid"))        #Note: Remove the '#' at the start of this line to install packages
library(readxl)
library(dplyr)
library(data.table)
library(ggplot2)
library(grid)
library(tidyverse)
library(htmltools)


# ----------------------------------
# Set Directory & Import Data
# ----------------------------------

# Set directory: this needs to match the folder where WISE data is stored
setwd("/Users/scott.miller/Documents/GitHub/WISE-Infographic/")

# Import Data: this needs to match the data set stored in the directory folder
data <- read_excel("[Example] Mozambique HWISE Baseline Data.xlsx")


# ----------------------------------
# Define Background Details for Automatic Text Generation
# ----------------------------------

text_scale <- "Household (HWISE)"   # Enter the WISE scale (e.g. HWISE, iWISE, etc.)
text_recallperiod <- "4 weeks"
text_country <- "Mozambique"    # Enter country name
text_survey <- "Baseline Survey"    # Enter brief survey description
text_collectedby <- "[Organization]"   # Enter name of organization that collected this data
text_timing <- "November 2023 (Baseline Survey)"  # Enter the timing of data collection 
text_geography <- "Project implementation areas within Memba, Larde, Erati, and Moma Districts"     # Describe the geographic coverage of the surveys
text_samplesize <- "437 Households. Two-stage simple random sampling of (i) communities and (ii) households."

# ----------------------------------
# Data Cleaning & Naming
# ----------------------------------

# create an object with all WISE scale variable names
names <- c("WISE_Worry", "WISE_Angry", "WISE_Interrupt", "WISE_Clothes", "WISE_Plans",
           "WISE_Food", "WISE_Hands", "WISE_Body", "WISE_Drink", "WISE_Sleep",
           "WISE_None", "WISE_Shame")

# create a new variable `WISE_[item]_n' for each variable based on numerical codes
for (name in names) {
    data <- data %>%
        mutate(!!paste0(name, "_n") := case_when(
            .data[[name]] == "Never" ~ 0,
            .data[[name]] == "Rarely (1–2 times in the last 4 weeks)" ~ 1,
            .data[[name]] == "Sometimes (3–10 times in the last 4 weeks)" ~ 2,
            .data[[name]] == "Often (10-20 times in last 4 weeks)" ~ 3,
            .data[[name]] == "Always (More than 20 times in last 4 weeks)" ~ 3,
            TRUE ~ NA_real_
        ))
}


# calculate each observation's overall WISE score
data$WISE_Score <- rowSums(data[, grep("_n$", names(data))], na.rm = F)

# create 'Insecurity' variables for binary (>=12) and level cutoffs
data <- data %>%
    mutate(
        Insecurity_binary = case_when(
            !is.na(WISE_Score) & WISE_Score <= 11 ~ "Not Water Insecure",
            !is.na(WISE_Score) & WISE_Score >= 12 ~ "Water Insecure",
            TRUE ~ as.character(NA)
        ),
        Insecurity_level = case_when(
            !is.na(WISE_Score) & WISE_Score <= 2 ~ "No-to-marginal",
            !is.na(WISE_Score) & WISE_Score <= 11 ~ "Low",
            !is.na(WISE_Score) & WISE_Score <= 23 ~ "Moderate",
            !is.na(WISE_Score) & WISE_Score >= 24 ~ "High",
            TRUE ~ as.character(NA)
        )
    )

# define color palette for charts
custom_colors <- c("#ff0000", "#ff7600", "#ffd000", "#0076c3")
insecurity_order <- c("High", "Moderate", "Low", "No-to-marginal")



# ----------------------------------
# WISE Analysis
# ----------------------------------

#-----------------------
## HWISE Classification
#-----------------------

# save the percentage of insecure responses
pct_insecure <- paste(round(prop.table(table(as.character(data$Insecurity_binary)))[2]*100, 1), "%", sep = "")
pct_high <- paste(round(prop.table(table(as.character(data$Insecurity_level)))[1]*100, 1), "%", sep = "")
pct_moderate <- paste(round(prop.table(table(as.character(data$Insecurity_level)))[2]*100, 1), "%", sep = "")
pct_low <- paste(round(prop.table(table(as.character(data$Insecurity_level)))[3]*100, 1), "%", sep = "")
pct_notomarginal <- paste(round(prop.table(table(as.character(data$Insecurity_level)))[4]*100, 1), "%", sep = "")

# Reshape data to long format, remove NAs, and order the response factor in reverse
long_data <- data[, grep("_n$", names(data))] %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "response") %>%
    filter(!is.na(response)) %>%  # Remove NA values
    mutate(response = factor(response, levels = c("3", "2", "1", "0")))  # Reverse order here

# Calculate percentages for each response category, excluding NAs from total
percentage_data <- long_data %>%
    group_by(variable, response) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(variable) %>%
    mutate(percentage = count / sum(count) * 100)

# Create and store stacked bar graph
Item_Prevalence <- ggplot(percentage_data, aes(x = variable, y = percentage, fill = response)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 101)) +
    coord_flip() +
    labs(x = "", y = "", title = "") +
    theme(
        legend.position = "bottom",
        axis.text.y = element_text(hjust = 1)
    ) +  
    guides(color = guide_legend(nrow = 1)) +
    theme(
        legend.position = c(0.3, -0.08),  # Adjust these values to move the legend
        legend.justification = c(0.5, 0.5),
        legend.direction = "horizontal",
        legend.box = "horizontal",
    ) +
    geom_text(aes(label = ifelse(percentage >= 8, paste0(round(percentage, 0), "%"), "")),
              position = position_stack(vjust = 0.5), color = "white", size = 3) +
    scale_fill_manual(
        values = c("3" = "#ff0000", "2" = "#ff7600", "1" = "#ffd000", "0" = "#0076c3"),
        breaks = c("3", "2", "1", "0"),
        labels = c("0" = "Never", "1" = "Rarely", "2" = "Sometimes", "3" = "Often/Always"),
        name = ""
    ) +
    scale_x_discrete(labels = c(
        "WISE_Worry_n" = "Worry about water",
        "WISE_Angry_n" = "Felt angry about water",
        "WISE_Interrupt_n" = "Had supply interruptions",
        "WISE_Clothes_n" = "Could not wash clothes",
        "WISE_Plans_n" = "Had to change plans",
        "WISE_Food_n" = "Had to eat differently",
        "WISE_Hands_n" = "Could not wash hands",
        "WISE_Body_n" = "Could not wash body",
        "WISE_Drink_n" = "Had no water to drink",
        "WISE_Sleep_n" = "Went to bed thirsty",
        "WISE_None_n" = "Had no water at all",
        "WISE_Shame_n" = "Felt shame about water"
    )) +
    guides(fill = guide_legend(reverse = TRUE))




#-----------------------
## Household Size Decomposition
#-----------------------

# Group household size into categories
data$HHsize_group <- cut(data$`Household Size`, breaks = c(0, 5, 7, 10, Inf),
                    labels = c("<5", "5-7", "8-10", "11+"),
                    right = FALSE)


# Calculate the percentage for each Insecurity_level within each HHsize_group, excluding NAs
hhsize_data <- data %>%
    filter(!is.na(Insecurity_level) & !is.na(HHsize_group)) %>%  # Filter out NAs
    mutate(Insecurity_level = factor(Insecurity_level, levels = insecurity_order)) %>%  # Set the order
    group_by(HHsize_group, Insecurity_level) %>%
    summarize(count = n(), .groups = "drop") %>%
    group_by(HHsize_group) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    arrange(HHsize_group, Insecurity_level)  # Ensure the result is sorted

# Create and store stacked bar graph
Insecurity_by_HHsize <- ggplot(hhsize_data, aes(x = HHsize_group, y = percentage, fill = Insecurity_level)) +
    geom_bar(stat = "identity") +
    labs(x = "Household Size", y = "Percentage") +
    geom_text(data = . %>% filter(percentage >= 8),  # Only label percentages >= 8% (otherwise bars are too small for text)
              aes(label = paste(round(percentage, 0), "%", sep = "")),
              position = position_stack(vjust = 0.5), color = "white", size = 3) +
    theme_minimal() +
    scale_fill_manual(values = custom_colors,
                      labels = c("High", 
                                 "Moderate", 
                                 "Low", 
                                 "No-to-marginal")) +  # You can change the color palette as needed
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Format y-axis as percentages
    theme(
        axis.text.x = element_text(hjust = 1),  # Rotate x-axis labels for better readability
        legend.position = "none",  # Move legend to bottom
    ) + 
    guides(fill = guide_legend(reverse = TRUE))


#-----------------------
## District Decomposition
#-----------------------


# Calculate the percentage for each Insecurity_level within each HHsize_group, excluding NAs
district_data <- data %>%
    filter(!is.na(Insecurity_level) & !is.na(District)) %>%  # Filter out NAs
    mutate(Insecurity_level = factor(Insecurity_level, levels = insecurity_order)) %>%  # Set the order
    group_by(District, Insecurity_level) %>%
    summarize(count = n(), .groups = "drop") %>%
    group_by(District) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    arrange(District, Insecurity_level)  # Ensure the result is sorted

# Create and store stacked bar graph
Insecurity_by_District <- ggplot(district_data, aes(x = District, y = percentage, fill = Insecurity_level)) +
    geom_bar(stat = "identity") +
    labs(x = "District", y = "Percentage", fill = "") +
    geom_text(data = . %>% filter(percentage >= 8),  # Only label percentages >= 8% (otherwise bars are too small for text)
              aes(label = paste(round(percentage, 0), "%", sep = "")),
              position = position_stack(vjust = 0.5), color = "white", size = 3) +
    theme_minimal() +
    scale_fill_manual(values = custom_colors,
                      labels = c("High", 
                                 "Moderate", 
                                 "Low", 
                                 "No-to-marginal")) +  # You can change the color palette as needed
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Format y-axis as percentages
    theme(
        axis.text.x = element_text(hjust = 1),  # Rotate x-axis labels for better readability
        legend.position = "bottom",  # Move legend to bottom
    ) +
    guides(fill = guide_legend(reverse = TRUE))


# ------------------------------------------------------------------------------
# Generate Infographic in PDF format
# ------------------------------------------------------------------------------


#-----------------------
# Create PDF & grid layout
#-----------------------

pdf("Infographic.pdf", width = 10, height = 14)

grid.newpage() 
pushViewport(viewport(layout = grid.layout(9, 4)))
grid.rect(gp = gpar(fill = "white", col = "white"))


#-----------------------
# Header - white background with Title
#-----------------------

grid.rect(gp = gpar(fill = "white", col = "white", alpha = 0.6), 
          x = unit(0.5, "npc"), 
          y = unit(0.95, "npc"), 
          width = unit(1, "npc"), 
          height = unit(0.2285, "npc"))

grid.text("Water Insecurity Experiences (WISE) Scales", 
          y = unit(0.98, "npc"), 
          x = unit(0.5, "npc"), 
          vjust = 3, hjust = .5, 
          gp = gpar(col = "#0076c3", cex = 2.4, alpha = 1))

grid.text(paste(text_country, text_survey, sep = " "), 
          y = unit(0.88, "npc"), 
          gp = gpar(col = "#0076c3", cex = 2.2))


#-----------------------
# Subheader - Blue background with data details
#-----------------------

grid.rect(gp = gpar(fill = "#0076c3", col = "#0076c3", alpha = 0.57), 
          x = unit(0.5, "npc"), 
          y = unit(0.785, "npc"), 
          width = unit(1, "npc"), 
          height = unit(0.115, "npc"))

grid.text("", 
          y = unit(0.78, "npc"), 
          x = unit(0.5, "npc"), 
          vjust = .5, hjust = .5, 
          gp = gpar(col = "#CA8B01", cex = 13, alpha = 0.3))

grid.text(paste(
    "",
    "Scale:",
    "Recall Period:",
    "Collected By:",
    "Collection Timing:",
    "Geographic Coverage:",
    "Sample Size & Details:",
    "",
    "", sep = "\n"), vjust = 0, hjust = 0, 
    x = unit(0.05, "npc"), 
    y = unit(0.705, "npc"), 
    gp = gpar(col = "Black", cex = 1, fontface = "bold"))

grid.text(paste(
    "",
    text_scale,
    text_recallperiod,
    text_collectedby,
    text_timing,
    text_geography,
    text_samplesize,
    "",
    "",sep = "\n"), vjust = 0, hjust = 0, 
    x = unit(0.27, "npc"), 
    y = unit(0.705, "npc"), 
    gp = gpar(col = "Black", cex = 1))


#-----------------------
# Top left paragraph (needs to be manually updated)
#-----------------------

grid.text(paste(
    "charity: water and its local partners",
    sep = "\n"), vjust = 0, hjust = 0, 
    x = unit(0.05, "npc"), 
    y = unit(0.68, "npc"), 
    gp = gpar(col = "Black", cex = 1.3, fontface = "bold"))


grid.text(paste(
    "",
    "are working to estimate experiences with water access and",
    "use around the world. Currently, 58 organizations across",
    "22 countries in Africa and Asia are measuring water",
    "insecurity in their program areas through charity: water's",
    "monitoring & evaluation framework.",
    "",
    sep = "\n"), vjust = 0, hjust = 0, 
    x = unit(0.05, "npc"), 
    y = unit(0.575, "npc"), 
    gp = gpar(col = "Black", cex = 1))


#-----------------------
# Center left - paragraph
#-----------------------

grid.text(paste(
    paste("Who is water insecure in ",text_country, "?", sep = ""),
    "", sep = "\n"), vjust = 0, hjust = 0, 
    x = unit(0.05, "npc"), 
    y = unit(0.535, "npc"), 
    gp = gpar(col = "Black", cex = 1.3, fontface = "bold"))


grid.text(paste(
    pct_insecure,
    "", sep = "\n"), vjust = 0, hjust = 0, 
    x = unit(0.052, "npc"), 
    y = unit(0.5075, "npc"), 
    gp = gpar(col = "#0076c3", cex = 1.5, fontface = "bold"))

grid.text(paste(
    "                   of households participating in the survey exper-", "\n",
    "ienced moderate-to-high water insecurity in 2023. ", pct_high, "\n",
    "of respondents' water insecurity experiences were classified", "\n",
    "as 'high,' while ", pct_moderate, " of respondents were 'moderately'", "\n",
    "insecure. ", pct_low, " were considered to have 'low' insecurity,", "\n",
    "while ", pct_notomarginal, " experienced no-to-marginal water insecurity.",
    sep = ""), vjust = 0, hjust = 0, 
    x = unit(0.05, "npc"), 
    y = unit(0.45, "npc"), 
    gp = gpar(col = "Black", cex = 1))

#-----------------------
# Center left - Insecurity by HH size graph
#-----------------------

pushViewport(viewport(layout.pos.row = 6:7, layout.pos.col = 1:2))
print(Insecurity_by_HHsize, newpage = FALSE)
popViewport()


#-----------------------
# Bottom left - Insecurity by District graph
#-----------------------

pushViewport(viewport(layout.pos.row = 8:9, layout.pos.col = 1:2))
print(Insecurity_by_District, newpage = FALSE)
popViewport()


#-----------------------
# Top right paragraph
#-----------------------

grid.text(paste(
    "How did we measure water insecurity?",
    "", sep = "\n"), vjust = 0, hjust = 0, 
    x = unit(0.53, "npc"), 
    y = unit(0.66, "npc"), 
    gp = gpar(col = "Black", cex = 1.3, fontface = "bold"))

grid.text(paste(
    "Most indicators measure water availability or",
    "infrastructure. These don’t tell us about people’s",
    "ability to reliably access or use water or how water",
    "insecurity varies by gender, age, etc. Which means we",
    "haven’t known exactly who is left behind… until now.",
    sep = "\n"), vjust = 0, hjust = 0, 
    x = unit(0.53, "npc"), 
    y = unit(0.592, "npc"), 
    gp = gpar(col = "Black", cex = 1))


#-----------------------
# Center right paragraph
#-----------------------

grid.text(paste(
    "How does water insecurity manifest",
    "in this program area?", 
    sep = "\n"), vjust = 0, hjust = 0, 
    x = unit(0.53, "npc"), 
    y = unit(0.53, "npc"), 
    gp = gpar(col = "Black", cex = 1.3, fontface = "bold"))

grid.text(paste(
    "We used the Household Water InSecurity Experiences",
    "(HWISE) Scale to measure individual experiences with",
    "water access and use. Respondents had the following",
    "negative experiences due to water problems in the",
    "last month.",
    sep = "\n"), vjust = 0, hjust = 0, 
    x = unit(0.53, "npc"), 
    y = unit(0.44, "npc"), 
    gp = gpar(col = "Black", cex = 1))


#-----------------------
# Center right - Item Prevalence Graph
#-----------------------

pushViewport(viewport(layout.pos.row = 6:8, layout.pos.col = 3:4))
print(Item_Prevalence, newpage = FALSE)
popViewport()



grid.text(paste(
    "These data provide insights on prevalence and severity",
    "of water insecurity that can guide policymaking,",
    "including resource allocation. The WISE Scales will also",
    "be used to measure the impact of interventions, and",
    "monitor progress and accountability.",
    sep = "\n"), vjust = 0, hjust = 0, 
    x = unit(0.53, "npc"), 
    y = unit(0.02, "npc"), 
    gp = gpar(col = "Black", cex = 1))

dev.off()

