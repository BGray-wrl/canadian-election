

library(ggplot2)
library(dplyr)
library(tidyr)
library(shadowtext)

# Reshape data for plotting
make_vis_plot_lasso<-function(data, span=0.75, cuttoff_date=as.Date("2024-06-01")){
  cad_long <- data %>%
    pivot_longer(cols = c(LPC, CPC, NDP, GPC, BQ, PPC), 
                 names_to = "Party", values_to = "Support") %>%
    filter(date >= cuttoff_date)%>%
    mutate(Weight = grade_weight*size_weight/2)
  
  # Define event dates
  tariff_delay_date <- as.Date("2025-02-03")
  trudeau_resignation_date <- as.Date("2025-01-06")
  label_date <- max(data$date)
  
  # Extract the values for labels at the given date
  smoothed_values <- cad_long %>%
    group_by(Party) %>%
    summarize(Support = predict(loess(Support ~ as.numeric(date), span = span), newdata = data.frame(date = label_date)))
  
  labels_data <- smoothed_values %>%
    mutate(date = label_date)
  
  # Plot with ggplot2
  ggplot(cad_long, aes(x = date, y = Support, color = Party)) +
    geom_point(alpha = 0.6,aes(size=Weight)) +  # Add points (scatter)
    geom_smooth(linewidth = 1.2,span=span,aes(weight=Weight)) +  # Add smoothed trend lines
    scale_color_manual(values = c("LPC" = "red", "CPC" = "blue", 
                                  "NDP" = "orange", "GPC" = "green", 
                                  "BQ" = "lightblue", "PPC" = "purple")) + 
    geom_vline(xintercept = as.numeric(tariff_delay_date), linetype = "dashed", color = "black") +
    geom_vline(xintercept = as.numeric(trudeau_resignation_date), linetype = "dashed", color = "black") +
    annotate("shadowtext", x = tariff_delay_date, y = Inf, label = "Tariff Delay",
             angle = 90, vjust = 1.5, hjust = 1.1, color = "black", bg.color="white") +
    annotate("shadowtext", x = trudeau_resignation_date, y = Inf, label = "Trudeau Resigns",
             angle = 90, vjust = 1.5, hjust = 1.1, color = "black", bg.color="white") +
    geom_point(data = labels_data, aes(x = label_date, y = Support), color='black', size = 2.9) +
    geom_point(data = labels_data, aes(x = label_date, y = Support, color = Party), size = 2.5) +
    geom_shadowtext(data = labels_data,fontface='bold', aes(x = label_date, y = Support, label = paste0(round(Support, 1), "%"), color = Party),
              vjust = -1, hjust = 0, size = 3.3, show.legend = FALSE,bg.color="white",bg.size=1) +
    labs(title = "Canadian Federal Election Polling Trends",
         subtitle = "Support for each party over time 2024-25",
         x = "Date",
         y = "Polling Support (%)",
         color = "Party") +  # Legend title
    theme_minimal() +  # Clean theme
    theme(legend.position = "right")
  
}

make_vis_plot_ewma<-function(data, cuttoff_date=as.Date("2024-061-01")){
  cad_long <- data %>%
    pivot_longer(cols = c(LPC, CPC, NDP, GPC, BQ, PPC), 
                 names_to = "Party", values_to = "Support") %>%
    filter(date >= cuttoff_date)
  
  # Define event dates
  tariff_delay_date <- as.Date("2025-02-03")
  trudeau_resignation_date <- as.Date("2025-01-06")
  label_date <- max(data$date)
  
  # Extract the values for labels at the given date
  labels_data <- cad_long %>%
    filter(date == label_date)
  
  # Plot with ggplot2
  ggplot(cad_long, aes(x = date, y = Support, color = Party)) +
    geom_line() +
    scale_color_manual(values = c("LPC" = "red", "CPC" = "blue", 
                                  "NDP" = "orange", "GPC" = "green", 
                                  "BQ" = "lightblue", "PPC" = "purple")) + 
    geom_vline(xintercept = as.numeric(tariff_delay_date), linetype = "dashed", color = "black") +
    geom_vline(xintercept = as.numeric(trudeau_resignation_date), linetype = "dashed", color = "black") +
    annotate("shadowtext", x = tariff_delay_date, y = Inf, label = "Tariff Delay", 
             angle = 90, vjust = 1.5, hjust = 1.1, color = "black", bg.color="white") +
    annotate("shadowtext", x = trudeau_resignation_date, y = Inf, label = "Trudeau Resigns", 
             angle = 90, vjust = 1.5, hjust = 1.1, color = "black", bg.color="white") +
    # Add dot and label for each party at label_date
    geom_point(data = labels_data, aes(x = label_date, y = Support), color='black', size = 2.9) +
    geom_point(data = labels_data, aes(x = label_date, y = Support, color = Party), size = 2.5) +
    geom_shadowtext(data = labels_data, fontface='bold', aes(x = label_date, y = Support, label = paste0(round(Support, 1), "%"), color = Party),
                    vjust = -1, hjust = 0, size = 3.3, show.legend = FALSE,bg.color="white",bg.size=1) +
    labs(title = "Canadian Federal Election Polling Trends",
         subtitle = "Support for each party over time 2024-25",
         x = "Date",
         y = "Polling Support (%)",
         color = "Party") +  # Legend title
    theme_minimal() +  # Clean theme
    theme(legend.position = "right")
  
}


# make_vis_plot(cad_with_weights)
p1<-make_vis_plot_lasso(cad_with_weights,span=0.5,cuttoff_date=as.Date("2024-01-01"))
p1

p2<-make_vis_plot_ewma(cad_ewma_avgs,cuttoff_date=as.Date("2024-10-01"))
p2

ggsave('cad_lasso.png',p1,width=10, height=6)

