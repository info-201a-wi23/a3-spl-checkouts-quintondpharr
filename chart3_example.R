# Calculate the total checkouts for each publisher
total_checkouts_by_publisher <- spl_df %>% 
     group_by(Publisher) %>% 
     summarise(total_checkouts = sum(Checkouts)) %>% 
     arrange(desc(total_checkouts))

# Select the top 5 publishers
top_5_publishers <- head(total_checkouts_by_publisher, 5)

ggplot(data = top_5_publishers) +
     geom_col(mapping = aes(
          x = reorder(Publisher, -total_checkouts),
          y = total_checkouts,
          fill = Publisher,)) +
     labs(x = "Publisher", y = "Total Checkouts", title = "Top 5 Publishers by Total Checkouts") +
     scale_y_continuous(labels = label_number_si()) +
     theme(axis.text.x = element_text(angle = 45, hjust = 1))
