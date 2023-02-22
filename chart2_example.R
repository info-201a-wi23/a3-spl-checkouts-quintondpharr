# Get the top 5 creators with the most checkouts
top_creators <- spl_df %>% 
     filter(!grepl("^\\s*$", Creator)) %>% # remove rows where Creator is blank/whitespace
     group_by(Creator) %>% 
     summarise(total_checkouts = sum(Checkouts, na.rm = T)) %>% 
     arrange(desc(total_checkouts)) %>% 
     slice_head(n = 5)

# Filter the data for the top creators
top_creators_checkouts <- spl_df %>% 
     filter(Creator %in% top_creators$Creator)

# Calculate the total checkouts per month for the top creators
top_creators_checkouts_by_month <- top_creators_checkouts %>% 
     group_by(CheckoutMonth, Creator) %>% 
     summarise(total_checkouts = sum(Checkouts))

# Plot the total checkouts per month by the top creators
ggplot(top_creators_checkouts_by_month, aes(x = CheckoutMonth, y = total_checkouts, color = Creator)) +
     geom_line(size = 1) +
     labs(title = "Total Checkouts per Month by Top Authors",
          x = "Month",
          y = "Total Checkouts Per Author") +
     scale_x_continuous(breaks = seq(1, 12, 1)) +
     scale_color_discrete(name = "Authors")


