# ==========================================================================
# Plot how post-Soviet countries introduced their measures against COVID-19
# ==========================================================================

library(dplyr)
library(ggplot2)
library(gganimate)
library(sf) 
library(maps)
library(extrafont)

df <- read.csv2("plot/plot_df.csv")

centroids <- read.csv2("labs.csv") %>%
	mutate(long = as.numeric(long),
		lat = as.numeric(lat)) %>%
	rename(long_t = long, lat_t = lat)

df <- left_join(df, centroids, by = "country_text_id") %>%
	mutate(labs = paste0(country_text_id, "\n", n_cases))

# for testing
# tst <- filter(df, start_date == "2020-02-29")

Sys.setlocale("LC_TIME", "English")

#
# Prepare the map base layer and country labels
# --------------------------------------------------------------------------
coords <- map_data("world", region = df$country_name) %>%
	select(-subregion, -order) %>%
	distinct(.keep_all = TRUE)

plot_data <- left_join(coords, df,
		by = c("region" = "country_name")) %>%
	mutate(start_date = as.Date(start_date, format = "%Y-%m-%d"))

#
# Plot the data
# --------------------------------------------------------------------------
p <- ggplot(plot_data, aes(x = long, y = lat)) +
	geom_polygon(aes(group = group, fill = as.factor(measure))) +
	geom_polygon(aes(group = group, fill = as.factor(measure)),
		colour = "#373440", show.legend = FALSE) +
	geom_text(plot_data, mapping = aes(x = long_t, y = lat_t, label = labs),
		size = 3, colour = "#2C2E33") +
	coord_map(ylim = c({min(plot_data$lat) - 1}, 65),
		xlim = c({min(plot_data$long) - 2}, 100)) +
	labs(title = "Measures against COVID-19 in post-Soviet countries",
		subtitle = "Date: {frame_time}",
			caption = "Data: @bob_from_space & @tjumachlinets (Twitter), ECDC"
			) +
	scale_fill_manual(values = c(
		"1" = "#D1DBBD",
		"2" = "#91AA9D",
		"3" = "#3E606F",
		"4" = "#438ED9", 
		"5" = "#244B73", 
		"6" = "#F2F307", 
		"7" = "#F2CB05", 
		"8" = "#E69C15", 
		"9" = "#FF5168", 
		"10" = "#D14D78", 
		"11" = "#F54F4B", 
		"12" = "#E01400", 
		"13" = "#ADCF4F"),
		labels = c(
			"1" = "Limits on international\ntravel",
			"2" = "Ban on international travel\nfor everyone",
			"3" = "Limits on transportation\nwithin the country",
			"4" = "Limit of public gatherings",
			"5" = "Ban on public gatherings",
			"6" = "Shutdown of schools/universities",
			"7" = "Limitations on the operation of\nstate institutions",
			"8" = "Limits on business operation",
			"9" = "Regional quarantine",
			"10" = "National quarantine",
			"11" = "Regional emergency",
			"12" = "National emergency",
			"13" = "No serious measures")) +
	theme(
		# background
		panel.background = element_rect(fill = "#FFFFFF"),
		plot.background = element_rect(fill = "#FFFFFF"),
		panel.grid = element_line(colour = "#FFFFFF"),
		panel.border = element_rect(colour = "#343D40",
			fill = NA),
		# title
		plot.title = element_text(colour = "#2C2E33",
			face = "bold", family = "Rockwell"),
		plot.subtitle = element_text(colour = "#2C2E33",
			family = "Rockwell", face = "italic"),
		# legend and caption
		legend.title = element_blank(),
		legend.position = "right",
		legend.background = element_rect(fill = "#FFFFFF"),
		legend.text = element_text(colour = "#2C2E33",
			family = "Rockwell", size = 11),
		legend.key.size = unit(0.7, "cm"),
		legend.key.width = unit(0.4, "cm"),
		text = element_text(family = "Rockwell",
			colour = "#2C2E33"),
		# axis
		axis.title = element_blank(),
		axis.text = element_blank(),
		axis.ticks = element_blank()
		) +
	transition_time(start_date)

animate(plot = p, fps = 1, width = 950, height = 650,
        renderer = gifski_renderer("plot/map_covid19.gif"))