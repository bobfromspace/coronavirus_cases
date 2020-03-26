# ==========================================================================
# Load all data files from ready_coding and transform them into united dataset
# ==========================================================================

library(dplyr)
library(openxlsx)
library(data.table)
library(httr)

fls <- list.files("ready_coding", full.names = TRUE)

df <- lapply(fls, function(fl) {
		df <- read.xlsx(fl, sheet = 1)
		vars <- grep("date", names(df), value = TRUE)
		df <- mutate_at(df, vars(vars), convertToDate)
	}) %>%
 	bind_rows()

#
# Transform the dataset
# --------------------------------------------------------------------------

non_transf <- select(df, starts_with("country"), COWcode)

transf <- select(df, country_id, starts_with("measure"), starts_with("start_date"))
transf <- data.table::melt(setDT(transf),
	id.vars = "country_id",
	measure.vars = patterns(measure = 'measure',
		start_date = 'start_date')) %>%
	select(-variable) %>%
	filter(!grepl("-04-|-03-25|-03-24", start_date))

min_date <- min(transf$start_date, na.rm = TRUE) - 1
max_date <- max(transf$start_date, na.rm = TRUE)

min_df <- data.frame(country_id = unique(transf$country_id),
	measure = 13,
	start_date = as.Date(min_date, format = "%Y-%m-%d"))

max_df <- group_by(transf, country_id) %>%
	arrange(desc(start_date)) %>%
	slice(1L) %>%
	ungroup() %>%
	mutate(measure = case_when(
		is.na(measure) ~ 13,
		TRUE ~ measure
		),
	start_date = case_when(
		measure == 13 ~ max(start_date, na.rm = TRUE),
		TRUE ~ max(start_date, na.rm = TRUE)
		)) %>%
	anti_join(transf)

date_df <- bind_rows(transf, min_df, max_df) %>%
	arrange(country_id, start_date) %>%
	filter(!is.na(start_date))

full_df <- lapply(unique(date_df$country_id), function(cntr_id) {
	df <- date_df[date_df$country_id == cntr_id,]
	dates <- list()

	for (row in 1:{nrow(df) - 1}) {
		dates[[row]] <- data.frame(country_id = unique(df$country_id),
			start_date = seq(df$start_date[row], df$start_date[row + 1], by = "days"))
	}
	append_df <- bind_rows(dates) %>%
		distinct() %>%
		anti_join(df)

	out_df <- bind_rows(df, append_df) %>%
		group_by(country_id) %>%
		arrange(start_date) %>%
		tidyr::fill(measure, .direction = "down") %>%
		ungroup()

	return(out_df)
	}) %>%
	bind_rows() %>%
	group_by(country_id, start_date) %>%
	filter(measure == max(measure)) %>%
	ungroup() %>%
	mutate(
	# OK, let's recode emergency, business measures, and gatherings for convenience
	measure = case_when(
		measure == 3 ~ 4,
		measure == 4 ~ 6,
		measure == 5 ~ 7,
		measure == 6 ~ 9,
		measure == 7 ~ 11,
		measure == 8 ~ 12,
		measure == 9 ~ 5,
		measure == 10 ~ 8,
		measure == 11 ~ 10,
		measure == 12 ~ 3,
		TRUE ~ measure
		)
	) 

out_df <- left_join(full_df, non_transf, by = "country_id")

#
# Get COVID-19 dataset
# --------------------------------------------------------------------------
covid_url <- paste0("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",
	Sys.Date(), ".xlsx")
GET(covid_url, authenticate(":", ":", type="ntlm"),
	write_disk(paste0("covid/covid19_", Sys.Date(), ".xlsx"), overwrite = TRUE))
covid <- read.xlsx(paste0("covid/covid19_", Sys.Date(), ".xlsx")) %>%
	mutate(start_date = convertToDate(DateRep)) %>%
	select(country_name = `Countries.and.territories`, start_date,
		n_prirost = Cases)

# filter the countries
cov_df <- semi_join(covid, out_df, by = "country_name")

# how many cases on the first date of observation
preprocc <- mutate(cov_df, bef_start = ifelse(start_date > as.Date(min_date, format = "%Y-%m-%d"), FALSE, TRUE))

start <- group_by(preprocc, country_name, bef_start) %>%
	summarise(n_cases = sum(n_prirost)) %>%
	ungroup() %>%
	filter(bef_start == TRUE) %>%
	select(-bef_start) %>%
	mutate(start_date = as.Date(min_date, format = "%Y-%m-%d"))

end <- filter(preprocc, bef_start == FALSE) %>%
	arrange(country_name, start_date) %>%
 	group_by(country_name) %>%
	mutate(n_cases = cumsum(n_prirost)) %>%
	ungroup() %>%
	select(-bef_start, -n_prirost)

cov_df <- bind_rows(start, end) %>%
	arrange(country_name, start_date)

#
# Merge two datasets
# --------------------------------------------------------------------------
out_df <- left_join(out_df, cov_df, by = c("country_name", "start_date")) %>%
	group_by(country_name) %>%
	tidyr::fill(n_cases, .direction = "down") %>%
	ungroup() %>%
	mutate(n_cases = ifelse(is.na(n_cases), 0, n_cases)) %>%
	arrange(country_id, start_date)

# Export the dataset
# --------------------------------------------------------------------------
write.csv2(out_df, "plot/plot_df.csv", row.names = FALSE)