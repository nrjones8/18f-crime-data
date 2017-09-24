library(dplyr)
library(ggplot2)
library(plyr)
library(reshape2)

prep_data <- function() {
  # This is an ugly function
  data <- read.csv('data/pe_employee_data.csv')
  postals_missing_years <- get_postal_missing_years(data)
  print(sprintf('Removing %s', postals_missing_years))
  rows_to_keep <- which(!(data$state_postal_abbr %in% postals_missing_years))
  print('num rows before')
  print(nrow(data))
  data <- data[rows_to_keep, ]
  print('after')
  print(nrow(data))
  print(names(data))
  # Also load data about how urban each state is, from 2010
  urban_stats <- get_urban_stats()
  with_urban_stats <- merge(data, urban_stats, by.x="state_postal_abbr", by.y="Abbreviation")
  return(with_urban_stats)
}

get_urban_stats <- function() {
  # Data are from 2010, see: https://www.census.gov/geo/reference/ua/ualists_layout.html,
  # specifically PctUrbanRural_State.txt
  pct_urban <- read.csv('data/PctUrbanRural_State.csv')
  
  # Other dataset uses state abbreviations (rather than state names), so include state
  # abbreviations in the returned dataframe as well. Data from
  # http://www.fonz.net/blog/archives/2008/04/06/csv-of-states-and-state-abbreviations/
  states_and_postal <- read.csv('data/states_with_postal.csv')
  merged <- merge(pct_urban, states_and_postal, by.x="STATENAME", by.y="State")
  # We're interested in % of people living in urban areas, not % of land that is urban
  return(merged[, c('STATENAME', 'POPPCT_URBAN', 'Abbreviation')])
}

get_postal_missing_years <- function(df) {
  counts <- ddply(df, "state_postal_abbr", function(grouped) {
    data.frame(num_years_of_data=nrow(grouped))
  })
  
  expected_num_years <- max(counts[, 'num_years_of_data'])
  return(counts[which(counts$num_years_of_data < expected_num_years), 'state_postal_abbr'])
}

yearly_summaries <- function(df) {
  yearly <- ddply(df, "data_year", function(grouped) {
    data.frame(
      min_off_per_1000=min(grouped$officer_rate_per_1000),
      median_off_per_1000=median(grouped$officer_rate_per_1000),
      max_off_per_1000=max(grouped$officer_rate_per_1000),
      
      min_civ_per_1000=min(grouped$civilian_rate_per_1000),
      median_civ_per_1000=median(grouped$civilian_rate_per_1000),
      max_civ_per_1000=max(grouped$civilian_rate_per_1000)
    )
  })
  
  return(yearly)
}


#' @param df data frame that contains data from original employee dataset
#' @param num_lags the number of years to "lag" by. E.g. if num_lags is 3, then the returned
#' data will contain a column with the difference in officer / civilian rates over 3
#' years, so a column for 1983 data would contain the difference in rate between 1983 and 1980
#' @return data frame with `delta_officer`
calculate_lags <- function(df, num_lags=1) {
  # Order by state, then year - so that each `prior_...` column will be comparing one state's
  # rate to that same state's rate, one year prior
  with_lag <- df %>%
    arrange(state_postal_abbr, data_year) %>%
    mutate(
      prior_officer_rate = lag(officer_rate_per_1000, k=num_lags),
      prior_civilian_rate = lag(civilian_rate_per_1000, k=num_lags)
    )
  
  with_lag$delta_officer_rate <- with_lag$officer_rate_per_1000 - with_lag$prior_officer_rate
  with_lag$delta_civilian_rate <- with_lag$civilian_rate_per_1000 - with_lag$prior_civilian_rate
  
  cols_to_keep <- c('state_postal_abbr', 'data_year', 'officer_rate_per_1000', 'civilian_rate_per_1000', 'delta_officer_rate', 'prior_officer_rate', 'prior_civilian_rate', 'delta_civilian_rate')

  # 1971 is the first year of data we have, so can't compare 1971 to anything prior
  rows_to_keep <- which(with_lag$data_year != '1971')
  return(with_lag[rows_to_keep, cols_to_keep])
}

plot_yearly_summary <- function(df) {
  # whyyyy
  melted <- melt(df, id.vars='data_year')
  
  g <- ggplot(melted, aes(x = data_year, y = value)) +
    geom_line(aes(color=variable)) +
    #geom_hline(yintercept=0) +
    scale_x_continuous("Year") +
    # If only plotting subsets of all games, keep axis constant
    ggtitle('Oh hai')
  
  print(g)
}

plot_officer_heatmap <- function(df, save_image=FALSE) {
  # Thanks to tutorial here: http://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/
  # Also good: http://quantcorner.wordpress.com/2013/11/02/creating-a-heatmap-to-visualize-returns-with-r-ggplot2/
  
  heat <- ggplot(df, aes(data_year, state_postal_abbr)) +
    geom_tile(aes(fill = officer_rate_per_1000)) +
    scale_fill_gradient2("Officer Rate per 1000") +
    theme_bw() +
    scale_x_continuous('Year') +
    scale_y_discrete('State') +
    ggtitle('Officers per 1000 People, by state and year') +
    # Center the damn title
    theme(plot.title = element_text(hjust = 0.5))
  
  print(heat)

  if (save_image) {
    ggsave("graphics/officer_rate_heatmap_state_year.png", device="png")
  }
}

plot_lagging_officer_heatmap <- function(df, save_image=FALSE) {
  heat <- ggplot(df, aes(data_year, state_postal_abbr)) +
    geom_tile(aes(fill = delta_officer_rate)) +
    scale_fill_gradient2("Change in Officer Rate per 1000", low = 'blue', high = 'red') +
    theme_bw() +
    scale_x_continuous('Year') +
    scale_y_discrete('State') +
    ggtitle('One Year Change in Officer rate per 1000') +
    # Center the damn title
    theme(plot.title = element_text(hjust = 0.5))
  
  print(heat)
  if (save_image) {
    ggsave("graphics/officer_rate_deltas_heatmap_state_year.png", device="png")
  }
}

plot_civilian_heatmap <- function(df, save_image=FALSE) {
  heat <- ggplot(df, aes(data_year, state_postal_abbr)) +
    geom_tile(aes(fill = civilian_rate_per_1000)) +
    scale_fill_gradient2("Officer Rate per 1000") +
    theme_bw() +
    scale_x_continuous('Year') +
    scale_y_discrete('State') +
    ggtitle('Civilian Police per 1000 People, by state and year') +
    # Center the damn title
    theme(plot.title = element_text(hjust = 0.5))
  
  print(heat)
  
  if (save_image) {
    ggsave("graphics/civilian_rate_heatmap_state_year.png", device="png")
  }
}

plot_lagging_civilian_heatmap <- function(df, save_image=FALSE) {
  heat <- ggplot(df, aes(data_year, state_postal_abbr)) +
    geom_tile(aes(fill = delta_civilian_rate)) +
    scale_fill_gradient2("Change in Civilian Employee Rate per 1000", low = 'blue', high = 'red') +
    theme_bw() +
    scale_x_continuous('Year') +
    scale_y_discrete('State') +
    ggtitle('One Year Change in Civilian Employee rate per 1000') +
    # Center the damn title
    theme(plot.title = element_text(hjust = 0.5))
  
  print(heat)
  if (save_image) {
    ggsave("graphics/civilian_rate_deltas_heatmap_state_year.png", device="png")
  }
}


plot_urban_officer <- function(df, save_plot=FALSE, year='2010') {
  only_year <- df[which(df$data_year == year), ]
  g <- ggplot(only_year, aes(x=POPPCT_URBAN, y=officer_rate_per_1000)) +
    geom_point(alpha=.45) +
    geom_smooth(method=lm) +
    #geom_jitter(position=position_jitter(width=5, height=0)) +
    geom_text(aes(label=state_postal_abbr), hjust=0, vjust=0) +
    scale_x_continuous("% of Population in Urban Areas") +
    scale_y_continuous("Officers per 1000 people") +
    ggtitle("Officers per 1000 people vs. Urban Population, by state") +
    theme(plot.title = element_text(hjust = 0.5))
  print(g)
  
  if (save_plot) {
    ggsave("graphics/urban_pop_vs_officer_rate.png", device="png")
  }
}

setwd('~/personal/src/18f-crime-data/')
data <- prep_data()

# Plotting everything together makes it clear that DC is an outlier; this likely makes sense,
# since I'd imagine cities have much higher rates of police, and DC is the other unit here that
# is not a full state (which would contain both cities and rural areas)
without_dc <- data[which(data$state_postal_abbr != 'DC'), ]

plot_officer_heatmap(without_dc, TRUE)
yearly <- yearly_summaries(without_dc)

lagged <- calculate_lags(without_dc)
plot_lagging_officer_heatmap(lagged, TRUE)
plot_lagging_civilian_heatmap(lagged, TRUE)

# Linear model stuff
year_2010 <- without_dc[which(without_dc$data_year == '2010'), ]
fit <- lm(officer_rate_per_1000 ~ POPPCT_URBAN, data=year_2010)
summary(fit)
plot_urban_officer(without_dc, TRUE)
