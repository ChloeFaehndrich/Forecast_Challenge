knitr::opts_chunk$set(echo = TRUE)
library(tidyverse) # install.packages('tidyverse'), collection of R packages for data manipulation, analysis and visualisation
library(lubridate)
source('ignore_sigpipe.R')
#read in the targets data
targets <- read_csv('https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz')

# read in the sites data
aquatic_sites <- read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") |>
  dplyr::filter(aquatics == 1)

targets[1000:1010,]

lake_sites <- aquatic_sites %>%
  filter(field_site_subtype == 'Lake')

targets <- targets %>%
  filter(site_id %in% lake_sites$field_site_id)

## Visualize the data

targets %>%
  filter(variable == 'temperature') %>%
  ggplot(., aes(x = datetime, y = observation)) +
  geom_point() +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  facet_wrap(~site_id, scales = 'free_y') +
  labs(title = 'temperature')

targets %>%
  filter(variable == 'oxygen') %>%
  ggplot(., aes(x = datetime, y = observation)) +
  geom_point() +  
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  facet_wrap(~site_id, scales = 'free_y')+
  labs(title = 'oxygen')

targets %>%
  filter(variable == 'chla') %>%
  ggplot(., aes(x = datetime, y = observation)) +
  geom_point() +   
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  facet_wrap(~site_id, scales = 'free_y')+
  labs(title = 'chla')

## Forecast for surface water temperature
targets <- targets %>%
  filter(variable == 'temperature')

# past stacked weather
df_past <- neon4cast::noaa_stage3()

variables <- ("precipitation_flux")
#Other variable names can be found at https://projects.ecoforecast.org/neon4cast-docs/Shared-Forecast-Drivers.html#stage-2

noaa_past <- df_past |> 
  dplyr::filter(site_id %in% lake_sites$field_site_id,
                datetime >= ymd('2017-01-01'),
                variable %in% variables) |> 
  dplyr::collect()

noaa_past

# aggregate the past to mean values
noaa_past_mean <- noaa_past |> 
  mutate(datetime = as_date(datetime)) |> 
  group_by(datetime, site_id, variable) |> 
  summarize(prediction = mean(prediction, na.rm = TRUE), .groups = "drop") |> 
  pivot_wider(names_from = variable, values_from = prediction)

# New forecast only available at 5am UTC the next day

forecast_date <- Sys.Date() 
noaa_date <- forecast_date - days(1)

df_future <- neon4cast::noaa_stage2()

variables <- "precipitation_flux"

noaa_future <- df_future |> 
  dplyr::filter(reference_datetime == noaa_date,
                datetime >= forecast_date,
                site_id %in% lake_sites$field_site_id,
                variable %in% variables) |> 
  dplyr::collect()
noaa_future

noaa_future_daily <- noaa_future |> 
  mutate(datetime = as_date(datetime)) |> 
  # mean daily forecasts at each site per ensemble
  group_by(datetime, site_id, parameter, variable) |> 
  summarize(prediction = mean(prediction)) |>
  pivot_wider(names_from = variable, values_from = prediction) |>
  # convert to Celsius
  #  mutate(air_temperature = air_temperature - 273.15) |> 
  select(datetime, site_id, precipitation_flux, parameter)

noaa_future_daily

##ggplot(noaa_future_daily, aes(x=datetime, y=precipitation_flux)) +
  ##geom_line(aes(group = parameter), alpha = 0.4)+
  #geom_line(data = noaa_past_mean, colour = 'darkblue') +
  #facet_wrap(~site_id, scales = 'free')

# ggplot(noaa_future_daily, aes(x=datetime, y=precipitation_flux)) +
#   geom_line(aes(group = parameter), alpha = 0.4)+
#   geom_line(data = noaa_past_mean, colour = 'darkblue') +
#   coord_cartesian(xlim = ymd(c('2022-09-01', '2022-12-05')))+
#   facet_wrap(~site_id, scales = 'free')

##Join the historic weather data with the targets to aid in fitting the linear model
targets_lm <- targets |> 
  filter(variable %in% c("temperature"))|>
  pivot_wider(names_from = 'variable', values_from = 'observation') |> 
  left_join(noaa_past_mean, 
            by = c("datetime","site_id"))
targets_lm[1000:1010,]

##fit a separate linear model for Lake Suggs
example_site <- 'SUGG'

site_target <- targets_lm |> 
  filter(site_id == example_site)

noaa_future_site <- noaa_future_daily |> 
  filter(site_id == example_site)

#Fit linear model based on past data: water temperature = m * air temperature + b
fit <- lm(site_target$temperature ~ site_target$precipitation_flux)

# use linear regression to forecast water temperature for each ensemble member
forecasted_temperature <- fit$coefficients[1] + fit$coefficients[2] * noaa_future_site$precipitation_flux

## Specify forecast model

temp_lm_forecast <- NULL

for(i in 1:length(lake_sites$field_site_id)) {  
  example_site <- lake_sites$field_site_id[i]
  
  site_target <- targets_lm |>
    filter(site_id == example_site)
  
  noaa_future_site <- noaa_future_daily |> 
    filter(site_id == example_site)
  
  #Fit linear model based on past data: water temperature = m * air temperature + b
  fit <- lm(site_target$temperature ~ lag(site_target$precipitation_flux))
  # fit <- lm(site_target$temperature ~ ....)
  
  # use linear regression to forecast water temperature for each ensemble member
  forecasted_temperature <- fit$coefficients[1] + fit$coefficients[2] * noaa_future_site$precipitation_flux
  
  # put all the relavent information into a tibble that we can bind together
  temperature <- tibble(datetime = noaa_future_site$datetime,
                        site_id = example_site,
                        parameter = noaa_future_site$parameter,
                        prediction = forecasted_temperature,
                        variable = "temperature")
  
  temp_lm_forecast <- dplyr::bind_rows(temp_lm_forecast, temperature)
  message(example_site, ' temperature forecast run')
  
}

##Looking at the forecasts we produced
temp_lm_forecast %>% 
  filter(variable == 'temperature') %>%
  ggplot(.,aes(x=datetime, y=prediction, group = parameter)) + 
  geom_point(data = targets,aes(x=datetime, y=observation, group = 'obs'), colour = 'darkblue') +
  geom_line(alpha = 0.3, aes(colour = 'ensemble member (parameter)')) + 
  facet_wrap(~site_id, scales = 'free_y') +
  scale_x_date(expand = c(0,0), date_labels = "%d %b") +
  labs(y = 'value') +
  geom_vline(aes(linetype = 'reference_datetime', xintercept = Sys.Date()), colour = 'blue', size = 1.5) +
  labs(title = 'site_id', subtitle = 'variable = temperature', caption = 'prediction') + 
  annotate("text", x = Sys.Date() - days(10), y = 20, label = "past")  +
  annotate("text", x = Sys.Date() + days(12), y = 20, label = "future")  +
  theme_bw() +
  coord_cartesian(xlim = c(min(temp_lm_forecast$datetime) - 15,
                           Sys.Date() + 30)) +
  scale_linetype_manual(values = 'dashed', name = '') +
  scale_colour_manual(values = 'darkgrey', name = '') +
  theme(strip.text = element_text(colour = 'orange'),
        axis.title.y = element_text(colour = 'green'),
        axis.title.x = element_text(colour = 'red'),
        axis.text.y = element_text(colour = 'purple'),
        axis.text.x = element_text(colour = 'red'),
        plot.caption = element_text(hjust = 0, colour = 'purple'),
        plot.title = element_text(colour = 'orange'), 
        plot.subtitle = element_text(colour = 'green')) 

## Convert to EFI standard for submission

temp_lm_forecast_EFI <- temp_lm_forecast %>%
  mutate(model_id = 'precip_mod',
         reference_datetime = as_date(min(datetime)) - days(1),
         family = 'ensemble',
         parameter = as.character(parameter)) %>%
  select(model_id, datetime, reference_datetime, site_id, family, parameter, variable, prediction)

## Submit forecast

# Start by writing the forecast to file
theme <- 'aquatics'
date <- temp_lm_forecast_EFI$reference_datetime[1]
forecast_name_1 <- paste0(temp_lm_forecast_EFI$model_id[1], ".csv")
forecast_file_1 <- paste(theme, date, forecast_name_1, sep = '-')
forecast_file_1

write_csv(temp_lm_forecast_EFI, forecast_file_1)

neon4cast::forecast_output_validator(forecast_file_1)

# can uses the neon4cast::forecast_output_validator() to check the forecast is in the right format
neon4cast::submit(forecast_file = forecast_file_1,
                  ask = FALSE) # if ask = T (default), it will produce a pop-up box asking if you want to submit
