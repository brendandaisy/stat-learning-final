require(tidyverse)
require(zoo)
require(ggthemes)
require(car)

### Read in the data (may take a min)
index <- read_csv('https://storage.googleapis.com/covid19-open-data/v2/index.csv')
epi <- read_csv('https://storage.googleapis.com/covid19-open-data/v2/epidemiology.csv')
gov <- read_csv('https://storage.googleapis.com/covid19-open-data/v2/oxford-government-response.csv')
mov <- read_csv('https://storage.googleapis.com/covid19-open-data/v2/mobility.csv')

### Filter data to just US states

state_keys <- index %>%
    filter(country_code == 'US', aggregation_level == 1) %>%
    filter(!str_detect(key, 'MP|GU|PR')) %>% # remove territories
    pull(key)

epi_state <- filter(
    epi,
    key %in% state_keys,
    new_tested > 0, new_confirmed > 0, # ignore these for now
    new_confirmed <=  new_tested
)

gov_state <- filter(gov, key %in% state_keys) %>%
    select(-stringency_index)

## inspect for factors with poor info
gov_state %>%
    select(-date, -key) %>%
    map(table, useNA = 'ifany')

## remove some vars based on above
gov_state <- gov_state %>%
    select(-c(
                international_support,
                fiscal_measures,
                emergency_investment_in_healthcare,
                investment_in_vaccines,
                vaccination_policy
            ))

mov_state <- mov %>%
    filter(key %in% state_keys)

## plot epidemic by state
epi_state %>%
    filter(date >= as.Date('2020-12-01')) %>%
    ggplot(aes(date, new_confirmed)) +
    geom_line() +
    facet_wrap(~key, scales = 'free_y') +
    theme(legend.position = 'none')

epi_state %>%
    filter(date >= as.Date('2020-12-01')) %>%
    ggplot(aes(date, new_tested)) +
    geom_line() +
    facet_wrap(~key, scales = 'free_y') +
    theme(legend.position = 'none')

epi_state %>%
    filter(
        date >= as.Date('2020-06-01'),
        date <= as.Date('2021-04-01')
    ) %>%
    ggplot(aes(date, new_confirmed / new_tested)) +
    geom_line() +
    facet_wrap(~key, scales = 'free') +
    theme(legend.position = 'none')

### Continuous reponse: pos_rate ~ gov_response

rate <- epi_state %>%
    transmute(date, key, pos_rate = 100 * new_confirmed / new_tested) %>%
    filter(pos_rate < 100)

dat_cont <- gov_state %>%
    inner_join(rate, by = c('date', 'key')) %>%
    drop_na %>% # remove remaining few NAs
    mutate_at(vars(-date, -key, -pos_rate), as.factor) %>%
    select_if(~length(unique(.x)) > 1) # just being extra careful

## find a nice looking transformation
ggplot(dat_cont, aes(log(pos_rate))) +
    geom_histogram(bins=25) +
    theme_few() +
    labs(x = 'Log percent positive cases', y = '')

## VIF: are we ok using all predictors throughout?
lm.fit = lm(log(pos_rate) ~. - date - key, data = dat_cont)
summary(lm.fit)
vif(lm.fit)

## remove collinearity (and fit above model again)
dat_cont <- select(dat_cont, -school_closing, -cancel_public_events)

## inspect residuals
plot(lm.fit)

write_csv(dat_cont, 'cont-response-may10.csv')

### Discrete response: new case rate ~ movement

## create response
dir <- epi_state %>%
    group_by(key) %>%
    transmute(
        date, key,
        sev_day = rollmean(new_confirmed, 7, NA),
        dir = rollapplyr(
            sev_day,
            width = 2,
            FUN = function(x) ifelse(x[1] <= x[2], 1, 0),
            fill = NA
        )
    ) %>%
    drop_na

## check it worked
dir %>%
    filter(date >= as.Date('2020-6-01'), # change dates of interest here
    date < as.Date('2021-1-31'), key %in% c('US_NY', 'US_CA', 'US_NC')) %>%
    ggplot(aes(date, log(sev_day), col = ifelse(dir == 0, 'Down', 'Up'))) +
    geom_point() +
    geom_line(col = 'grey20', alpha = .6) +
    facet_wrap(~key, scales = 'free_y', nrow = 3) +
    labs(x = 'Date', y = 'Log seven-day-average', col = 'Direction') +
    theme_few()

## join with predictors
dat_disc <- mov_state %>%
    inner_join(dir, by = c('date', 'key')) %>%
    drop_na

## VIF
glm <- glm(dir ~ . - sev_day - key - date, family = binomial, data = dat_disc)
vif(glm)

dat_disc <- select(dat_disc, -mobility_residential)

write_csv(dat_disc, 'disc-response-may10.csv')
