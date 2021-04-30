require(tidyverse)
require(zoo)
require(ggthemes)

### Read in the data (may take a min)
index <- read_csv('https://storage.googleapis.com/covid19-open-data/v2/index.csv')
epi <- read_csv('https://storage.googleapis.com/covid19-open-data/v2/epidemiology.csv')
gov <- read_csv('https://storage.googleapis.com/covid19-open-data/v2/oxford-government-response.csv')

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
    select(-stringency_index) %>%
    select( # remove these for now because they were causing model issues
        -c(
             international_travel_controls,
             international_support,
             public_information_campaigns
         )
    )

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
    transmute(date, key, pos_rate = new_confirmed / new_tested)

dat_lm <- gov_state %>%
    inner_join(rate, by = c('date', 'key')) %>%
    filter(
        date >= as.Date('2020-6-01')
    ) %>% # change dates of interest here
    select_if(~length(unique(.x)) > 1) %>%
    mutate_at(
        vars(
            school_closing:restrictions_on_internal_movement,
            debt_relief,
            testing_policy:contact_tracing,
            facial_coverings:vaccination_policy
        ),
        as.factor
    )

lm <- lm(pos_rate ~ . - date - key, data = dat_lm)
summary(lm)

### Old code for at the county level (not used)

ocounty_keys <- index %>%
    filter(country_code == 'US', aggregation_level == 2) %>%
    pull(key)

epi_us_county <- filter(epi, key %in% county_keys) %>%
    drop_na(new_tested, new_confirmed) %>%
    filter(new_tested > 0, new_confirmed > 0)

epi_us_county %>%
    filter(
        date >= as.Date('2020-02-01'),
        date <= as.Date('2020-04-01'),
        str_detect(key, 'OR')
    ) %>%
    ggplot(aes(date, new_confirmed, col = key)) +
    geom_point() +
    geom_line() +
    theme(legend.position = 'none')

epi_us_county %>%
    ggplot(aes(new_tested)) +
    geom_density()

subc <- sample(unique(epi_us_county$key), 50)

epi_us_county %>%
    filter(date >= as.Date('2020-12-01'), key %in% subc) %>%
    ggplot(aes(date, log(new_tested), col = key)) +
    geom_line(alpha = .7) +
    theme_few() +
    theme(legend.position = 'none')

dat <- epi_us_county %>%
    mutate(
        new_negative = map2_dbl(new_tested, new_confirmed, ~max(.x - .y, 0)),
        nc3 = rollmean(new_confirmed, 3, fill = NA),
        nc7 = rollmean(new_confirmed, 7, fill = NA)
    )

dat %>%
    filter(
        date >= as.Date('2021-01-01'),
        date <= as.Date('2021-04-01')
    ) %>%
    ggplot(aes(date, nc7, col = key)) +
    geom_point() +
    geom_line() +
    theme(legend.position = 'none')

ggplot(dat, aes(log(new_tested), log(new_confirmed), col = key)) +
    geom_point() +
    theme(legend.position = 'none')

fit <- glm(
    cbind(new_confirmed, new_negative) ~ date,
    family = binomial(),
    data = dat
)

fit2 <- lm(new_confirmed ~ new_tested, data = dat)

