require(tidyverse)

f <- 'COVID-19/csse_covid_19_data/csse_covid_19_daily_reports_us/'

dat <- list.files(f, '*.csv') %>%
    map_dfr(~mutate(read_csv(paste0(f, .x)), date = as.Date(.x, '%m-%d-%Y')))

write_csv(dat, 'us-daily-full.csv')

p1 <- ggplot(dat, aes(date, Deaths, col = Province_State)) +
    geom_line(alpha = .7) +
    theme(legend.position = 'bottom')

ggsave('deaths.pdf', p1, width = 9, height = 8)
