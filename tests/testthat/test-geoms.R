test_that('geom_timeline draws correctly', {
  df = data.frame(LONGITUDE = 1,
                  LATITUDE = 2,
                  DATE = as.Date('2010-01-01'),
                  EQ_PRIMARY = 3,
                  LOCATION_NAME = 'Aa',
                  DEATHS = 0)

  out = df %>%
    ggplot() +
    geom_timeline(aes(x=DATE, size=EQ_PRIMARY, color=DEATHS)) +
    scale_y_continuous(limits = c(0, 4)) +
    labs(y='', size='Richter scale value', color = '# deaths') +
    theme_timeline()

  expect_equal(class(out), c('gg', 'ggplot'))
  expect_equal(out$labels, list(y='', size='Richter scale value', colour='# deaths', x='DATE'))
  expect_equal(out$data, df)
})

test_that('geom_timeline_label draws correctly', {
  df = data.frame(LONGITUDE = 1,
                  LATITUDE = 2,
                  DATE = as.Date('2010-01-01'),
                  EQ_PRIMARY = 3,
                  LOCATION_NAME = 'Aa',
                  DEATHS = 0)

  out = df %>%
    ggplot() +
    geom_timeline_label(aes(x=DATE, size=EQ_PRIMARY, color=DEATHS, label=LOCATION_NAME)) +
    scale_y_continuous(limits = c(0, 4)) +
    labs(y='', size='Richter scale value', color = '# deaths') +
    theme_timeline()

  expect_equal(class(out), c('gg', 'ggplot'))
  expect_equal(out$labels, list(y='', size='Richter scale value', colour='# deaths', x='DATE',
                                label='LOCATION_NAME'))
  expect_equal(out$data, df)
})
