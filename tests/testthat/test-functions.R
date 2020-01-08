test_that('eq_location_clean works', {
  df = data.frame(LOCATION_NAME = c('AA:BB', 'CC:DD:EE'), stringsAsFactors = F)
  expected_out = data.frame(LOCATION_NAME = c('Bb', 'Ee'), stringsAsFactors = F)
  expect_equal(eq_location_clean(df), expected_out)
})

test_that('eq_clean_data works', {
  #require(readr)

  # use interal data
  filepath = system.file("extdata", "earthquakes.txt", package = "noaaeq")

  # load and clean data
  df = expect_warning(readr::read_delim(filepath, delim = "\t")) %>%
    eq_clean_data()

  expect_equal(class(df$DATE), 'Date')
  expect_equal(dim(df), c(6156, 42))
})

test_that('eq_create_label works', {
  df = data.frame(LOCATION_NAME = c('A', 'B'),
                  EQ_PRIMARY = c(1,2),
                  TOTAL_DEATHS = c(3, NA),
                  stringsAsFactors = F)
  expected_out = c("<b>Location:</b> A <br /> <b>Magnitude:</b> 1 <br /> <b>Total deaths:</b> 3 <br />",
                   NA)
  expect_equal(eq_create_label(df), expected_out)
})

test_that('eq_map works', {
  df = data.frame(LONGITUDE = 1,
                  LATITUDE = 2,
                  DATE = as.Date('2010-01-01'),
                  EQ_PRIMARY = 3,
                  LOCATION_NAME = 'Aa',
                  TOTAL_DEATHS = 0)
  # basic output
  out = eq_map(df)
  expect_equal(class(out), c('leaflet', 'htmlwidget'))
  expect_equal(out$x$limits$lat, c(2, 2))
  expect_equal(out$x$limits$lng, c(1, 1))
  expect_equal(out$x$calls[[2]]$args[[9]], df$DATE)

  # annotation output
  out = df %>%
    mutate(popup_text = eq_create_label(.)) %>%
    eq_map(annot_col = 'popup_text')
  expect_equal(out$x$calls[[2]]$args[[9]],
               "<b>Location:</b> Aa <br /> <b>Magnitude:</b> 3 <br /> <b>Total deaths:</b> 0 <br />")
})
