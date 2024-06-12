test_that("test data world_boundaries", {
  data("world_boundaries")
  expect_equal(world_boundaries$NAME[1], 'Afghanistan')
  expect_equal(as.character(class(world_boundaries)), 'data.frame')
  expect_true('geometry' %in% names(world_boundaries))
})

