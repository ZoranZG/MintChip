library(data.table)
library(MintChip)

test_that("Interactions must be non-null", {
  expect_error(mintchip)
})

test_that("Interactions cannot be anything other than a data.frame or data.table",{
  expect_error(mintchip(interactions = 'hello_world'))
})

test_that("Interactions has a 'start' column",{
  expect_error(mintchip(interactions = data.table('end' = 1)))
})

test_that("Interactions has an 'end' column",{
  expect_error(mintchip(interactions = data.table('start' = 1)))
})

test_that("Interactions 'start' column is numeric",{
  expect_error(mintchip(interactions = data.table('start' = 'hello world', 'end' = 1)))
})

test_that("Interactions 'end' column is numeric",{
  expect_error(mintchip(interactions = data.table('start' = 1, 'end' = 'hello world')))
})

test_that("height_scale must be numeric",{
  expect_error(mintchip(interactions = data.table('start' = 1, 'end' = 10), height_scale = 'hello world'))
})

test_that("height_scale must be between [0,Inf)",{
  expect_error(mintchip(interactions = data.table('start' = 1, 'end' = 10), height_scale = -1))
})

test_that("thickness_scale must be numeric",{
  expect_error(mintchip(interactions = data.table('start' = 1, 'end' = 10), 
                        height_scale = 1, thickness_scale = 'hello world'))
})

test_that("thickness_scale must be between [0,Inf)",{
  expect_error(mintchip(interactions = data.table('start' = 1, 'end' = 10), 
                        height_scale = 1, thickness_scale = -1))
})

test_that("gene_list must be character vector",{
  expect_error(mintchip(interactions = data.table('start' = 1, 'end' = 10), 
                        height_scale = 1, thickness_scale = 1, gene_list = c(1,2,3)))
})


test_that("features has a 'start' column",{
  expect_error(mintchip(interactions = data.table('end' = 1, 'start' = 1),
                        features = data.table('end' = 1)))
})

test_that("features has an 'end' column",{
  expect_error(mintchip(interactions = data.table('end' = 1, 'start' = 1),
                        features = data.table('start' = 1)))
})

test_that("features 'start' column is numeric",{
  expect_error(mintchip(interactions = data.table('end' = 1, 'start' = 1),
                        features = data.table('start' = 'hello world', 'end' = 1)))
})

test_that("features 'end' column is numeric",{
  expect_error(mintchip(interactions = data.table('end' = 1, 'start' = 1),
                        features = data.table('start' = 1, 'end' = 'hello world')))
})

test_that("Plotting runs to the end",{
  expect_error(mintchip(interactions = data.table('end' = 4, 'start' = 1),
           features = data.table('start' = 1, 'end' = 3, 'name' = 'hello', 'color' = 'blue')), NA)
})

