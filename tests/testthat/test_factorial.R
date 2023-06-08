data("inference")
items <- c(paste0("item_0",3:9),"item_10")


test_that(" test factor explorer ", {
  tmp <- factor_explorer(inference, items)
  expect_named(tmp, c("paralle analysis", "vss analysis", "factor"))
  expect_silent(tmp)
  expect_named(tmp$factor, c("parallele","VSS complexity 1", "VSS complexity 2", "Velicer MAP", "BIC",  "eBIC"  ))
})




test_that("test factor comparison",{
  factors <- factor_explorer(inference, items)
  tmp1 <- factor_comparaison(inference, items, factors)
  tmp2 <- factor_comparaison(inference, items,c(4,1,2))
  expect_identical(tmp1,tmp2)
  expect_named(tmp1,c("1" , "2",  "4" , "model comparaison"))
})
