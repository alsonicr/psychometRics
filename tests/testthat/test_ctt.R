
data("inference")
items <- c(paste0("item_0",3:9),"item_10")


test_that(" CTT frequency ", {
  expect_output(str(freq.scan(inference, items)), "2 VARIABLES", ignore.case = TRUE)
  expect_output(str(freq.scan(inference, items)), "8 obs")
})


test_that(" CTT discrimination ", {
  tmp <- discrimination_cor(inference, items, type="rir")
  expect_output(str(tmp), "9 variables")
  expect_output(str(tmp), "8 obs")
  tmp <- discrimination_cor(inference, items, type=c("rir","rit"))
  expect_output(str(tmp), "9 variables")
  expect_output(str(tmp), "16 obs")
})



test_that(" CTT alpha ", {
  tmp <- alpha.scan(inference, items,parallel = T, verbose = F)
  expect_output(str(tmp), "8 obs")
  expect_output(str(tmp), "10 variables")
  tmp <- alpha.scan(inference, items,parallel = F, verbose = F)
  expect_output(str(tmp), "8 obs")
  expect_output(str(tmp), "10 variables")
})

test_that(" CTT summary ",{
  tmp <- CTT_summary(inference,items,verbose=F)
  expect_named(tmp, c("frequence","discrimination","alpha","digits"))
})

