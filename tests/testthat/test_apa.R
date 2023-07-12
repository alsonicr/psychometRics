
test_that(" APA format ", {
  expect_equal(apa.n(34.036, d=2), "34.04")
  expect_equal(apa.n(34.036, d=3), "34.036")
  expect_equal(apa.n(34.036, d=4), "34.0360")
  expect_equal(apa.n(0.001, d=1), "0.0")
  expect_equal(apa.n(0.001, d=2), "0.00")
  expect_equal(apa.n(0.001, d=3), "0.001")
  expect_equal(apa.n(0, d=2, p=T), ".00")
  expect_equal(apa.n(0.001, d=2,p=T),".00")
  expect_equal(apa.n(0.001, d=3,p=T),".001")
  expect_equal(apa.n(-0.10, d=2,oto=T),"-.10")
  expect_equal(apa.n(-0.10, d=3,oto=T),"-.100")
})
