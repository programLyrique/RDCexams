test_that("normalization of IST variants", {
  expect_equal(normalize_school_name("IST plop"), "INSTITUT PLOP")
  expect_equal(normalize_school_name("I.S.T. plop"), "INSTITUT PLOP")
  expect_equal(normalize_school_name("plop iS.T"), "PLOP INSTITUT")
  expect_equal(normalize_school_name(" IST. plop"), "INSTITUT PLOP")
  expect_equal(normalize_school_name(" IST plop "), "INSTITUT PLOP")
})


test_that("normalization of INST variants", {
    expect_equal(normalize_school_name("INST plop"), "INSTITUT PLOP")
    expect_equal(normalize_school_name("I.N.S.T. plop"), "INSTITUT PLOP")
    expect_equal(normalize_school_name("plop iN.ST"), "PLOP INSTITUT")
    expect_equal(normalize_school_name(" INST. plop"), "INSTITUT PLOP")
    expect_equal(normalize_school_name(" INST plop "), "INSTITUT PLOP")
})
