test_that("REDCap logic is translated to R", {
  expect_equal(translate_logic("[sex] = '2'"), "`sex` == 2")
  expect_equal(translate_logic("[a] <> 'x' and [b] >= 3"), "`a` != \"x\" & `b` >= 3")
  expect_equal(translate_logic("[race(2)] = '1' or [age] < 18"), "`race___2` == 1 | `age` < 18")
  expect_equal(translate_logic("[x] = ''"), ".blank(`x`)")
  expect_equal(translate_logic("[baseline_arm_1][weight] > 0"), ".ev(\"baseline_arm_1\", \"weight\") > 0")
  expect_match(translate_logic("rounddown(datediff([dob],[d],\"y\"),0)"), "^\\.rounddown\\(\\.datediff\\(")
})

test_that("logic is evaluated row-wise with REDCap semantics", {
  d <- data.frame(record_id = c("1", "2", "3"), sex = c("1", "2", NA), weight = c(80, 60, NA),
                  height = c(200, 150, 170), stringsAsFactors = FALSE)
  expect_equal(eval_logic("[sex] = '2'", d), c(FALSE, TRUE, NA))
  expect_equal(eval_logic("[weight] = ''", d), c(FALSE, FALSE, TRUE))
  expect_equal(eval_logic("round([weight]/(([height]/100)^2),1)", d), c(20, 26.7, NA))
  expect_equal(eval_logic("if([weight] > 70, 1, 0)", d), c(1, 0, NA))
  expect_equal(eval_logic("sum([weight],[height])", d), c(280, 210, 170))
  expect_equal(eval_logic("", d), c(TRUE, TRUE, TRUE))
})

test_that("cross-event references look up the record's other event", {
  d <- data.frame(record_id = c("1", "1", "2", "2"), redcap_event_name = rep(c("base", "fu"), 2),
                  redcap_repeat_instrument = NA, w = c(70, 68, 90, 95), stringsAsFactors = FALSE)
  fu <- d[d$redcap_event_name == "fu", ]
  expect_equal(eval_logic("[w] - [base][w]", fu, d), c(-2, 5))
})

test_that("datediff handles units and sign", {
  d <- data.frame(id = "1", a = as.Date("2000-01-01"), b = as.Date("2010-01-01"))
  expect_equal(floor(eval_logic("datediff([a],[b],\"y\")", d)), 10)
  expect_equal(eval_logic("datediff([b],[a],\"d\",true)", d), -3653)
})
