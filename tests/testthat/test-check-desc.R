BASE_DESC_LINES <- readLines(system.file("DESCRIPTION"))
TESTWHAT_DESC_LINES <- readLines(system.file("DESCRIPTION", package = "testwhat"))

# check_has_desc_element --------------------------------------------------

context("check_has_desc_element")

test_that(
  "test check_has_desc_element() passes on a DESCRIPTION with the required field", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_desc() %>% check_has_desc_element('Package')"
    lst$DC_CODE <- BASE_DESC_LINES
    output <- test_it(lst)
    passes(output)    
  }
)

