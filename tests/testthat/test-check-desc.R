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

test_that(
  "test check_has_desc_element() fails on a DESCRIPTION without the required field", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_desc() %>% check_has_desc_element('PACKAGE')"
    lst$DC_CODE <- BASE_DESC_LINES
    output <- test_it(lst)
    fails(output)    
  }
)

# check_desc_element_matches ----------------------------------------------

context("check_desc_element_matches")

test_that(
  "test check_desc_element_matches() passes on a DESCRIPTION with a matching field", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_desc() %>% check_desc_element_matches('License', '^Part of R \\d.\\d.\\d$')"
    lst$DC_CODE <- BASE_DESC_LINES
    output <- test_it(lst)
    passes(output)    
  }
)

test_that(
  "test check_desc_element_matches() passes on a DESCRIPTION with a fixed-matching field", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_desc() %>% check_desc_element_matches('Title', 'The R Base Package', fixed = TRUE)"
    lst$DC_CODE <- BASE_DESC_LINES
    output <- test_it(lst)
    passes(output)    
  }
)

test_that(
  "test check_desc_element_matches() fails on a DESCRIPTION without the required field", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_desc() %>% check_desc_element_matches('PACKAGE', 'base', fixed = TRUE)"
    lst$DC_CODE <- BASE_DESC_LINES
    output <- test_it(lst)
    fails(output)    
  }
)

test_that(
  "test check_desc_element_matches() fails on a DESCRIPTION with a non-matching field", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_desc() %>% check_desc_element_matches('Description', 'Army bases, bass fishing, and bass drums', fixed = TRUE)"
    lst$DC_CODE <- BASE_DESC_LINES
    output <- test_it(lst)
    fails(output)    
  }
)


