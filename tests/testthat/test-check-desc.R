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

# check_desc_version ------------------------------------------------------

context("check_desc_version")

test_that(
  "test check_desc_version() passes on a DESCRIPTION with a correct Version field", {
    lst <- list()
    # Solution code not considered
    # Version of base pkg is same as version of R
    lst$DC_SCT <- "ex() %>% parse_desc() %>% check_has_desc_version(as.package_version(R.Version()))"
    lst$DC_CODE <- BASE_DESC_LINES
    output <- test_it(lst)
    passes(output)    
  }
)

test_that(
  "test check_has_desc_version() fails on a DESCRIPTION without the Version field", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_desc() %>% check_has_desc_version(as.package_version(R.Version()))"
    lst$DC_CODE <- BASE_DESC_LINES[!grepl("^Version", BASE_DESC_LINES)]
    output <- test_it(lst)
    fails(output)    
  }
)

test_that(
  "test check_has_desc_version() fails on a DESCRIPTION with an incorrect Version field", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_desc() %>% check_has_desc_version(as.package_version('0.0-1'))"
    lst$DC_CODE <- BASE_DESC_LINES
    output <- test_it(lst)
    fails(output)    
  }
)

# check_desc_date ------------------------------------------------------

context("check_desc_date")

test_that(
  "test check_desc_date() passes on a DESCRIPTION with a correct Date field", {
    lst <- list()
    # Solution code not considered
    # Default Date is today
    lst$DC_SCT <- "ex() %>% parse_desc() %>% check_desc_date())"
    lst$DC_CODE <- c(BASE_DESC_LINES, paste("Date:", Sys.Date()))
    output <- test_it(lst)
    passes(output)    
  }
)

test_that(
  "test check_desc_date() fails on a DESCRIPTION without the Date field", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_desc() %>% check_desc_date()"
    lst$DC_CODE <- BASE_DESC_LINES
    output <- test_it(lst)
    fails(output)    
  }
)

test_that(
  "test check_desc_date() fails on a DESCRIPTION with an incorrect Date field", {
    lst <- list()
    # Solution code not considered
    # Default Date is today
    lst$DC_SCT <- "ex() %>% parse_desc() %>% check_desc_date()"
    lst$DC_CODE <- c(BASE_DESC_LINES, paste("Date:", as.Date("1915-06-16")))
    output <- test_it(lst)
    fails(output)    
  }
)

# check_desc_authors_at_r -------------------------------------------------

context("check_desc_authors_at_r")

test_that(
  "test check_desc_authors_at_r() passes on a DESCRIPTION with a correct Authors@R field", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_desc() %>% check_desc_authors_at_r(as.person('R Core Team <R-core@r-project.org> [aut, cre]')))"
    lst$DC_CODE <- c(
      BASE_DESC_LINES, 
      paste("Authors@R:", as.person("R Core Team <R-core@r-project.org> [aut, cre]"))
    )
    output <- test_it(lst)
    passes(output)    
  }
)

test_that(
  "test check_desc_authors_at_r() fails on a DESCRIPTION without the Authors@R field", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_desc() %>% check_desc_authors_at_r(utils::as.person('R Core Team <R-core@r-project.org> [aut, cre]'))"
    lst$DC_CODE <- BASE_DESC_LINES
    output <- test_it(lst)
    fails(output)    
  }
)

test_that(
  "test check_desc_authors_at_r() fails on a DESCRIPTION with an incorrect Authors@R field", {
    lst <- list()
    # Solution code not considered
    lst$DC_SCT <- "ex() %>% parse_desc() %>% check_desc_authors_at_r(utils::as.person('R Core Team <R-core@r-project.org> [aut, cre]'))"
    lst$DC_CODE <- c(BASE_DESC_LINES, paste("Authors@R:", as.person('Richie Cotton <richie@datacamp.com> [aut, cre]')))
    output <- test_it(lst)
    fails(output)    
  }
)


