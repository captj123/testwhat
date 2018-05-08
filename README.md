# testwhat

[![Build Status](https://api.travis-ci.org/datacamp/testwhat.svg?branch=master)](https://travis-ci.org/datacamp/testwhat)
[![codecov.io](https://codecov.io/github/datacamp/testwhat/coverage.svg?branch=master)](https://codecov.io/github/datacamp/testwhat?branch=master)
[![Rdocs](http://www.rdocumentation.org/badges/version/testwhat)](http://www.rdocumentation.org/packages/testwhat)

The `testwhat` package helps you to write Submission Correctness Tests (SCTs) for interactive R exercises on the DataCamp platform.

- If you are new to teaching on DataCamp, check out https://authoring.datacamp.com.
- If you want to learn what SCTs are and how they work, visit https://authoring.datacamp.com/courses/sct.html.
- For a complete overview of all functionality inside `testwhat` and articles about what to use when, consult https://datacamp.github.io/testwhat.

## Installation and tests

```R
library("devtools")
install_github("datacamp/testwhat")
```

`testwhat` currently depends on the proprietary `RBackend` and `RCompletion` packages to run tests.

```R
devtools::test()
```

For details, questions and suggestions, [contact us](mailto:content-engineering@datacamp.com).

