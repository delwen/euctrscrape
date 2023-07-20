# euctrscrape

## To install and use

Get `euctrscrape` from Github:

```
install.packages("devtools")
library(devtools)
install_github("delwen/euctrscrape")
library(euctrscrape)
```

`euctrscrape` downloads information for a given trial from the EUCTR.

It takes a vector of EUCTR trial IDs as input, and returns the following:

#### 1) Information from the protocol for each trial:
- Full title
- Member state concerned
- Trial status
- Sponsor name
- Sponsor status
- Sponsor country
- Funder
- Registration date (date first entered on the EudraCT database)
- Additional identifiers

If a trial involves >1 country protocol, `euctrscrape` returns all fields
for each country protocol.

#### 2) Information from the results page for each trial (if available):
- Additional identifiers

`euctrscrape` returns basic trial information and found identifiers separately.
The EUCTR ID of any trials that are unresolved are also returned. See examples
in the `data` folder.

See `euctr_query.R` for an example of how to use `euctrscrape`.
