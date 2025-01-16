<!-- README.md is generated from README.Rmd. Please edit that file -->

# RedcapLite

<!-- badges: start -->
<!-- badges: end -->

**RedcapLite: A Lightweight Interface for REDCap**

Overview: RedcapLite provides a streamlined interface for interacting with REDCap (Research Electronic Data Capture) via its API. Designed for flexibility and efficiency, this package is ideal for researchers and data managers who need efficient tools for handling REDCap data exports or connecting to the REDCap API for streamlined workflows.

**Key Features:**

- **Data Retrieval:** Fetch raw data directly from REDCap.

- **Preprocessing:** Add value labels, handle checkbox fields, and assign column attributes.

- **Project Support:** Suitable for both longitudinal and non-longitudinal projects.

- **Data Storage:** Save processed data in reusable formats.

##Installation

You can install the release version of RedcapLite from CRAN:

``` r
# install.packages("pak")
pak::pak("Waiguru254/RedcapLite")
```
**Install by `remotes` package**
The development version can be installed from GitHub after installing the remotes package:
This is a basic example which shows you how to solve a common problem:

``` r
install.packages("remotes") # Install the 'remotes' package if not already installed
remotes::install_github("Waiguru254/RedcapLite")
### Load the package
library(RedcapLite)

```

**Features**

- **Easy connection to REDCap projects via the API.**

- **Tools for preprocessing REDCap data.**

- **Automated generation of reports**:
  - Identify outliers.
  - Handle missing values.

- **Follow-up tracking**:
  - Manage and monitor identified data queries.

- **Lightweight and intuitive interface**:
  - Simplifies common REDCap workflows.
  
 **Example**

This is a basic example showing how to connect to a REDCap project and retrieve data:
```r
library(RedcapLite)
# Example: Connecting to a REDCap project
api_url <- "https://your-redcap-instance/api/"
api_token <- "YOUR_API_TOKEN"

data <- redcap_read(api_url = api_url, api_token = api_token)
head(data)
```
You'll need to provide your own REDCap API URL and token.

**Getting Help**

If you encounter any issues or have questions about the package, please file an issue on GitHub with a minimal reproducible example.

**About**

Package: RedcapLite

Authors: Waiguru Muriuki, 

Maintainer: Waiguru (waigurusamuel@gmail.com)

License: GPL-3

Depends: R (>= 4.0.0)

For more details about REDCap, visit REDCap.
