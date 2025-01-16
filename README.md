<!-- README.md is generated from README.Rmd. Please edit that file -->

# RedcapLite

<!-- badges: start -->
<!-- badges: end -->

**RedcapLite: A Lightweight Interface for REDCap**

RedcapLite is a streamlined interface designed for interacting with REDCap (Research Electronic Data Capture) through its API. It offers researchers and data managers a flexible and efficient solution to handle various tasks, such as exporting data from REDCap or integrating with the API for smoother workflows.

Built with adaptability and performance in mind, this package caters to the needs of those managing complex datasets or requiring robust tools for optimizing REDCap-related processes. RedcapLite simplifies and enhances the user experience, making it an ideal choice for improving data management efficiency.

When setting preprocess_data = TRUE, the function automatically enriches your dataset by adding variable and value labels derived from the REDCap data dictionary. This feature seamlessly integrates metadata, ensuring that columns are accurately labeled for improved clarity and analysis. Additionally, any updates made to the codebook on the REDCap server are instantly implemented, keeping your dataset aligned with the latest project definitions. This dynamic approach eliminates manual effort, enhances data interpretability, and ensures consistency with the most up-to-date project metadata.

Checkbox fields in REDCap often pose challenges for data processing in other packages, as they generate multiple binary columns for each option without clear labels, making the data difficult to interpret and analyze. Our implementation addresses this by creating a compact, well-organized column that consolidates multiple-choice responses into a single, human-readable field while simultaneously generating one-hot encoded columns for detailed analysis. Each column is properly labeled, ensuring clarity and alignment with the REDCap data dictionary. This dual approach simplifies downstream processing, making it easier to handle checkbox data efficiently and accurately without losing important metadata.


**Key Features:**

- **Data Retrieval:** Fetch raw data directly from REDCap.

- **Preprocessing:** Add value labels, handle checkbox fields, and assign column attributes.

- **Project Support:** Suitable for both longitudinal and non-longitudinal projects.

- **Data Storage:** Save processed data in reusable formats.

**Installation**

To install the release version of RedcapLite from CRAN with ease, you can use the pak package, which simplifies dependency management and package installation in R. Begin by ensuring the pak package is installed, and then use it to fetch and install RedcapLite directly from its repository. This approach not only ensures you get the latest release but also resolves dependencies efficiently for a seamless setup. Here’s how you can do it:

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

data <- readcapdata(api_url = api_url, api_token = api_token)
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

