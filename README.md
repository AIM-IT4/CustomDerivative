
# CustomDerivative

A versatile R package for creating and pricing custom derivatives.

## Installation

You can install the `CustomDerivative` package from GitHub using the following command:

```R
devtools::install_github("YourGitHubUsername/CustomDerivative")
```

## Usage

Load the package and create a new derivative:

```R
library(CustomDerivative)

derivative <- CustomDerivative$new(100, 100, 1, 0.2, 0.05, function(x) max(x - 100, 0))
price <- derivative$price()
```

## License

This project is licensed under the MIT License.

## Authors

- **Amit Kumar Jha** - *Initial work* - [YourGitHubUsername](https://github.com/YourGitHubUsername)

