# circle-analysis

R package boiler idea for network analysis using `rtweet` and visualization using graph technique. This package shall be improved in the future for more complex topic association for certain users and other metrices to analyze circle. The current one that is being implemented is number of mentions within user.

## `rtweet` Authorization

To start using this function, you'll need to generate `rtweet` authentication with the following syntax:

```
library(rtweet)

create_token(app = "your app name",
             consumer_key = "enter your API key",
             consumer_secret = "enter your API secret",
             access_token = "enter your access token",
             access_secret = "enter your access token secret")
```

Head over to <developers.twitter.com> to gain your application access.

## Using `get_circle()`

The function received username parameters.

```
get_circle("username")
```

To install use the following command:

```
devtools::install_github("tiaradwiputri/circle-analysis")
```