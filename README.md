# Happiness model via Daylio

R code to examine the relationship between daily activities and overall rating of the day. The five level outcome variable is split into Great vs. not Great, and Great/Good vs. Mediocre/Bad/Awful, in order to see the effect on variables on having a good or great day and having a great day, to see if there are certain activities that contribute to the effect on mood at varying levels of goodness. Multivariable models are fitted in order to determine whether there are is an effect of activities on mood, whilst adjusting for the other activities.

### Set-up

```
install r-studio
```

Install the app:
* [Daylio](https://daylio.webflow.io/)

Start collecting data!

### Code

Run 'importData.R' first, then run 'greatGoodModel.R' or 'greatModel.R' depending on preference.

Edit 'importData.R' with the name of your exported Daylio data, and change the activities to reflect your chosen activity data.

## Built With

* [RStudio](http://www.rstudio.com/)
