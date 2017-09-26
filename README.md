# PAAC(Pensions Annual Allowance Calculator) application

The business logic for calculations were unfortunately not clearly set out at the start of the
development of this service. Consequently the code base evolved and became unnecessarily complex.
This is a fork of the original service to simplify and clarify the calculations.

# Old Notes

[![Build Status](https://travis-ci.org/hmrc/paac.svg)](https://travis-ci.org/hmrc/paac) [ ![Download](https://api.bintray.com/packages/hmrc/releases/paac/images/download.svg) ](https://bintray.com/hmrc/releases/paac/_latestVersion)

# Prerequisites

You will require [SBT](http://www.scala-sbt.org/download.html) to be installed on your machine.
At present there are no other dependencies required by this service.

# Building

On a command line simply use `sbt clean package`

# Running

If you are running the service without the frontend you can just use `sbt run`, however it's most likely that
this service is used with the frontend which will expect this service to be available on port 9443 so run using
either `sbt "run 9443"` or `sbt run -Dhttp.port=9443`

## Using the service

You can use [curl](https://curl.haxx.se) to send JSON directly to the service.
There is a single REST endpoint on http://127.0.0.1:9443/paac/calculate which expects a JSON array of objects
of the format where amounts are in pence:

```
{"contributions": [
{
"taxPeriodStart": {"year": 2013, "month": 4, "day": 6},
 "taxPeriodEnd": {"year": 2014, "month": 4, "day": 5},
 "amounts":{"definedBenefit":300000,"moneyPurchase":0}
},
{
"taxPeriodStart": {"year": 2014, "month": 4, "day": 6},
 "taxPeriodEnd": {"year": 2015, "month": 4, "day": 5},
 "amounts":{"definedBenefit":1200000,"moneyPurchase":0}
}
],
"startFromYear": 2012,
"missingYearsAreRegistered": false }
```

## Trigger in Period 1
```
{"contributions": [
{
"taxPeriodStart": {"year": 2015, "month": 4, "day": 6},
 "taxPeriodEnd": {"year": 2015, "month": 6, "day": 30},
 "amounts":{"definedBenefit":3000,"moneyPurchase":0,"income":0,"triggered":false}
},
{
"taxPeriodStart": {"year": 2015, "month": 7, "day": 1},
 "taxPeriodEnd": {"year": 2015, "month": 7, "day": 8},
 "amounts":{"definedBenefit":0,"moneyPurchase":11000,"income":0,"triggered":true}
},
{
"taxPeriodStart": {"year": 2015, "month": 7, "day": 9},
 "taxPeriodEnd": {"year": 2016, "month": 4, "day": 5},
 "amounts":{"definedBenefit":0,"moneyPurchase":12000,"income":0,"triggered":true}
}
],
"startFromYear": 2012,
"missingYearsAreRegistered": false }
```

## Trigger in Period 2
```
{"contributions": [
{
"taxPeriodStart":{"year": 2015, "month": 4, "day": 6},
"taxPeriodEnd":{"year": 2015, "month": 7, "day": 8},
"amounts":{"definedBenefit":3000,"moneyPurchase":11000,"income":0,"triggered":false}
},
{
"taxPeriodStart":{"year": 2015, "month": 7, "day": 9},
"taxPeriodEnd":{"year": 2015, "month": 10, "day": 31},
"amounts":{"definedBenefit":6000,"moneyPurchase":6000,"income":0,"triggered":false}
},
{
"taxPeriodStart":{"year": 2015, "month": 11, "day": 1},
"taxPeriodEnd":{"year": 2016, "month": 4, "day": 5},
"amounts":{"definedBenefit":0,"moneyPurchase":12000,"income":0,"triggered":true}
}
],
"startFromYear": 2012,
"missingYearsAreRegistered": false }
```

### Group 2 testing (Defined contribution/money purchase only example)
```
{"contributions": [
{
"taxPeriodStart":{"year": 2015, "month": 4, "day": 6},
"taxPeriodEnd":{"year": 2015, "month": 7, "day": 8},
"amounts":{"definedBenefit":null,"moneyPurchase":11000,"income":0,"triggered":false}
},
{
"taxPeriodStart":{"year": 2015, "month": 7, "day": 9},
"taxPeriodEnd":{"year": 2015, "month": 10, "day": 31},
"amounts":{"definedBenefit":null,"moneyPurchase":6000,"income":0,"triggered":false}
},
{
"taxPeriodStart":{"year": 2015, "month": 11, "day": 1},
"taxPeriodEnd":{"year": 2016, "month": 4, "day": 5},
"amounts":{"definedBenefit":0,"moneyPurchase":12000,"income":0,"triggered":true}
}
],
"startFromYear": 2012,
"missingYearsAreRegistered": false }
```

### Parameters
#### startFromYear
The startFromYear parameter is used to control when calculations (namely carry forwards) should begin from. To obtain results similar to the existing extended calculator this parameter should be set to `2008` and missingYearsAreRegistered should be `true`.

#### missingYearsAreRegistered
The missingYearsAreRegistered parameter is used to control the carry forward portion of the calculations. If set to true any years not supplied as contributions it will be assumed that the 'user' is registered in a pension for those years and therefore carry forward applies. This should be set to true to mimic the existing extended calculator. To prevent carry forward of allowances for missing years then set this parameter to false.

### CURL Example

```
curl -H "Content-Type: application/json" -d '{"contributions": [{"taxPeriodStart":{"year":2014,"month":4,"day":6},"taxPeriodEnd":{"year":2015,"month":4,"day":5}, "amounts":{"definedBenefit":3000,"moneyPurchase":0}},{"taxPeriodStart":{"year":2013,"month":4,"day":6},"taxPeriodEnd":{"year":2014,"month":4,"day":5}, "amounts":{"definedBenefit":8000,"moneyPurchase":0}}], "startFromYear": 2012, "missingYearsAreRegistered": false }' -X POST -vvvv http://127.0.0.1:9443/paac/calculate
```

### Peculiarities

Please note that 2015 pension input period is unique in that it comprises of two periods (pre and post alignment). They are specified by
using the correct start end dates in the input JSON e.g.

```
{"contributions": [
{ /* 2015 Period 1 */
"taxPeriodStart": {"year": 2015, "month": 4, "day": 6},
 "taxPeriodEnd": {"year": 2015, "month": 7, "day": 8},
 "amounts":{"definedBenefit":3000,"moneyPurchase":0}
},
{ /* 2015 Period 2 */
"taxPeriodStart": {"year": 2015, "month": 7, "day": 9},
 "taxPeriodEnd": {"year": 2016, "month": 4, "day": 5},
 "amounts":{"definedBenefit":12000,"moneyPurchase":0}
}
],
"startFromYear": 2012,
"missingYearsAreRegistered": false }
```

# Debugging

Debugging requires two steps as follows:

1. On a command line pass in the -jvm-debug <port> to sbt like so: `sbt -jvm-debug 5005 "run 9443"`
2. In an editor that supports remote debugging start the remote debug on port 5005 with listening to socket.
    - For IntelliJ this is accomplished by selecting "Edit Configurations..." in the toolbar
    - Click '+' button to add new 'Remote' configuration
    - Ensure 'socket', and 'attach' are selected
    - Set host to either localhost or to 0.0.0.0 and port to 5005

# Testing

## Coverage Report
To run with coverage `sbt clean coverage test` and an HTML report will be available in the target/scala-2.11/scoverage-report/index.html
directory.

## Integration Testing

Run using `sbt it:test`

## Unit Testing

Run using `sbt test`

Calculation tests are driven by text files containing a table of input and expected output values. If you find a new test case simply create a new file in the one of the `group1`, `group2`, or `group3` folders under `test\assets\calculators` with the appropriate format and it will be executed on the next `sbt test` run.

### Turning on Full Stack Traces

Turn on full stacktrace in sbt console using `set testOptions in "paac" += Tests.Argument("-oF")`

# Other

Scalastyle is enabled for this project. To run use `sbt scalastyle` on the command line.
