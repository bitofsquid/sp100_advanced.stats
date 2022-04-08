# sp100_top20

This project builds on the initial work shown in the sp100_basic.stats repo posted previously. Here single and multi-factor regressions are computed for the largest 20 firms in the S&P 100 Index as well as for the Fidelity Magellen equity mutual fund.

The script uses the quantmod and Quandl packages to gather data on the 20 firms and leverages base R functionality to perform the regression testing and plot some interesting data. Tables containing summary statistics can be seen by running the script itself. 

Return data is from Yahoo for the 20 firms and from Ken French's data library for the market and other relevant equity factors used in the regressions.

Here is a plot of each ticker's position relative to the Security Market Line.

![test](https://github.com/bitofsquid/sp100_top20/blob/master/Rplot.png)

And a simple plot illustrating a time series of monthly returns for the Fidelity Magellen mutual fund.
![test](https://github.com/bitofsquid/sp100_top20/blob/master/Rplot01.png)
