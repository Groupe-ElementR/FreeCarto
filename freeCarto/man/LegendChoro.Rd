% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/legends.R
\name{LegendChoro}
\alias{LegendChoro}
\title{LegendChoro}
\usage{
LegendChoro(pos = "topleft", legTitle = "Title of the legend",
  legTitleCex = 0.8, legValuesCex = 0.6, distr, cols, round = 2,
  nodata = TRUE, nodatalabel = "No data", frame = FALSE, symbol = "box")
}
\arguments{
\item{pos}{bottomleft, topleft, topright, bottomright, left, right, top, bottom, middle}

\item{legTitle}{Title of the legend}

\item{legTitleCex}{legTitleCex}

\item{legValuesCex}{legValuesCex}

\item{distr}{vector of classes}

\item{cols}{vector of colors}

\item{round}{round class values}

\item{nodata}{if TRUE, a white box 'no data' is drawn.}

\item{nodatalabel}{label for no data value. Default = "No data"}

\item{frame}{if TRUE, a frame is drawn}

\item{symbol}{'line' or 'box'}
}
\value{
plot
}
\description{
legend for chorpoleth layers
}
\details{
plot a legend
}
\examples{
data("nuts2006")
plot(nuts0.spdf)
rect(par()$usr[1], par()$usr[3], par()$usr[2], par()$usr[4], border = "black")
LegendChoro(pos = "bottomright", legTitle = "Title of the legend", legTitleCex = 0.8,
             legValuesCex = 0.6, distr = c(1,2,3,4,10.27,15.2),
             cols = carto.pal(pal1 = "orange.pal",n1 = 5), round =2,
             nodata = TRUE, nodatalabel = "No data available", frame = TRUE, symbol="box")
 LegendChoro(pos = "bottomleft", legTitle = "Title of the legend", legTitleCex = 0.8,
             legValuesCex = 0.6, distr = c(1,2,3,4,10,15.27),
             cols = carto.pal(pal1 = "orange.pal",n1 = 5), round =2,
             nodata = TRUE, nodatalabel = "No data available",
             frame = TRUE, symbol="line")
}

