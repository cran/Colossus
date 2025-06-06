% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Utility.R
\name{Time_Since}
\alias{Time_Since}
\title{Automates creating a date since a reference column}
\usage{
Time_Since(df, dcol0, tref, col_name, units = "days")
}
\arguments{
\item{df}{a data.table containing the columns of interest}

\item{dcol0}{list of ending month, day, and year}

\item{tref}{reference time in date format}

\item{col_name}{vector of new column names}

\item{units}{time unit to use}
}
\value{
returns the updated dataframe
}
\description{
\code{Time_Since} generates a new dataframe with a column containing time since a reference in a given unit
}
\examples{
library(data.table)
m0 <- c(1, 1, 2, 2)
m1 <- c(2, 2, 3, 3)
d0 <- c(1, 2, 3, 4)
d1 <- c(6, 7, 8, 9)
y0 <- c(1990, 1991, 1997, 1998)
y1 <- c(2001, 2003, 2005, 2006)
df <- data.table::data.table(
  "m0" = m0, "m1" = m1,
  "d0" = d0, "d1" = d1,
  "y0" = y0, "y1" = y1
)
tref <- strptime("3-22-1997", format = "\%m-\%d-\%Y", tz = "UTC")
df <- Time_Since(df, c("m1", "d1", "y1"), tref, "date_since")

}
\seealso{
Other Data Cleaning Functions: 
\code{\link{Check_Dupe_Columns}()},
\code{\link{Check_Trunc}()},
\code{\link{Check_Verbose}()},
\code{\link{Convert_Model_Eq}()},
\code{\link{Correct_Formula_Order}()},
\code{\link{Date_Shift}()},
\code{\link{Def_Control}()},
\code{\link{Def_Control_Guess}()},
\code{\link{Def_model_control}()},
\code{\link{Def_modelform_fix}()},
\code{\link{Event_Count_Gen}()},
\code{\link{Event_Time_Gen}()},
\code{\link{Joint_Multiple_Events}()},
\code{\link{Replace_Missing}()},
\code{\link{factorize}()},
\code{\link{factorize_par}()},
\code{\link{gen_time_dep}()},
\code{\link{interact_them}()}
}
\concept{Data Cleaning Functions}
