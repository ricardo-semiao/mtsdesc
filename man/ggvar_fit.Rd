% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggvar_fit.R
\name{ggvar_fit}
\alias{ggvar_fit}
\title{Plot VAR fitted values}
\usage{
ggvar_fit(
  x,
  series = NULL,
  index = NULL,
  args_aes = list(),
  args_line = list(),
  args_labs = list(),
  args_facet = list(),
  ...
)
}
\arguments{
\item{x}{A "varest" object to get fitted values from.}

\item{series}{A character vector with series (variables) to  consider. Defaults to all (\code{NULL}).}

\item{index}{A vector of labels to the x-axis, normally dates. Must have length equal to \code{x$obs}. Defaults to a integer sequence.}

\item{args_aes}{Defines aesthetics to differentiate the data. A named list of aesthetics names (\code{*}) -- arguments passed to \verb{ggplot2::scale_*_manual}. See more in the 'Customization' section.}

\item{args_line}{Additional arguments passed to \link[ggplot2:geom_path]{geom_line} (respectively). See more in the 'Customization' section.}

\item{args_labs}{Additional arguments passed to \link[ggplot2:labs]{labs}. If an empty list, will be changed to default values.}

\item{args_facet}{Additional arguments passed to the faceting engine used.}

\item{...}{Arguments passed to methods, see the 'Methods' section.}
}
\value{
A \link[ggplot2:ggplot]{ggplot}.
}
\description{
Plots fitted values of a VAR model, versus the actual values.
\code{ggvar_fit} Plots each serie in a facet. \code{ggvar_fit_colored}
plots all in the same graph, each with a different color.
}
\details{
\subsection{Customization}{

The graph can be customized both with the 'static' arguments passed to each layer -- using the \verb{args_*} arguments --, and, if applicable, the 'dynamic' aesthetics -- using the \code{args_aes} argument.

The \code{args_aes} is a list with '* = arguments to \verb{scale_*_manual} \
elements, where '*' represents the name of an aesthetic to apply to the \
data. View \code{vignette('ggplot2-specs', 'ggplot2')} to see the available \
aesthetics.

After built, the result can be further customized as any ggplot, adding or overwriting layers with the \link[ggplot2:gg-add]{ggplot's +}. It is useful to understand the data and the mappings coded by the package, using the function \link{get_gg_info}.

See \code{vignette('customizing-graphs')} for more details.
}

\subsection{Methods}{

The data from \code{x} is extracted with the generic function mtsdesc:::fit_setup. Each class conditions an external function to pass the \code{...} arguments to. Below there is a list with all the currently implemented classes:
\itemize{
\item Class \code{'varest'}: passed to nothing.
}
}
}
\examples{
x <- vars::VAR(freeny[-2])
ggvar_fit(x, args_facet = list(scales = "free_y"))

}
\seealso{
Other historic values plots: 
\code{\link{ggvar_history}()},
\code{\link{ggvar_predict}()}
}
\concept{historic values plots}
