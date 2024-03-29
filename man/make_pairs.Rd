% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_pairs.R
\name{make_pairs}
\alias{make_pairs}
\alias{sets}
\alias{make_sets}
\alias{make_pairs_wf_source}
\title{Combinations and permutations of record-sets.}
\usage{
sets(n, r, permutations_allowed = TRUE, repeats_allowed = TRUE)

make_sets(
  x,
  r,
  strata = NULL,
  permutations_allowed = TRUE,
  repeats_allowed = TRUE
)

make_pairs(
  x,
  strata = NULL,
  repeats_allowed = TRUE,
  permutations_allowed = FALSE
)

make_pairs_wf_source(..., data_source = NULL)
}
\arguments{
\item{n}{\code{[integer]}. Size of Vector.}

\item{r}{\code{[integer]}. Number of elements in a set.}

\item{permutations_allowed}{\code{[logical]}. If \code{TRUE}, permutations of the same set are included.}

\item{repeats_allowed}{\code{[logical]}. If \code{TRUE}, repeat values are included in each set.}

\item{x}{\code{[atomic]}. Vector.}

\item{strata}{Subsets of \code{x}. Blocking attribute. Limits the creation of combinations or permutations to those from the same \code{strata}.}

\item{...}{Arguments passed to \code{\link{make_pairs}}.}

\item{data_source}{\code{[character]}. Data source identifier. Limits the creation of combinations or permutations to those from a different \code{data_source}}
}
\value{
A \code{list} of a vector's elements and corresponding indexes.
}
\description{
Combinations and permutations of record-sets.
}
\details{
\bold{\code{set()}} - Create \code{r}-set combinations or permutations of \code{n} observations.

\bold{\code{make_set()}} - Create \code{r}-set combinations or permutations of vector \code{x}.

\bold{\code{make_pairs()}} - Create \code{2}-set combinations or permutations of vector \code{x}.

\bold{\code{make_pairs_wf_source()}} - Create \code{2}-set combinations or permutations of vector \code{x} that are from different sources (\code{data_source}).
}
\examples{

sets(4, 2)
sets(4, 2, repeats_allowed = FALSE, permutations_allowed = FALSE)
make_sets(month.abb[1:4], 2)
make_sets(month.abb[1:4], 3)

make_pairs(month.abb[1:4])
make_pairs(month.abb[1:4], strata = c(1, 1, 2, 2))
make_pairs_wf_source(month.abb[1:4], data_source = c(1, 1, 2, 2))

}
\seealso{
\code{\link{eval_sub_criteria}}
}
