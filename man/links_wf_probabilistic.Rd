% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/record_group.R
\name{links_wf_probabilistic}
\alias{links_wf_probabilistic}
\alias{prob_score_range}
\title{Probabilistic record linkage}
\usage{
links_wf_probabilistic(
  attribute,
  blocking_attribute = NULL,
  cmp_func = diyar::exact_match,
  cmp_threshold = 0.95,
  probabilistic = TRUE,
  m_probability = 0.95,
  score_threshold = 1,
  id_1 = NULL,
  id_2 = NULL,
  ...
)

prob_score_range(attribute, m_probability = 0.95)
}
\arguments{
\item{attribute}{\code{[list]}. Attributes to compare.}

\item{blocking_attribute}{\code{[atomic]}. Subsets of the dataset.}

\item{cmp_func}{\code{[list|function]}. String comparators for each \code{attribute}. See \code{Details}.}

\item{cmp_threshold}{\code{[list|numeric|\link{number_line}]}. Weight-thresholds for each \code{cmp_func}. See \code{Details}.}

\item{probabilistic}{\code{[logical]}. If \code{TRUE}, scores are assigned base on Fellegi-Sunter model for probabilistic record linkage. See \code{Details}.}

\item{m_probability}{\code{[list|numeric]}. The probability that a match from the string comparator is actually from the same entity.}

\item{score_threshold}{\code{[numeric|\link{number_line}]}. Score-threshold for linked records. See \code{Details}.}

\item{id_1}{\code{[list|numeric]}. One half of a specific pair of records to check for match weights and score-thresholds.}

\item{id_2}{\code{[list|numeric]}. One half of a specific pair of records to check for match weights and score-thresholds.}

\item{...}{Arguments passed to \bold{\code{links}}}
}
\value{
\code{\link[=pid-class]{pid}}; \code{list}
}
\description{
A specific use case of \code{links} for probabilistic record linkage.
}
\details{
\code{links_wf_probabilistic} is a wrapper function of \code{\link{links}} for probabilistic record linkage.
Its implementation is based on Fellegi and Sunter (1969) model for deciding if two records belong to the same entity.

In summary, record pairs are created and categorised as matches and non-matches (\code{cmp_func}).
Two probabilities (\code{m} and \code{u}) are then estimated for each record pair to score matches and non-matches.
The \code{m}-probability is the probability that matched records are actually from the same entity i.e a true match,
while \code{u}-probability is the probability that matched records are not from the same entity i.e. a false match.
\code{m}-probabilities must be supplied but \code{u}-probabilities are calculated for each value of an \code{attribute}.
This is calculated as the frequency of each value in the dataset.
Record pairs whose total score are above a certain threshold (\code{score_threshold}) are assumed to belong to the same entity.

Agreement (match) and disagreement (non-match) scores are calculated as described by Asher et al. (2020).

For each record pair, an agreement for attribute \eqn{i} is calculated as;

\deqn{\log_{2}(m_{i}/u_{i})}{log_2 (m_i / u_i)}

For each record pair, a disagreement score for attribute \eqn{i} is calculated as;

\deqn{\log_{2}((1-m_{i})/(1-u_{i}))}{log_2 ((1-m_i) / (1-u_i))}

where \eqn{m_{i}}{m_i} and \eqn{u_{i}}{u_i} are the \code{m} and \code{u}-probabilities for each value of attribute \eqn{i}.

Missing data (\code{NA}) are categorised as non-matches and assigned a \code{u}-probability of \code{0}.

By default, matches and non-matches for each \code{attribute} are determined as an \code{\link{exact_match}} with a binary outcome.
String comparators can also be used with thresholds (\code{cmp_threshold}) for each similarity score.
If \code{probabilistic} is \code{FALSE},
the sum of all similarity scores is used as the \code{score_threshold} instead of deriving one from the \code{m} and \code{u}-probabilities.

\code{links_wf_probabilistic} requires a \code{score_threshold} in advance of the linkage process.
This differs from the typical approach where a \code{score_threshold} is selected after the linkage process,
following a review of all calculated scores.
To help with this, \code{prob_score_range} will return the range of scores attainable for a given set of attributes.
Additionally, \code{id_1} and \code{id_2} can be used to link specific records pairs, aiding the review of potential scores.

A \code{blocking_attribute} can be used to reduce processing time by restricting comparisons to subsets of the dataset.
}
\examples{
# Using exact matches
dfr <- missing_staff_id[c("staff_id",  "initials",
                          "hair_colour", "branch_office")]
score_range <- prob_score_range(attribute = as.list(dfr))
prob_pids1 <- links_wf_probabilistic(attribute = as.list(dfr),
                                     score_threshold = score_range$minimum_score)
prob_pids1

# Using other logical tests e.g. string comparators
# For example, matching last word in `hair_colour` and `branch_office`
last_word_wf <- function(x) tolower(gsub("^.* ", "", x))
last_word_cmp <- function(x, y) last_word_wf(x) == last_word_wf(y)
prob_pids2 <- links_wf_probabilistic(attribute = as.list(dfr),
                                     cmp_func = c(diyar::exact_match,
                                                  diyar::exact_match,
                                                  last_word_cmp,
                                                  last_word_cmp),
                                     score_threshold = score_range$mid_scorce)
prob_pids2

# Results for specific record pairs
prob_pids3 <- links_wf_probabilistic(attribute = as.list(dfr),
                                     cmp_func = c(diyar::exact_match,
                                                  diyar::exact_match,
                                                  last_word_cmp,
                                                  last_word_cmp),
                                     score_threshold = score_range$mid_scorce,
                                     id_1 = c(1, 1, 1),
                                     id_2 = c(6, 7, 4))
prob_pids3
}
\references{
Fellegi, I. P., & Sunter, A. B. (1969). A Theory for Record Linkage. \emph{Journal of the Statistical Association}, 64(328), 1183–1210. https://doi.org/10.1080/01621459.1969.10501049

Asher, J., Resnick, D., Brite, J., Brackbill, R., & Cone, J. (2020). An Introduction to Probabilistic Record Linkage with a Focus on Linkage Processing for WTC Registries. \emph{International journal of environmental research and public health}, 17(18), 6937. https://doi.org/10.3390/ijerph17186937.
}
\seealso{
\code{\link{links}}; \code{\link{episodes}}; \code{\link{partitions}}; \code{\link{predefined_tests}}; \code{\link{sub_criteria}}
}
