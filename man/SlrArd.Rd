\name{SlrArd}
\alias{SlrArd}
\title{
  Sparse Logistic Regression
}
\description{
  Sparse logistic regression (SLR) with automatic relevance
  determination (ARD).
}
\usage{
SlrArd(T, X, bias = TRUE, method = c("VB", "VBMacKay", "PX-VB"), control = list(), check.lb=TRUE, a0 = 0, b0 = 0, mu0 = 0, xi0 = 1)
}
\arguments{
  \item{T}{
    A vector of binary categorical dependent variable.
  }
  \item{X}{
    A matrix, each row of which is a vector of independent variables.
  }
  \item{bias}{
    A logical value specifying whether to include intercept or not.
  }
  \item{method}{
    The method to be used.
    See \sQuote{Details}.
  }
  \item{control}{
    A list of control parameters.
    See \sQuote{Details}.
  }
  \item{check.lb}{
    A logical value whether to check the validity of parameter updates.
    Defaults to TRUE.
    \cr
  }
  Hyper parameters:
  \item{a0}{
    Shape of Gamma distribution.
  }
  \item{b0}{
    Rate of Gamma distribution.
    \cr
  }
  Initial values:
  \item{mu0}{
    Coefficients.
  }
  \item{xi0}{
    Variational parameter.
  }
}
\details{
  Some independent variables can be pruned in each VB-EM iteration and
  never be used for successive iterations when irrelevance of the
  variables exceed a threshold determined by
  \code{control$pruning}. Explicitly set \code{control$pruning = Inf} to
  prevent the pruning.

  Method \sQuote{"VB"} is a variational Bayesian method that is
  robust. See Bishop (2006).

  Method \sQuote{"VBMacKay"} is basically same as \sQuote{"VB"} but some
  parameters are  updated based on method of MacKay (1992). Global
  convergence has not been proven but this algorithm will be faster.

  Method \sQuote{"PX-VB"} based on the Parameter eXpanded VB method
  proposed in Qi and Jaakkola (2007). Global convergence is proven and
  could be faster than \sQuote{"VB"}.

  The \command{control} argument is a list that can supply any of the
  following components:

  \itemize{
    \item pruning: threshold of independent variables pruning. No
    variables are pruned if \sQuote{Inf}. Defaults to \sQuote{1e+8}.
  }
  See \command{?optim} for meanings of the following control parameters.
  \itemize{
    \item maxit: Defaults to \sQuote{10^5}.
    \item reltoj: Defaults to \sQuote{sqrt(.Machine$double.eps)}.
    \item trace: Defaults to \sQuote{TRUE}.
    \item REPORT: Defaults to \sQuote{floor(control$maxit / 20)}.
  }
}
\value{
  \item{coefficients}{
    A named vector of coefficients.
  }
  \item{irrelevance}{
    A named vector of irrelevance.
  }
  \item{iterations}{
    A number of iterations.
  }
  \item{converged}{
    A logical value giving whether the iterations converged or not.
  }
  \item{lower.bound}{
    The lower bound of the marginal log-likelihood minus a constant
    term.
  }
  \item{method}{
    The method used.
  }
  \item{lb.diff}{
    The difference of the lower bounds in each update.
    This exists when \command{check.lb = TRUE}.
  }
  \item{fitted.values}{
    The fitted values.
  }
  \item{residuals}{
    The residuals, that is \command{T} minus fitted values.
  }
}
\references{
  Bishop, C. M. (2006) Pattern recognition and machine
  learning. Springer.

  MacKay, D. J. C. (1992) Bayesian interpolation. _Neural Computation_,
  *4*(3), 415-447.

  Qi, Y. and Jaakkola, T. S. (2007) Parameter expanded variational
  Bayesian methods. _Advances in Neural Information Processing Systems_,
  *19*, 1097.
}
\author{
  Hiroshi Saito \email{die.saito@gmail.com}
}
\examples{
data(iris)

tmp <- iris[iris$Species != 'versicolor',]
T <- tmp$Species == 'setosa'
X <- as.matrix(tmp[,1:4])

res <- SlrArd(T, X, bias=TRUE, method="VB", control = list(maxit=500))
print(coefficients(res))

res <- SlrArd(T, X, bias=TRUE, method="VBMacKay")  ## faster
print(coefficients(res))

res <- SlrArd(T, X, bias=FALSE, method="VBMacKay")  ## without bias
print(coefficients(res))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
\keyword{ regression }
