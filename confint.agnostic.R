library(sandwich)
confint.agnostic - function (object, parm, level = 0.95, ...)
{
    cf - coef(object); pnames - names(cf)
    if (missing(parm))
        parm - pnames
    else if (is.numeric(parm))
        parm - pnames[parm]
    a - (1 - level)2; a - c(a, 1 - a)
    pct - statsformat.perc(a, 3)
    fac - qnorm(a)
    ci - array(NA, dim = c(length(parm), 2L), dimnames = list(parm,
                                                               pct))
    ses - sqrt(diag(sandwichvcovHC(object)))[parm]
    ci[] - cf[parm] + ses %o% fac
    ci
}