
# ================================== #
# Principios de Finanzas: Capítulo 3 #
# ================================== #

# ------------------------------- #
# Sistema de amortización francés #
# ------------------------------- #

library(FinCal)
a = 5000/pv.annuity(r = 0.09, n = 5, pmt = -1, type = 0);a

library(FinancialMath)
amort.table(Loan = 5000,
            n    = 5,
            pmt  = NA,
            i    = 0.09,
            ic   = 1,
            pf   = 1,
            plot = TRUE)

amort.period(Loan = 5000,
             n    = 5,
             pmt  = NA,
             i    = 0.09,
             ic   = 1,
             pf   = 1)

A1 = 5000/fv.annuity(r = 0.09, n = 5, pmt = -1, type = 0); A1

library(tvm)
a = pmt(amt = 5000, maturity = 5, rate = 0.09);a
i = rate(amt = 5000, maturity = 5, pmt = a);i
T = loan(rate = 0.09, maturity = 5, amt = 5000, type = "french");T
cashflow(T)
