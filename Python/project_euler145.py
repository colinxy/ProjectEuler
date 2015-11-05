# How many reversible numbers are there below one-billion?
"""
Some positive integers n have the property that the sum [ n + reverse(n) ]
consists entirely of odd (decimal) digits. For instance, 36 + 63 = 99 and
409 + 904 = 1313. We will call such numbers reversible; so 36, 63, 409, and 904
are reversible. Leading zeroes are not allowed in either n or reverse(n).

There are 120 reversible numbers below one-thousand.

How many reversible numbers are there below one-billion (109)?
"""

# 2 digits (4+3+2+1)*2 = 20
# 3 digits __ _0/1/2/3/4_ __ (1+1+2+2+2+1+1)*2*5 = 100
# 4 digits __ __ __ __ 20 * 30 = 600
# 5 digits 0
# 6 digits 20 * 30 * 30 = 18000
# 7 digits 100 * ((4+3+2+1)*2+5) * 100/5 = 50000
# 8 digits 20 * 30 * 30 * 30 = 540000
