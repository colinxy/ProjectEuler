
from mathutil import prime_under


# p(7000) = 70663
# the last few primes:
#   70607, 70619, 70621, 70627, 70639, 70657
# prime gaps:
#        12     2      6      12     18
# utility index (lower the better), defined as cumulative prime gaps
# divided by index diff. We want to pick numbers with better utility
# (lower utility index value the better)
#        10     9.5    12      15     18
#
# if we pick all p(6999), the remainder would be
# 6999 * n % 7000 = 6337
# to satisfy k-multiple condition, we pick smaller primes while optimizing utility

k = 7000
primes = prime_under(70663)
n = 70663

utility = [
    (primes[k-1]-primes[i]) / (k-1-i)
    for i in range(k-1)
]
best_utilities = sorted(zip(utility, range(k-1)))[:10]
for u, idx in best_utilities:
    print(u, k-1-idx, primes[k-1]-primes[idx], sep='\t')
print()

# top utilities:
# 9.5                4       38
# 9.555555555555555  9       86
# 10.0               5       50
# 10.090909090909092 66      666

# 6999 * n % 7000 = 6337
idx_slots = (k-1) * n % k
# try the highest few utilities, best pick:
#   p(k-1-4): 1582 times => 1582 * 4 = 6328
#   p(k-1-9): 1 time     => 1    * 9 = 9
# total                              = 6337
offset = 1582 * 38 + 1 * 86
max_prime_score = primes[k-1]*n - offset
print(max_prime_score)
