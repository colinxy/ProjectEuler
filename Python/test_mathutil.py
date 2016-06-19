
import unittest
import mathutil


class BasePrimeTest(unittest.TestCase):
    def setUp(self):
        self.prime_under100 = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37,
                               41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83,
                               89, 97]
        self.large_primes = [2147483647, 200560490131,
                             63018038201, 688846502588399]
        self.large_composites = [172081, 340561, 232250619601]

    def primality(self, prime_test):
        "Prime test does not test for negative numbers"
        # cumulative test
        self.assertListEqual(self.prime_under100,
                             list(filter(prime_test, range(1, 100))))
        # large prime
        for p in self.large_primes:
            self.assertTrue(prime_test(p))
        # large composite
        for c in self.large_composites:
            self.assertFalse(prime_test(c))


class PrimeTest(BasePrimeTest):
    def test_is_prime(self):
        self.primality(mathutil.is_prime)

    def test_miller_rabin(self):
        self.primality(mathutil.miller_rabin)


class TestPerformance(unittest.TestCase):
    pass

# should work under both python 2 and python 3
if __name__ == '__main__':
    unittest.main(verbosity=2)
