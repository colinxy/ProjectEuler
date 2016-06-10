import java.math.BigInteger;
import java.util.*;

/**
 * Created by yxy on 3/12/2015.
 */
public class MathUtil {
    public static long pow(long base, long exp) {
        long result = 1;
        long pow = base;

        while (exp > 0) {
            if ((exp & 1) == 1) result *= pow;

            exp >>= 1;
            pow *= pow;
        }

        return result;
    }

    public static long pow(long base, long exp, long mod) {
        long power = 1;

        while (exp > 0) {
            if ((exp & 1) == 1) power = power * base % mod;

            exp >>= 1;
            base = base * base % mod;
        }

        return power;
    }

    /**
     safer version of pow, with less probability to overflow
     */
    public static long powMod(long base, long exp, long mod) {
        long result = 1;
        base %= mod;

        for (int i = 0; i < exp; i++) {
            result = result * base % mod;
        }

        return result;
    }


    private static final BigInteger TWO = BigInteger.valueOf(2);

    /**
     * Computes the integer square root of a number.
     *
     * @param n  The number.
     *
     * @return  The integer square root, i.e. the largest number whose square
     *     doesn't exceed n.
     */
    public static BigInteger sqrt(BigInteger n)
    {
        if (n.signum() >= 0)
        {
            final int bitLength = n.bitLength();
            BigInteger root = BigInteger.ONE.shiftLeft(bitLength / 2);

            while (!isSqrt(n, root))
            {
                root = root.add(n.divide(root)).divide(TWO);
            }
            return root;
        }
        else
        {
            throw new ArithmeticException("square root of negative number");
        }
    }

    private static boolean isSqrt(BigInteger n, BigInteger root)
    {
        final BigInteger lowerBound = root.pow(2);
        final BigInteger upperBound = root.add(BigInteger.ONE).pow(2);
        return lowerBound.compareTo(n) <= 0
                && n.compareTo(upperBound) < 0;
    }


    public static boolean isSquare(long n) {
        long root = Math.round(Math.sqrt(n));
        return root * root == n;
    }

    public static boolean isSquare(BigInteger n) {
        BigInteger root = sqrt(n);
        return root.multiply(root).equals(n);
    }

    public static int sum(List<Integer> integers) {
        int result = 0;
        for (int i : integers) {
            result += i;
        }
        return result;
    }

    public static int product(List<Integer> integers) {
        int result = 1;
        for (int i : integers) {
            result *= i;
        }
        return result;
    }

    public static int gcd(int x, int y) {
        while (y != 0) {
            int tmp = x % y;
            x = y;
            y = tmp;
        }
        return x;
    }

    public static long gcd(long x, long y) {
        while (y != 0) {
            long tmp = x % y;
            x = y;
            y = tmp;
        }
        return x;
    }

    public static long nCr(long n, long r) {
        if (r > n / 2) r = n - r;

        long result = 1;
        for (int i = 1; i <= r; i++) {
            result *= n - i + 1;
            result /= i;
        }

        return result;
    }

    public static BigInteger nCrBig(long n, long r) {
        if (r > n / 2) r = n - r;

        BigInteger result = BigInteger.ONE;
        for (int i = 1; i <= r; i++) {
            result = result.multiply(BigInteger.valueOf(n - i + 1));
            result = result.divide(BigInteger.valueOf(i));
        }

        return result;
    }

    /*
    public static BigInteger nCrBigMod(long n, long r, long mod) {
        if (r > n / 2) r = n - r;
        BigInteger modBig = BigInteger.valueOf(mod);

        BigInteger result = BigInteger.ONE;
        HashSet<Long> toDivide = new HashSet<Long>();
        for (long i = 1; i <= r; i++) {
            toDivide.add(i);
        }

        for (long i = n-r+1; i <= n; i++) {
            long current = i;

            Iterator<Long> iterator = toDivide.iterator();
            while (iterator.hasNext()) {
                Long element = iterator.next();
                if (current % element == 0) {
                    iterator.remove();
                    current /= element;
                }
            }

            result = result.multiply(BigInteger.valueOf(current)).mod(modBig);
        }

        return result;
    }*/

    public static boolean isPrime(int x) {
        if (x <= 3) return x >= 2;
        if (x % 2 == 0 || x % 3 == 0) return false;

        for (int i = 5, j = (int)Math.sqrt(x); i <= j; i += 6) {
            if (x % i == 0 || x % (i+2) == 0)
                return false;
        }

        return true;
    }

    public static boolean isPrimeLong(long x) {
        if (x <= 3) return x >= 2;
        if (x % 2 == 0 || x % 3 == 0) return false;

        for (long i = 5, j = (long)Math.sqrt(x); i <= j; i += 6) {
            if (x % i == 0 || x % (i+2) == 0)
                return false;
        }

        return true;
    }

    public static List<Integer> getPrime(int ceiling) {
        List<Integer> primes = new ArrayList<Integer>();
        boolean[] isPrime = new boolean[ceiling+1];

        for (int i = 0; i <= ceiling; i++)
            isPrime[i] = true;

        for (int p = 2; p <= ceiling; p++) {
            if (isPrime[p]) {
                primes.add(p);
                for (long i = (long)p*p; i <= ceiling; i += p) {
                    isPrime[(int)i] = false;
                }
            }
        }

        return primes;
    }

    public static BitSet isPrimeArray(int ceiling) {
        BitSet isPrime = new BitSet(ceiling);

        for (int i = 0; i <= ceiling; i++)
            isPrime.set(i);
        isPrime.set(0, false);
        isPrime.set(1, false);

        for (int p = 2; p <= ceiling; p++) {
            if (isPrime.get(p)) {
                for (long i = (long)p*p; i < ceiling; i += p) {
                    isPrime.set((int)i, false);
                }
            }
        }

        return isPrime;
    }

/*
    public static int[] getPrime(int lowerBound, int upperBound) {

    }*/

    public static boolean isPermutationOf(int x, int y) {
        char[] s1 = Integer.toString(x).toCharArray();
        char[] s2 = Integer.toString(y).toCharArray();
        if (s1.length != s2.length)
            return false;
        Arrays.sort(s1);
        Arrays.sort(s2);
        return Arrays.equals(s1, s2);
    }

    public static HashMap<Integer, Integer> primeFactorization(int x) {
        HashMap<Integer, Integer> result = new HashMap<Integer, Integer>();
        if (x % 2 == 0) {
            int exp = 1;
            x /= 2;
            while (x % 2 == 0) {
                exp ++;
                x /= 2;
            }
            result.put(2, exp);
        }

        int factor = 3;
        while (factor * factor <= x) {
            if (x % factor == 0) {
                int exp = 1;
                x /= factor;
                while (x % factor == 0) {
                    exp ++;
                    x /= factor;
                }
                result.put(factor, exp);
            }

            factor += 2;
        }

        if (x != 1) result.put(x, 1);

        return result;
    }

    public static ArrayList<ArrayList<Integer>> getAllFactors(int ceiling) {
        ArrayList<ArrayList<Integer>> allFactors = new ArrayList<ArrayList<Integer>>();
        for (int i = 0; i <= ceiling; i++) {
            allFactors.add(new ArrayList<Integer>());
        }

        boolean[] isPrime = new boolean[ceiling+1];
        for (int i = 0; i <= ceiling; i++) {
            isPrime[i] = true;
        }
        for (int i = 2; i <= ceiling; i++) {
            if (isPrime[i]) {
                allFactors.get(i).add(i);
                for (int j = 2*i; j <= ceiling; j+=i) {
                    allFactors.get(j).add(i);
                    isPrime[j] = false;
                }
            }
        }

        return allFactors;
    }

    public static void testMathUtil() {
        System.out.println(Long.MAX_VALUE + " " + Long.MIN_VALUE);
        System.out.println(Integer.MAX_VALUE + " " + Integer.MIN_VALUE);

        System.out.println("pow(12, 12) = " + pow(12, 12));
        System.out.println("pow(112413, 143, 213) = " + pow(112413, 143, 213));
        System.out.println("powMod(2, 25025, 10000000000) = " + powMod(2, 25025, pow(10, 10)));
        System.out.println("gcd(1341, 523893) = " + gcd(1341, 523893));
        System.out.println("nCr(45, 30) = " + nCr(45, 30));
        System.out.println("nCrBig(100, 50) = " + nCrBig(100, 50));
        // System.out.println("nCrBigMod(1000, 300, 1000000) = " + nCrBigMod(1000, 300, 1000000));
        System.out.println("isPrime(997) = " + isPrime(997));
        System.out.println("primeFactor(36) = " + primeFactorization(36));
        System.out.println("primeFactor(120) = " + primeFactorization(120));
        System.out.println("isPermutationOf(87109, 79180) = " + isPermutationOf(87109, 79180));

        System.out.println("getPrime(100) = " + getPrime(100));
        int primeUnder = (int)pow(10, 8);
        long getPrimeTime = System.nanoTime();
        System.out.println("getPrime(" + primeUnder + ")");
        List<Integer> primes = getPrime(primeUnder);
        System.out.println("500500th prime " + primes.get(500500-1));
        System.out.println("7037th prime " + primes.get(7037-1));
        /*for (int i: primes)
            System.out.print(i + " ");*/
        System.out.println("number of primes is " + primes.size() + "\nthe last prime is " + primes.get(primes.size()-1));
        System.out.println("primes acquired by " + (System.nanoTime()-getPrimeTime) / 1000000000.0 + " seconds");
        // roughly 7 seconds for primeUnder = 10 ^ 8, number of primes: 5761455

        /*long getSlowPrimeTime = System.nanoTime();
        List<Integer> primesSlow = new ArrayList<Integer>();
        for (int i = 11; i < primeUnder; i+=10)
            if (MathUtil.isPrime(i))
                primesSlow.add(i);
        for (int i = 13; i < primeUnder; i+=10)
            if (MathUtil.isPrime(i))
                primesSlow.add(i);
        for (int i = 17; i < primeUnder; i+=10)
            if (MathUtil.isPrime(i))
                primesSlow.add(i);
        for (int i = 19; i < primeUnder; i+=10)
            if (MathUtil.isPrime(i))
                primesSlow.add(i);
        System.out.println(primesSlow.get(primesSlow.size()-1));
        System.out.println("primes acquired by " + (System.nanoTime()-getSlowPrimeTime) / 1000000000.0 + " seconds");*/

        int ceiling = 1000000;
        long getAllFactorsTime = System.nanoTime();
        System.out.println("getAllFactors(" + ceiling + ")");
        ArrayList<ArrayList<Integer>> factors = getAllFactors(ceiling);
        System.out.println(factors.get(1000000-1));
        System.out.println("All factors acquired by " + (System.nanoTime()-getAllFactorsTime) / 1000000000.0 + " seconds");
    }

    public static void main(String[] args) {
        testMathUtil();
    }
}
