import java.util.List;

/**
 * Created by yxy on 9/15/15.
 * This implementation is rather slow, runs in 270s on my machine
 */
public class ProjectEuler146 {
    public static final int[] increment = {1, 3, 7, 9, 13, 27};
    public static List<Integer> primes;

    public static int smallFactor(long num) {
        int num_sqrt = (int)Math.sqrt(num)+1;

        for (Integer prime : primes) {
            if (prime > num_sqrt) break;
            if (num % prime == 0) return prime;
        }

        /*
        if (num % 3 == 0) return 3;

        for (int p = 7; p < num_sqrt; p += 30) {
            if (num % p == 0) return p;
            if (num % (p+4) == 0) return p+4;
            if (num % (p+6) == 0) return p+6;
            if (num % (p+10) == 0) return p+10;
            if (num % (p+12) == 0) return p+12;
            if (num % (p+16) == 0) return p+16;
            if (num % (p+22) == 0) return p+22;
            if (num % (p+24) == 0) return p+24;
        }*/
        return -1;
    }

    public static long primePattern(int top) {
        long totalSum = 0;

        // an optimization:
        // f(n) = n * n + i
        // if f(n) % p == 0  ==>  f(n+i) % p == 0
        boolean[] toCheck = new boolean[top+1];
        for (int i = 1; i < toCheck.length; i++) {
            toCheck[i] = true;
        }

        for (int n = 10; n <= top; n += 10) {  // increment by 10: divisibility by 5
            if (!toCheck[n]) continue;

            long n_sq = (long)n * n;
            boolean allPrime = true;

            for (int incre : increment) {

                long num = n_sq + incre;
                int sFactor = smallFactor(num);

                if (sFactor < 0) {
                    if (num < top) sFactor = (int) num;
                    else continue;
                } else {
                    allPrime = false;
                }

                int index = n;
                for (; index <= top; index += sFactor) {
                    toCheck[index] = false;
                    if (index % 10 == 0) break;
                }
                sFactor *= 10;
                for (; index <= top; index += sFactor) {
                    toCheck[index] = false;
                }
            }

            if (allPrime) {
                if (smallFactor(n_sq+19) != -1 && smallFactor(n_sq+21) != -1) {
                    totalSum += n;
                    // System.out.println(n);
                }
            }
        }

        return totalSum;
    }

    public static void main(String[] args) {
        System.out.println("running project euler 146");
        long start = System.nanoTime();

        int size = 150000000;

        primes = MathUtil.getPrime(size);

        System.out.println("primes acquired by: " + (System.nanoTime()-start)/1000000000.0 + " seconds");

        System.out.println(primePattern(size));  // 260 secs, slow

        System.out.println("Running time: " + (System.nanoTime()-start)/1000000000.0 + " seconds");
    }
}
