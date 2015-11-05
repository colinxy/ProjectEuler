import java.math.BigInteger;
import java.util.Map;

/**
 * Created by yxy on 7/9/2015.
 */
public class ProjectEuler211 {
    public static long expSum(long base, int exp) {
        long sum = 1;
        long current = base;
        for (int i = 0; i < exp; i++) {
            sum += current;
            current *= base;
        }
        return sum;
    }
    
    public static long divSqSum(int ceiling) {
        long sum = 1;  // 1 already included
        for (int i = 2; i < ceiling; i++) {
            Map<Integer, Integer> factors = MathUtil.primeFactorization(i);

            BigInteger sqSum = BigInteger.ONE;
            for (Map.Entry<Integer, Integer> fs : factors.entrySet()) {
                sqSum = sqSum.multiply(BigInteger.valueOf(expSum((long)fs.getKey()*fs.getKey(), fs.getValue())));
            }

            if (MathUtil.isSquare(sqSum)) sum += i;
        }
        
        return sum;
    }

    public static long divSqSumSieve(int ceiling) {
        long[] sigma2 = new long[ceiling+1];
        for (int i = 0; i <= ceiling; i++) {
            sigma2[i] = 1;
        }

        boolean[] isPrime = new boolean[ceiling+1];
        for (int i = 0; i <= ceiling; i++) {
            isPrime[i] = true;
        }

        for (int factor = 2; factor <= ceiling; factor++) {
            if (isPrime[factor]) {
                long factor_sq = (long)factor * factor;
                sigma2[factor] = factor_sq + 1;

                for (int j = 2*factor; j <= ceiling; j += factor) {
                    isPrime[j] = false;

                    int num = j;
                    int exp = 1;
                    num /= factor;
                    while (num % factor == 0) {
                        exp ++;
                        num /= factor;
                    }
                    sigma2[j] *= expSum(factor_sq, exp);
                }
            }
        }

        int count = 0;
        long sum = 0;

        for (int i = 1; i <= ceiling; i++) {
            if (sigma2[i] < 0)
                throw new ArithmeticException("square root of negative number");

            if (MathUtil.isSquare(sigma2[i])) {
                count ++;
                sum += i;
            }
        }

        return sum;
    }

    public static void main(String[] args) {
        System.out.println("running project euler 211");
        long start = System.nanoTime();

        System.out.println(divSqSumSieve(64000000 - 1));
        // System.out.println(divSqSum(1000000));

        System.out.println("Running time: " + (System.nanoTime()-start)/1000000000.0 + " seconds");
    }
}
