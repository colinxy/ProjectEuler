import java.util.List;

/**
 * Created by yxy on 6/28/2015.
 */
public class ProjectEuler231 {
    public static long getNumOfMultiple(long num, long start, long end) {
        long result = end / num - start / num;
        if (start % num == 0) result += 1;
        return result;
    }

    public static long sumPrimeFact(long m, long n) {
        long factorSum = 0;
        List<Integer> primes = MathUtil.getPrime((int)m);

        for (int p : primes) {
            long num = p;

            long mulCount;
            while ((mulCount = getNumOfMultiple(num, m-n+1, m)) > 0) {
                factorSum += mulCount * p;
                num *= p;
            }

            num = p;
            while ((mulCount = getNumOfMultiple(num, 1, n)) > 0) {
                factorSum -= mulCount * p;
                num *= p;
            }
        }

        return factorSum;
    }

    public static void main(String[] args) {
        System.out.println("running project euler 231");
        long start = System.nanoTime();

        System.out.println(sumPrimeFact(10, 3));
        System.out.println(sumPrimeFact(20000000, 5000000));

        System.out.println("Running time: " + (System.nanoTime()-start)/1000000000.0 + " seconds");
    }
}
