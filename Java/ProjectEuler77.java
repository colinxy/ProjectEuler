import java.util.*;

/**
 * Created by yxy on 3/26/2015.
 */
public class ProjectEuler77 {
    public static List<Integer> primes = MathUtil.getPrime(1000000);

    public static int partition(int n) {
        return partition(n, 2);
    }

    public static int partition(int n, int start) {
        if (start == n)
            return 1;

        int ways = MathUtil.isPrime(n) ? 1 : 0;

        int startIndex = primes.indexOf(start);

        for (int i = startIndex, j = n / 2; primes.get(i) <= j; i ++) { // n >= start
            ways += partition(n - primes.get(i), primes.get(i));
        }
        return ways;
    }

    public static void main(String[] args) {
        System.out.println("running project euler 77");
        long begin = System.nanoTime();
        System.out.println(partition(71));
        System.out.println();
        System.out.println("Execution time: " + (System.nanoTime() - begin) / 1000000000.0 + " second(s)");
    }
}
