import java.util.List;

/**
 * Created by yxy on 5/1/2015.
 */
public class ProjectEuler123 {
    
    public static void main(String[] args) {
        System.out.println("running project euler 123");
        long begin = System.nanoTime();

        List<Integer> primes = MathUtil.getPrime(5000000);
        for (int i = 7037; i < 1000000; i+=2) {
            int prime = primes.get(i-1);
            long remainder = 2 * i * (long)prime;

            if (remainder > 10000000000L) {
                System.out.println(i + "th prime " + prime + " remainder " + remainder);
                break;
            }
        }

        System.out.println("Execution time: " + (System.nanoTime() - begin) / 1000000000.0 + " second(s)");
    }
}
