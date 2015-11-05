import java.math.BigInteger;

/**
 * Created by yxy on 7/12/2015.
 *
 * credit to Nicolas Siplis, who provided insights to my solution,
 * so that my O(n) and O(n log(n)) solution were obsolete.
 *
 * I did not come up with ingenious optimization, though it seemed
 * so obvious afterwards. And I could have done some memory optimization.
 */
public class ProjectEuler512 {
    static BigInteger ONE = BigInteger.ONE;

    public static long modPowers(long n) {
        long current = 1;
        long mod = 0;
        for (int i = 0; i < n; i++) {
            mod = (mod + current) % (n + 1);
            current = current * n % (n+1);
        }

        return mod;
    }

    public static long g(int n) {
        int[] phi = new int[n+1];
        for (int i = 1; i <= n; i++) {
            phi[i] = i;
        }

        System.out.println("successfully allocated: " + (System.nanoTime()-start)/1000000000.0 + " seconds");

        boolean[] isPrime = new boolean[n+1];
        for (int i = 0; i <= n; i++) {
            isPrime[i] = true;
        }

        for (int i = 2; i <= n; i++) {
            if (isPrime[i]) {
                phi[i] --;
                for (int j = 2*i; j <= n; j+=i) {
                    phi[j] = phi[j] / i * (i-1);
                    isPrime[j] = false;
                }
            }
        }

        System.out.println("Euler Phi calculated: " + (System.nanoTime()-start)/1000000000.0 + " seconds");

        long g_n = 1;  // phi[1] already excluded
        for (int i = 2; i <= n; i++) {
            long f_i;
            if (i % 2 == 0) f_i = 0;
            else f_i = phi[i];
            // System.out.println(f_i);

            // long f_i = modPowers(i) * phi[i] % (i+1);

            // BigInteger powers = BigInteger.valueOf(i);
            // BigInteger phiI = BigInteger.valueOf(phi[i]);
            // BigInteger multiplyBy = powers.modPow(powers, powers.add(ONE)).subtract(ONE).divide(powers.subtract(ONE));
            // long f_i = multiplyBy.multiply(phiI).mod(powers.add(ONE)).longValue();

            g_n += f_i;
        }

        return g_n;
    }

    static long start;

    public static void main(String[] args) {
        System.out.println("running project euler 512");
        start = System.nanoTime();

        System.out.println(g(500000000));

        System.out.println("Running time: " + (System.nanoTime()-start)/1000000000.0 + " seconds");
    }
}
