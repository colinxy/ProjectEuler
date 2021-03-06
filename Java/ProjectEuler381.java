import java.util.List;

/**
 * Created by yxy on 8/28/2015.
 */
public class ProjectEuler381 {
    static int k = 5;
    static long start;


    /**
     * using extended euclidean algorithm
     * solve modular inverse of the form a*x % p = 1
     * a and p are assumed to be co-prime
     *
     * s*p + t*a = 1
     */
    public static int inverseMod(int a, int p) throws ArithmeticException{
        int mod = p;
        int s    = 0,    t = 1;
        int oldS = 1, oldT = 0;

        while (a != 1) {
            int div = p / a;

            int temp = a;
            a = p % a;
            p = temp;

            int tempS = s, tempT = t;
            s = oldS - div*s;
            t = oldT - div*t;
            oldS = tempS;
            oldT = tempT;
        }

        int ans = t % mod;
        if (ans < 0) ans += mod;

        return ans;
    }

    public static long s(int p) {
        long result = 0;  // result = ((p-1) + 1) % p  which is 0
        long last = 1;
        for (int i = p-2; i > p-k; i--) {

            long curr = 0;
            curr = inverseMod(i, p) * last % p;
            last = curr;
            result += curr;
        }

        return result % p;
    }

    public static long primeKFactorial(int size) {
        List<Integer> primes = MathUtil.getPrime(size);
        System.out.println("primes generated by " + (System.nanoTime()-start)/1000000000.0 + " seconds");

        long sigma_s = 4;  // include 5
        for (int i = 3, j = primes.size(); i < j; i++) {  // skip 2, 3, 5
            int prime = primes.get(i);
            sigma_s += s(prime);
        }

        return sigma_s;
    }

    public static void main(String[] args) {
        System.out.println("running project euler 381");
        start = System.nanoTime();
        // System.out.println(inverseMod(8, 23));
        // System.out.println(s(7));
        System.out.println(primeKFactorial(100000000));

        System.out.println("Running time: " + (System.nanoTime()-start)/1000000000.0 + " seconds");
    }
}
