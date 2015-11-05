import java.util.Arrays;

/**
 * Created by yxy on 8/29/2015.
 * credit to Lster and Lux Perpetua from ubuntuforums.org
 * for their wonderful insight on using a sieve like algorithm
 * to speed up the squareFree execution, and using a single field char
 * to represent three fields: isSquareFree, numOfFactor, isPrime (0, non 0)
 * I came up with the inclusion-exclusion algo by myself
 *
 * EDIT: ran jaap's code from project euler thread (2s).
 * my java implementation run in comparable time (3s)
 * but my python implementation, which runs in the same algorithm as jaap's
 * java code, was way slower (5-10s for sqrt(N) case), and I do not
 * see anything difference in algorithm that would cause the difference.
 * The difference therefore must reside in the language difference. I will further
 * investigate the reason why such a great difference in execution speed (more
 * than 100x, near 1000x in this case) occurs.
 */
public class ProjectEuler193 {
    public static long squareFree(long upperBound) {
        int sqrtBound = (int) Math.sqrt(upperBound) + 1;
        int sqrtSqrtBound = (int) Math.sqrt(sqrtBound) + 1;
        int[] factors = new int[sqrtBound];

        for (int i = 0; i < sqrtBound; i++) factors[i] = 0;

        for (int p = 2; p < sqrtBound; p ++) {
            if (factors[p] != 0) continue;

            if (p < sqrtSqrtBound) {
                int squared = p*p;
                    for (int i = squared; i < sqrtBound; i+=squared) {
                    factors[i] = -1;
                }
            }

            for (int i = p; i < sqrtBound; i+=p) {
                if (factors[i] >= 0) {
                    factors[i] ++;
                }
            }
        }

        // System.out.println(Arrays.toString(factors));

        // inclusion-exclusion for the total number of squareFree
        long total = upperBound;
        for (int i = 0; i < sqrtBound; i++) {
            if (factors[i] > 0) {
                long s = (long)i * i;
                long curr = upperBound / s;
                total += (factors[i] % 2 == 0) ? curr : -curr;
            }
        }

        return total;
    }

    public static void main(String[] args) {
        System.out.println("running project euler 193");
        long start = System.nanoTime();

        System.out.println(squareFree(MathUtil.pow(2, 50)));

        System.out.println("Running time: " + (System.nanoTime()-start)/1000000000.0 + " seconds");
    }
}
