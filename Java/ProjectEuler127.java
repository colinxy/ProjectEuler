import java.util.ArrayList;
import java.util.List;

/**
 * Created by yxy on 6/28/2015.
 */
public class ProjectEuler127 {
    public static boolean shareFactor(List<Integer> a, List<Integer> b) {
        int aLen = a.size();
        int bLen = b.size();

        int bStart = 0;
        for (int aItem : a) {
            for (int j = bStart; j < bLen; j++) {
                int bItem = b.get(j);

                if (aItem < bItem) {
                    bStart = j;
                    break;
                }

                if (aItem == bItem) return true;
            }
        }

        return false;
    }

    public static long gcdLast(int limit) {
        long result = 0;

        ArrayList<ArrayList<Integer>> allFactors = MathUtil.getAllFactors(limit);
        long[] radicals = new long[limit+1];

        for (int i = 0, j = allFactors.size(); i < j; i++) {
            radicals[i] = MathUtil.product(allFactors.get(i));
        }
        // System.out.println(Arrays.toString(radicals));
        System.out.println("preprocessing finished: " + (System.nanoTime()-start)/1000000000.0 + " seconds");

        // Check for the properties
        for (int a = 1; a < limit; a++) {
            for (int b = a + 1, k = limit-a; b < k; b++) {

                if (radicals[a] * radicals[b] * radicals[a + b] >= a+b) continue;
                if (shareFactor(allFactors.get(a), allFactors.get(b))) continue;
                result += a + b;
            }
        }

        return result;
    }

    public static long notNested(int limit) {
        long result = 0;

        ArrayList<ArrayList<Integer>> allFactors = MathUtil.getAllFactors(limit);
        long[] radicals = new long[limit+1];

        for (int i = 0, j = allFactors.size(); i < j; i++) {
            radicals[i] = MathUtil.product(allFactors.get(i));
        }
        // System.out.println(Arrays.toString(radicals));
        System.out.println("preprocessing finished: " + (System.nanoTime()-start)/1000000000.0 + " seconds");

        // Check for the properties
        for (int a = 1; a < limit; a++) {
            for (int b = a + 1, k = limit-a; b < k; b++) {

                if (shareFactor(allFactors.get(a), allFactors.get(b))) continue;
                if (radicals[a] * radicals[b] * radicals[a + b] >= a+b) continue;
                result += a + b;
            }
        }

        return result;
    }

    public static long abcHit(int limit) {
        ArrayList<ArrayList<Integer>> allFactors = MathUtil.getAllFactors(limit);
        long[] radicals = new long[limit+1];

        for (int i = 0, j = allFactors.size(); i < j; i++) {
            radicals[i] = MathUtil.product(allFactors.get(i));
        }
        // System.out.println(Arrays.toString(radicals));
        System.out.println("preprocessing finished: " + (System.nanoTime()-start)/1000000000.0 + " seconds");

        long result = 0;
        for (int a = 1; a < limit; a++) {
            for (int b = a+1, k = limit-a; b < k; b++) {
                if ( ! shareFactor(allFactors.get(a), allFactors.get(b))) {
                    if (radicals[a] * radicals[b] * radicals[a+b] < a+b) {
                        result += a+b;
                    }
                }
            }
        }

        return result;
    }

    public static long start;

    public static void main(String[] args) {
        System.out.println("running project euler 127");
        start = System.nanoTime();

        System.out.println(gcdLast(120000));
        System.out.println("Running time: " + (System.nanoTime()-start)/1000000000.0 + " seconds");

        // System.out.println("Three methods compute the results with different efficiency\n");

        // start = System.nanoTime();
        // System.out.println(notNested(120000));
        // System.out.println("Running time: " + (System.nanoTime()-start)/1000000000.0 + " seconds\n");

        // start = System.nanoTime();
        // System.out.println(abcHit(120000));
        // System.out.println("Running time: " + (System.nanoTime()-start)/1000000000.0 + " seconds");
    }
}
