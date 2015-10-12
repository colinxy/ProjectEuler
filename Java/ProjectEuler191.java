import java.util.Arrays;

/**
 * Created by yxy on 5/6/2015.
 */
public class ProjectEuler191 {
    public static long[] cache;

    public static long LOA(int n) {
        cache = new long[n+1];
        cache[0] = 1;
        cache[1] = 2;
        cache[2] = 4;
        cache[3] = 7;
        for (int i = 4; i <= n; i++) {
            cache[i] = cache[i-1] + cache[i-2] + cache[i-3];
        }

        // System.out.println(Arrays.toString(cache));

        long result = cache[n];
        for (int i = 0; i < n; i++) {
            result += cache[i] * cache[n-i-1];
        }

        return result;
    }

    public static void main(String[] args) {  // essentially dynamic programming
        System.out.println("running project euler 191");
        long start = System.nanoTime();

        System.out.println(LOA(30));

        System.out.println("Running time: " + (System.nanoTime()-start)/1000000000.0 + " seconds");
    }
}
