import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Created by yxy on 5/29/2015.
 */
public class ProjectEuler124 {
    public static int product(List<Integer> integers) {
        int result = 1;
        for (int i : integers) {
            result *= i;
        }
        return result;
    }

    public static int orderedRadical(int n, int k) {
        Pair[] rads = new Pair[n+1];
        rads[0] = new Pair(0, 0);
        rads[1] = new Pair(1, 1);
        ArrayList<ArrayList<Integer>> allFactors = MathUtil.getAllFactors(n);

        for (int i = 2; i <= n; i++) {
            rads[i] = new Pair(i, product(allFactors.get(i)));
        }

        Arrays.sort(rads);

        // System.out.println(rads[k]);
        return rads[k].x;
    }

    public static void main(String[] args) {
        System.out.println("running project euler 124");
        long start = System.nanoTime();

        System.out.println(orderedRadical(1200000, 120000));

        System.out.println("Running time: " + (System.nanoTime()-start)/1000000000.0 + " seconds");
    }
}
