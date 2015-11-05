import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by yxy on 3/12/2015.
 */
public class ProjectEuler69 {
    public static int TotientMaximum(int n) {
        double maxTotient = 0.0;
        int maxIndex = 0;

        ArrayList<ArrayList<Integer>> allFactors = MathUtil.getAllFactors(n);

        for (int i = 2; i <= n; i++) {
            int tot = i;
            for (int factor : allFactors.get(i)) {
                tot /= factor;
                tot *= (factor-1);
            }

            double thisTotient = ((double) i) / tot;
            if (thisTotient > maxTotient) {
                maxIndex = i;
                maxTotient = thisTotient;
            }
        }

        return maxIndex;
    }

    public static int totient(int x) {  // very slow implementation
        HashMap<Integer, Integer> factors = MathUtil.primeFactorization(x);

        int notRelativePrime = 0;  // 1 is relatively prime with any number

        for (int i = 2; i < x; i ++){
            for (Integer prime : factors.keySet()) {
                if (i % prime == 0) {
                    notRelativePrime++;
                    break;
                }
            }
        }

        return x - 1 - notRelativePrime;
    }

    public static int totientEuler(int x) {
        int totientValue = x;
        for (int i : MathUtil.primeFactorization(x).keySet()) {
            totientValue /= i;
            totientValue *= (i-1);
        }
        // System.out.println(x + " " + totientValue);
        return totientValue;
    }

    public static void main(String[] args) {
        System.out.println("running project euler 69");
        long begin = System.nanoTime();
        System.out.println(totientEuler(30));
        System.out.println(TotientMaximum(1000000));
        System.out.println("Execution time: " + (System.nanoTime() - begin) / 1000000000.0 + " second(s)");
    } // 170s using totientEuler every time, 6s using getAllFactors
}
