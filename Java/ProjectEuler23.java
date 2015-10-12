import java.util.ArrayList;
import java.util.List;

/**
 * Created by yxy on 5/5/2015.
 */
public class ProjectEuler23 {
    public static int divisorSum(int n) {
        int sum = 1;
        for (int i = 2, j = (int)Math.sqrt(n)+1; i < j; i++) {
            if (n % i == 0) {
                int other = n / i;
                if (i != other) sum += i + other;
                else sum += i;
            }
        }
        return sum;
    }

    public static List<Integer> getAbundant(int upperLimit) {
        List<Integer> abundant = new ArrayList<Integer>();
        for (int i = 1; i < upperLimit; i++) {
            if (i < divisorSum(i))
                abundant.add(i);
        }
        return abundant;
    }

    public static void main(String[] args) {
        System.out.println("running project euler 23");
        long start = System.nanoTime();

        List<Integer> abundantNumbers = getAbundant(28123);

        boolean[] sumOf2Abundant = new boolean[28123];
        int size = abundantNumbers.size();
        for (int i = 0; i < size; i++) {
            for (int j = i; j < size; j++) {
                int k = abundantNumbers.get(i) + abundantNumbers.get(j);
                if (k < 28123) sumOf2Abundant[k] = true;
            }
        }
        int sum = 0;
        for (int i = 0; i < sumOf2Abundant.length; i++) {
            if (!sumOf2Abundant[i]) sum += i;
        }
        System.out.println(sum);

        System.out.println("Running time: " + (System.nanoTime()-start)/1000000000.0 + " seconds");
    }
}
