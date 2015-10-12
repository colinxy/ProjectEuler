/**
 * Created by yxy on 5/26/2015.
 */
public class ProjectEuler73 {
    public static int fractionInRange23(int d) {
        int count = 0;
        for (int i = 5; i <= d; i++) {
            int start = i / 3 + 1;
            int end = i / 2;

            for (int j = start; j <= end; j++) {
                if (MathUtil.gcd(i, j) == 1) count++;
            }
        }

        return count;
    }

    public static void main(String[] args) {
        System.out.println("running project euler 73");
        long begin = System.nanoTime();
        System.out.println(fractionInRange23(12000));
        System.out.println("Execution time: " + (System.nanoTime() - begin) / 1000000000.0 + " second(s)");
    }
}
