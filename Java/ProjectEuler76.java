/**
 * Created by yxy on 3/26/2015.
 */
public class ProjectEuler76 {
    public static int partition(int n) {  // excluding the number n itself
        return partition(n, 1) - 1;
    }

    public static int partition(int n, int start) {
        if (start == n)
            return 1;
        int ways = 1;  // n itself
        for (int i = start, j = n / 2; i <= j; i ++) {
            ways += partition(n - i, i);
        }
        return ways;
    }

    public static void main(String[] args) {
        System.out.println("running project euler 76");
        long begin = System.nanoTime();
        System.out.println(partition(100));
        System.out.println("Execution time: " + (System.nanoTime() - begin) / 1000000000.0 + " second(s)");
    }
}
