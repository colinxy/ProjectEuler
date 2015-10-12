import java.util.ArrayList;

/**
 * Created by yxy on 3/26/2015.
 */
public class ProjectEuler78 {
    public static long[] partitionCache = new long[10000000]; // 10 ** 7

    public static long getPartitionCache(int index) {
        if (index < 0)
            return 0;

        return partitionCache[index];
    }

    public static long partition(int n) { // last 10 digit of partition
        long p = 0;
        int k = 1;
        while (true) {
            int i = k * (3*k-1) / 2;
            if (n - i < 0)
                break;
            p += getPartitionCache(n - i) * (int)Math.pow(-1, k-1);
            p %= 10000000000L;
            k ++;
        }
        k = -1;
        while (true) {
            int i = k * (3*k-1) / 2;
            if (n - i < 0)
                break;
            p += getPartitionCache(n - i) * (int)Math.pow(-1, k-1);
            p %= 10000000000L;
            k --;
        }

        if (p < 0)
            p += 10000000000L;

        return p;
    }

    public static void main(String[] args) {
        System.out.println("running project euler 78");
        long begin = System.nanoTime();

        partitionCache[0] = 1L;
        for (int i = 1; i < 100000; i ++) {
            long p = partition(i);
            partitionCache[i] = p;

            // System.out.println(i + " " + p);
            if (p % 1000000 == 0) {
                System.out.println("value n found: " + i);
                break;
            }
        }
        // System.out.println(partition(150));
        System.out.println("Execution time: " + (System.nanoTime() - begin) / 1000000000.0 + " second(s)");
    }
}
