import java.math.BigInteger;
import java.util.Arrays;

/**
 * Created by yxy on 6/30/2015.
 */
public class ProjectEuler250 {
    public static long div250(int size, long mod) {
        long[] remainder = new long[250];
        for (int i = 0; i < 250; i++) {remainder[i] = 0;}

        for (int i = 1; i <= size; i++) {
            if (i % 10 == 0) {
                remainder[0] += 1;
                continue;
            }
            remainder[(int) MathUtil.pow(i, i, 250)] += 1;
        }
        // System.out.println(Arrays.toString(remainder));

        BigInteger bigMod = BigInteger.valueOf(mod);

        BigInteger[] result = new BigInteger[250];
        for (int i = 0; i < 250; i++) result[i] = BigInteger.ZERO;

        result[0] = BigInteger.valueOf(MathUtil.powMod(2, remainder[0], mod));
        // System.out.println(0 + " " + Arrays.toString(result));

        for (int rem = 1; rem < 250; rem++) {  // loop starts at 1
            BigInteger[] change = new BigInteger[250];
            for (int i = 0; i < 250; i++) change[i] = BigInteger.ZERO;

            if (rem == 125) {
                change[125] = BigInteger.valueOf(2).pow((int)remainder[125]-1).mod(bigMod);
                change[0] = change[125];
            }
            else {
                for (int i = 0; i <= remainder[rem]; i++) {  // pick  (0 ~ remainder[rem]) numbers
                    BigInteger big_nCr = MathUtil.nCrBig(remainder[rem], i);
                    change[rem * i % 250] = change[rem * i % 250].add(big_nCr).mod(bigMod);
                }
            }
            // System.out.println(rem + " " + Arrays.toString(change));

            BigInteger[] current = new BigInteger[250];
            for (int i = 0; i < 250; i++) current[i] = BigInteger.ZERO;

            for (int i = 0; i < 250; i++) {
                for (int j = 0; j < 250; j++) {
                    current[(i+j) % 250] = current[(i+j) % 250].add(change[i].multiply(result[j])).mod(bigMod);
                }
            }
            result = current;
            // System.out.println(rem + " " + Arrays.toString(result));
        }

        return result[0].longValue() - 1;  // excluding empty subset
    }

    public static void main(String[] args) {
        System.out.println("running project euler 250");
        long start = System.nanoTime();

        System.out.println(div250(250250, MathUtil.pow(10, 16)));  // 100s

        System.out.println("Running time: " + (System.nanoTime()-start)/1000000000.0 + " seconds");
    }
}
