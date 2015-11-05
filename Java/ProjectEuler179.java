/**
 * Created by yxy on 7/8/2015.
 */
public class ProjectEuler179 {
    public static int[] numOfDivUnder(int ceiling) {
        int[] numOfDiv = new int[ceiling+1];
        for (int i = 0; i <= ceiling; i++) {
            numOfDiv[i] = 1;
        }

        boolean[] isPrime = new boolean[ceiling+1];
        for (int i = 0; i <= ceiling; i++) {
            isPrime[i] = true;
        }

        for (int factor = 2; factor <= ceiling; factor++) {
            if (isPrime[factor]) {
                numOfDiv[factor] = 2;

                for (int j = 2*factor; j <= ceiling; j += factor) {
                    isPrime[j] = false;

                    int num = j;
                    if (num % factor == 0) {
                        int exp = 1;
                        num /= factor;
                        while (num % factor == 0) {
                            exp ++;
                            num /= factor;
                        }
                        numOfDiv[j] *= exp+1;
                    }
                }
            }
        }

        return numOfDiv;
    }

    public static int consPosDiv(int ceiling) {
        int[] numOfDiv = numOfDivUnder(ceiling);

        /*for (int i = 0; i < 1000; i++) {
            System.out.println(i + " " + numOfDiv[i]);
        }*/

        int count = 0;
        for (int i = 2; i < ceiling; i++) {
            if (numOfDiv[i] == numOfDiv[i-1]) {
                count ++;
            }
        }

        return count;
    }

    public static void main(String[] args) {
        System.out.println("running project euler 179");
        long start = System.nanoTime();

        System.out.println(consPosDiv((int)MathUtil.pow(10, 7)));

        System.out.println("Running time: " + (System.nanoTime()-start)/1000000000.0 + " seconds");
    }
}
