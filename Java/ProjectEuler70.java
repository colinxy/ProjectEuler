/**
 * Created by yxy on 3/13/2015.
 */
public class ProjectEuler70 {
    public static int totientPermutation(int n) {
        double minTotient = 10000000.0;
        int minIndex = 0;

        // int[] numbersToTest;
        // boolean[]

        for (int i = 2; i < n; i++) {
            int totient = ProjectEuler69.totientEuler(i);

            if (MathUtil.isPermutationOf(i, totient)) {
                double thisTotient = ((double) i) / totient;
                // System.out.println(i);
                if (minTotient > thisTotient) {
                    minIndex = i;
                    minTotient = thisTotient;
                    // System.out.println(minIndex + " " + totient);
                }
            }
        }

        return minIndex;
    }

    public static void main(String[] args) {
        System.out.println("running project euler 70");
        long begin = System.nanoTime();
        // System.out.println(ProjectEuler69.totientEuler(87109));
        System.out.println(totientPermutation(10000000));
        System.out.println("Execution time: " + (System.nanoTime() - begin) / 1000000000.0 + " second(s)");
    }
}
