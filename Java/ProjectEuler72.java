/**
 * Created by yxy on 3/29/2015.
 */
public class ProjectEuler72 {
    public static long fareySequence(int n) {
        long farey = 0;
        for (int i = 2; i <= n; i++) { // number of proper fractions, 2 already subtracted
            int totient = ProjectEuler69.totientEuler(i);
            farey += totient;
        }
        return farey;
    }

    public static void main(String[] args) {
        System.out.println("running project euler 72");
        long begin = System.nanoTime();
        System.out.println(fareySequence(1000000));
        System.out.println("Execution time: " + (System.nanoTime() - begin) / 1000000000.0 + " second(s)");
    }
}
