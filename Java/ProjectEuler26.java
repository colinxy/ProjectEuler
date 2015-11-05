import java.util.ArrayList;
import java.util.List;

/**
 * Created by yxy on 5/5/2015.
 */
public class ProjectEuler26 {

    public static List<Integer> getCycle(int n) {  // cycle of 1/n
        List<Integer> decimal = new ArrayList<Integer>();
        List<Integer> mods = new ArrayList<Integer>();
        int div = 0;
        int mod = 1;
        while (true) {
            mod *= 10;
            div = mod / n;
            mod %= n;

            if (mods.contains(mod)) {
                if (mod == mods.get(mods.size()-1)) {
                    decimal = new ArrayList<Integer>();
                    decimal.add(div);
                } else {
                    decimal = decimal.subList(mods.indexOf(mod), decimal.size());
                }
                break;
            }

            decimal.add(div);
            mods.add(mod);

            if (mod == 0) {
                break;
            }
        }

        return decimal;
    }

    public static void main(String[] args) {
        System.out.println("running project euler 26");
        long start = System.nanoTime();

        int max = 0;
        int maxI = 0;
        for (int i = 2; i <= 1000; i++) {
            int size = getCycle(i).size();
            if (size > max) {
                max = size;
                maxI = i;
            }
        }
        System.out.println(maxI + ": " + max);

        System.out.println("Running time: " + (System.nanoTime()-start)/1000000000.0 + " seconds");
    }
}
