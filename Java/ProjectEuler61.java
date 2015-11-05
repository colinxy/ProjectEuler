import java.util.ArrayList;
import java.util.List;

/**
 * Created by yxy on 5/5/2015.
 */
public class ProjectEuler61 {
    public static Integer[][] choices = new Integer[6][];

    public static int triangle(int n) {
        return n * (n + 1) / 2;
    }

    public static int square(int n) {
        return n * n;
    }

    public static int pentagonal(int n) {
        return n * (3*n - 1) / 2;
    }

    public static int hexagonal(int n) {
        return n * (2*n - 1);
    }

    public static int heptagonal(int n) {
        return n * (5*n - 3) / 2;
    }

    public static int octagonal(int n) {
        return n * (3*n -2);
    }

    public static int getLast2(int n) {
        return n % 100;
    }

    public static int getFirst2(int n) {
        return n / 100;
    }

    public static void findCandidateOfFirst2(Integer[] left, int find, int match, List<Integer> result) throws RuntimeException{
        if (left.length == 0 && find == match) {
            throw new RuntimeException("solution found " + result.toString());
        }
        for (Integer i: left) {  // the set of choice left
            for (Integer j: choices[i-3]) {  // actual number to choose from
                if (getFirst2(j) == find) {
                    Integer[] thisChoice = new Integer[left.length-1];
                    int index = 0;
                    for (Integer aLeft : left) {
                        if (!aLeft.equals(i)) thisChoice[index++] = aLeft;
                    }
                    List<Integer> thisResult = new ArrayList<Integer>(result);
                    thisResult.add(j);
                    findCandidateOfFirst2(thisChoice, getLast2(j), match, thisResult);
                }
            }
        }
    }

    public static void sixCyclic4digit() {
        List<Integer> tri = new ArrayList<Integer>();
        List<Integer> sq = new ArrayList<Integer>();
        List<Integer> pent = new ArrayList<Integer>();
        List<Integer> hex = new ArrayList<Integer>();
        List<Integer> hep = new ArrayList<Integer>();
        List<Integer> oct = new ArrayList<Integer>();

        int index = 1;
        while (true) {
            int t = triangle(index++);
            if (t < 1000) continue;
            if (t > 9999) break;
            tri.add(t);
        }

        index = 1;
        while (true) {
            int s = square(index++);
            if (s < 1000) continue;
            if (s > 9999) break;
            sq.add(s);
        }

        index = 1;
        while (true) {
            int p = pentagonal(index++);
            if (p < 1000) continue;
            if (p > 9999) break;
            pent.add(p);
        }

        index = 1;
        while (true) {
            int h = hexagonal(index++);
            if (h < 1000) continue;
            if (h > 9999) break;
            hex.add(h);
        }

        index = 1;
        while (true) {
            int h = heptagonal(index++);
            if (h < 1000) continue;
            if (h > 9999) break;
            hep.add(h);
        }

        index = 1;
        while (true) {
            int h = octagonal(index++);
            if (h < 1000) continue;
            if (h > 9999) break;
            oct.add(h);
        }

        choices[0] = tri.toArray(new Integer[tri.size()]);
        choices[1] = sq.toArray(new Integer[sq.size()]);
        choices[2] = pent.toArray(new Integer[pent.size()]);
        choices[3] = hex.toArray(new Integer[hex.size()]);
        choices[4] = hep.toArray(new Integer[hep.size()]);
        choices[5] = oct.toArray(new Integer[oct.size()]);

        /*System.out.println(tri);
        System.out.println(sq);
        System.out.println(pent);
        System.out.println(hex);
        System.out.println(hep);
        System.out.println(oct);*/

        for (int i: oct) {
            Integer[] choice = {3, 4, 5, 6, 7};
            List<Integer> result = new ArrayList<Integer>();
            result.add(i);

            try {
                findCandidateOfFirst2(choice, getLast2(i), getFirst2(i), result);
            } catch (RuntimeException ex) {
                System.out.println(ex.getMessage());
                break;
            }
        }
    }

    public static void main(String[] args) {  // in essence, a depth first search
        System.out.println("running project euler 61");
        long start = System.nanoTime();

        sixCyclic4digit();

        System.out.println("Running time: " + (System.nanoTime()-start)/1000000000.0 + " seconds");
    }
}
