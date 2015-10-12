
/**
 * Created by yxy on 5/5/2015.
 */
public class ProjectEuler19 {
    public static int[] months = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
                               // Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
    public static int count = 0;

    public static boolean isLeapYear(int n) {
        return n % 4 == 0 && (n % 100 != 0 || n % 400 == 0);
    }

    // Sun 0, Mon 1, Tue 2, Wed 3, Thu 4, Fri 5, Sat 6
    public static int sundays(int year, int initial) {  // return changed state
        for (int i = 0, j = months.length; i < j; i++) {
            if (initial == 0) {
                // System.out.println(year + " " + (i+1));
                count++;
            }

            initial += months[i] % 7;
            if (i == 1 && isLeapYear(year))
                initial++;
            initial %= 7;
        }
        return initial;
    }

    public static int sundaysWithoutIncrement(int year, int initial) {  // return changed state
        for (int i = 0, j = months.length; i < j; i++) {
            initial += months[i] % 7;
            if (i == 1 && isLeapYear(year))
                initial++;
            initial %= 7;
        }
        return initial;
    }

    public static void main(String[] args) {
        System.out.println("running project euler 19");
        long start = System.nanoTime();

        int initial = sundaysWithoutIncrement(1900, 1);
        for (int i = 1901; i < 2001; i++) {
            initial = sundays(i, initial);
        }

        System.out.println(count);

        System.out.println("Running time: " + (System.nanoTime()-start)/1000000000.0 + " seconds");
    }
}
