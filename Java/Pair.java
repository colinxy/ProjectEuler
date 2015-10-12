
/**
 * Created by yxy on 5/29/2015.
 */
public class Pair implements Comparable<Pair>{
    public int x;
    public int y;

    public Pair(int x, int y) {
        this.x = x;
        this.y = y;
    }

    @Override
    public String toString() {
        return "(" + x + ", " + y + ')';
    }

    @Override
    public int compareTo(Pair o) {
        return this.y - o.y;
    }
}
