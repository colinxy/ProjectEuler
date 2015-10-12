import java.util.*;

/**
 * Created by yxy on 7/2/2015.
 */
public class ProjectEuler249 {
    public Node[] nodes;

    public long currCount(int index) {
        if (nodes[index].counted) return nodes[index].getCount();

        long counts = 0;
        for (int i : nodes[index].from) {
            if (nodes[i].counted) {
                long thisCount = nodes[i].getCount();
                counts += thisCount;
            }
            else {
                long thisCount = currCount(i);
                counts += thisCount;
            }
        }

        nodes[index].setCount(counts);
        nodes[index].counted = true;

        return counts;
    }

    public long sumPrimeGraph(int ceiling, long mod) {
        List<Integer> primes = MathUtil.getPrime(ceiling);
        int max = MathUtil.sum(primes);
        List<Integer> allPrimes = MathUtil.getPrime(max);

        nodes = new Node[max+1];
        for (int i = 0; i < nodes.length; i++) {
            nodes[i] = new Node();
        }

        Set<Integer> encountered = new HashSet<Integer>();
        encountered.add(0);

        for (int p : primes) {
            List<Integer> toAdd = new ArrayList<Integer>();

            for (int element : encountered) {
                int newCandidate = element + p;
                toAdd.add(newCandidate);
                nodes[newCandidate].append(element);
            }

            encountered.addAll(toAdd);
        }

        nodes[0].counted = true;
        nodes[0].setCount(1);

        long total = 0;
        for (int p : allPrimes) {
            total = (total + currCount(p)) % mod;
        }

        return total;
    }

    public static long sumPrime(int ceiling, long mod) {
        List<Integer> primes = MathUtil.getPrime(ceiling);
        int max = MathUtil.sum(primes);
        List<Integer> allPrimes = MathUtil.getPrime(max);

        long[] eachCount = new long[max+1];
        for (int i = 0; i < eachCount.length; i++) eachCount[i] = 0;
        eachCount[0] = 1;

        HashSet<Integer> encountered = new HashSet<Integer>();
        encountered.add(0);

        for (Integer prime : primes) {
            Map<Integer, Long> update = new HashMap<Integer, Long>();

            for (Integer element : encountered) {
                Integer newCandidate = element + prime;
                update.put(newCandidate, eachCount[element]);
            }

            encountered.addAll(update.keySet());

            for (Map.Entry<Integer, Long> entry : update.entrySet()) {
                eachCount[entry.getKey()] = (eachCount[entry.getKey()] + entry.getValue()) % mod;
            }
        }

        // System.out.println(Arrays.toString(eachCount));

        long total = 0;
        for (int p : allPrimes) {
            total = (total + eachCount[p]) % mod;
        }

        return total;
    }

    public static void main(String[] args) {
        System.out.println("running project euler 249");
        long start = System.nanoTime();

        // the nature of the problem require it to do eager add, not lazy add
        // System.out.println((new ProjectEuler249()).sumPrimeGraph(10, MathUtil.pow(10, 16)));

        System.out.println(sumPrime(5000, MathUtil.pow(10, 16)));

        System.out.println("Running time: " + (System.nanoTime()-start)/1000000000.0 + " seconds");
    }
}


class Node {
    private long count;
    public boolean counted;
    public List<Integer> from;

    public Node() {
        count = 0;
        counted = false;
        from = new ArrayList<Integer>();
    }

    public void setCount(long count) {
        this.count = count;
    }

    public long getCount() {
        return count;
    }

    public void increCount(long incre) {
        count += incre;
    }

    public void append(int fromIndex) {
        from.add(fromIndex);
    }

    @Override
    public String toString() {
        return counted + ": " + count + "\n"
                + from;
    }
}
