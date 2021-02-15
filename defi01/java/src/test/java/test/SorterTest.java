package test;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import test.emalios.Sorter;
import test.syrows.ArraySorter;

import java.util.Date;

public class SorterTest {


    private class ArraySortWrapper {
        public int[] sort(int[] array) {
            start = new Date();
            // emalios
            array = Sorter.sortArray(array, array.length);
            // syrows
            //ArraySorter.sort(array, array.length);

            if (logEnableArraySortWrapper)
                System.out.println(String.format("time = %d", (new Date()).getTime() - start.getTime()));

            return array;

        }
    }

    private ArraySortWrapper wrapper;

    private Date start;

    private boolean logEnableArraySortWrapper = true;

    @Before
    public void setUp() {
        wrapper = new ArraySortWrapper();
    }

    @Test
    public void testSimple() {
        System.out.println("testSimple");
        int[] input = new int[]{1, 23, 4, 56, -1, 100};
        Assert.assertArrayEquals(TestUtils.sortArray(input), wrapper.sort(input));
    }

    @Test
    public void testSimpleX1000() {
        System.out.println("testSimpleX1000");
        logEnableArraySortWrapper = false;

        int[] input = new int[]{1, 23, 4, 56, -1, 100};

        Date start = new Date();
        for (int i = 0; i < 1000; i++) {
            wrapper.sort(input);
        }

        System.out.println(String.format("time = %d", (new Date()).getTime() - start.getTime()));

    }

    @Test
    public void testLimit() {
        System.out.println("testLimit");
        int[] input = new int[]{Integer.MIN_VALUE, Integer.MAX_VALUE, 0};
        Assert.assertArrayEquals(TestUtils.sortArray(input), wrapper.sort(input));
    }

    @Test
    public void testEquality() {
        System.out.println("testEquality");
        int[] input = new int[]{1, 1, 1, 1, 1, 1, 1, 1};
        Assert.assertArrayEquals(TestUtils.sortArray(input), wrapper.sort(input));
    }

    @Test
    public void testLargeArrayRandom150k() {
        System.out.println("testLargeArrayRandom150k");
        int length = 150000;
        int[] input = new int[length];

        for (int i = 0; i < length; i++) {
            input[i] = (int) (Math.random() * Integer.MAX_VALUE);
        }

        Assert.assertArrayEquals(TestUtils.sortArray(input), wrapper.sort(input));
    }

    @Test
    public void testLargeOderingArray150k() {
        System.out.println("testLargeOderingArray150k");
        int length = 150000;
        int[] input = new int[length];

        for (int i = 0; i < length; i++) {
            input[i] = i;
        }

        Assert.assertArrayEquals(TestUtils.sortArray(input), wrapper.sort(input));
    }

    @Test
    public void testLargeReverseOderingArray150k() {
        System.out.println("testLargeReverseOderingArray150k");
        int length = 150000;
        int[] input = new int[length];

        for (int i = 0, j = length - 1; i < length || j >= 0; i++, j--) {
            input[j] = i;
        }

        Assert.assertArrayEquals(TestUtils.sortArray(input), wrapper.sort(input));
    }

    @Test
    public void testLargeArrayRandom10k() {
        System.out.println("testLargeArrayRandom10K");
        int length = 10000;
        int[] input = new int[length];

        for (int i = 0; i < length; i++) {
            input[i] = (int) (Math.random() * Integer.MAX_VALUE);
        }

        Assert.assertArrayEquals(TestUtils.sortArray(input), wrapper.sort(input));
    }

    @Test
    public void testLargeOderingArray10k() {
        System.out.println("testLargeOderingArray10K");
        int length = 10000;
        int[] input = new int[length];

        for (int i = 0; i < length; i++) {
            input[i] = i;
        }

        Assert.assertArrayEquals(TestUtils.sortArray(input), wrapper.sort(input));
    }

    @Test
    public void testLargeReverseOderingArray10k() {
        System.out.println("testLargeReverseOderingArray10k");
        int length = 10000;
        int[] input = new int[length];

        for (int i = 0, j = length - 1; i < length || j >= 0; i++, j--) {
            input[j] = i;
        }

        Assert.assertArrayEquals(TestUtils.sortArray(input), wrapper.sort(input));
    }

    @Test
    public void testLargeArrayRandom1k() {
        System.out.println("testLargeArrayRandom1K");
        int length = 1000;
        int[] input = new int[length];

        for (int i = 0; i < length; i++) {
            input[i] = (int) (Math.random() * Integer.MAX_VALUE);
        }

        Assert.assertArrayEquals(TestUtils.sortArray(input), wrapper.sort(input));
    }

    @Test
    public void testLargeOderingArray1k() {
        System.out.println("testLargeOderingArray1K");
        int length = 1000;
        int[] input = new int[length];

        for (int i = 0; i < length; i++) {
            input[i] = i;
        }

        Assert.assertArrayEquals(TestUtils.sortArray(input), wrapper.sort(input));
    }

    @Test
    public void testLargeReverseOderingArray1k() {
        System.out.println("testLargeReverseOderingArray1k");
        int length = 1000;
        int[] input = new int[length];

        for (int i = 0, j = length - 1; i < length || j >= 0; i++, j--) {
            input[j] = i;
        }

        Assert.assertArrayEquals(TestUtils.sortArray(input), wrapper.sort(input));
    }
}
