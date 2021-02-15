package test;

import java.util.Arrays;

public class TestUtils {

    /**
     * Sort array and return copy
     *
     * @return sorted new copy
     */
    public static int[] sortArray(int[] array) {
        int[] copy = array.clone();
        Arrays.sort(copy);
        return copy;
    }

}
