package test.syrows;

public class ArraySorter {

    /**
     *  Sort an array of number using a heap.²
     *
     * @param array The array to sort.
     * @param n The length of the array. Useless in Java but only to respect the challenge.
     */
    public static void sort(int[] array, int n) {

        Heap heap = new Heap(array);

        for(int i = n - 1; i >= 0; i--) array[i] = heap.removeMax();
    }
}
