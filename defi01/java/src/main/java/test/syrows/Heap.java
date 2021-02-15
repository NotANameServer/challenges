package test.syrows;

public class Heap {

    private final int[] array;
    private int index = 0;

    public Heap(int[] array) {

        this.array = array;

        // Adding all the content of the array into the queue.
        // In other words, it sorts the array using it.
        for(int number : array) this.addNumber(number);
    }

    /**
     * Add a number into the heap.
     *
     * @param number The number of add into the heap.
     */
    public void addNumber(int number) {

        // WARNING : In this example, we're not checking the index is not greater than the array
        // because we only want to sort the array and not really to add elements.
        // For larger uses, it can be done multiplying the size of the array by 2 and by recopying it.

        int child = this.index;
        int parent = (child - 1) / 2; // Getting parent index.

        this.array[child] = number; // Adding the number at the end of the array (according to the index).
        this.index++; // Incrementing index.

        // While parent < child, node values.
        while(this.array[parent] < this.array[child]) {

            this.swap(parent, child);

            child = parent;
            parent = (child - 1) / 2;
        }
    }

    /**
     * Removes and returns the max element of the heap.
     *
     * @return the max element of the heap.
     */
    public int removeMax() {

        if(this.index == 0)
            throw new UnsupportedOperationException("heap is empty.");

        int max = this.array[0]; // Retrieving max value at the root.
        this.array[0] = this.array[--this.index]; // Setting the last value at the root.

        int parent = 0, swapIndex = this.getSwapIndex(parent);

        while(swapIndex != -1) {

            this.swap(parent, swapIndex);

            parent = swapIndex; // The parent is now the child with the one it has been swapped.
            swapIndex = this.getSwapIndex(parent); // Retrieving new index to swap with.
        }
        return max;
    }

    private int getSwapIndex(int parent) {

        // If i is the index of the parent node, the children nodes are:
        // 1 - (2 * i) + 1
        // 2 - (2 * i) + 2
        int child1 = (2 * parent) + 1;
        int child2 = (2 * parent) + 2;

        // WARNING : Only the max child is swapped. It must be verified

        if(child1 < this.index && this.array[child1] >= this.array[child2] && this.array[parent] < this.array[child1])
            return child1;

        if(child2 < this.index && this.array[parent] < this.array[child2])
            return child2;

        return -1; // No swap to do. All is done.
    }

    private void swap(int index1, int index2) {

        int temp = this.array[index1];
        this.array[index1] = this.array[index2];
        this.array[index2] = temp;
    }
}
