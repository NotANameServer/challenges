function quickSort(arr, start, end) {
    if (arr.length > 1) {
        const pivot = arr[end]
        let i = start
        let pivotPos = start
        let rightArrLength = 0
        while(i < end) {
            if (arr[i] <= pivot) {
                if (rightArrLength >= 1) {
                    let temp = arr[i]
                    arr[i] = arr[pivotPos]
                    arr[pivotPos] = temp
                }
                pivotPos += 1
            } else {
                rightArrLength += 1
            }
            i++
        }
        // Reposition pivot
        arr[end] = arr[pivotPos]
        arr[pivotPos] = pivot

        // Left array to sort
        if (pivotPos - 1 > start) {
            quickSort(arr, start, pivotPos - 1)
        }
        // Right array to sort
        if (pivotPos < end) {
            quickSort(arr, pivotPos, end)
        }
    }
}

function sort(a, n) {
    quickSort(a, 0, a.length - 1)
}
