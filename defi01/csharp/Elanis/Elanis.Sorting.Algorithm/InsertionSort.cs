namespace Elanis.Sorting.Algorithm {
	public class InsertionSort : ISortingAlgorithm {
		public int[] SortIntList(int[] list) {
			for (int i = 1; i < list.Length; i++) {
				for (int j = i; j > 0; j--) {
					if (list[j - 1] > list[j]) {
						int swapTmp = list[j];
						list[j] = list[j - 1];
						list[j - 1] = swapTmp;
					} else {
						break;
					}
				}
			}

			return list;
		}
	}
}
