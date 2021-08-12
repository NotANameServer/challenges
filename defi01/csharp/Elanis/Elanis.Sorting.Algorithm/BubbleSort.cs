namespace Elanis.Sorting.Algorithm {
	public class BubbleSort : ISortingAlgorithm {
		public int[] SortIntList(int[] list) {
			int nonSortedAmount = list.Length;
			while (nonSortedAmount > 0) {
				int lastChanged = 0;

				for (int i = 1; i < nonSortedAmount; i++) {
					if (list[i - 1] > list[i]) {
						int tmpVal = list[i];
						list[i] = list[i - 1];
						list[i - 1] = tmpVal;

						lastChanged = i;
					}
				}

				nonSortedAmount = lastChanged;
			}

			return list;
		}
	}
}
