namespace Elanis.Sorting.Algorithm {
	public class SelectionSort : ISortingAlgorithm {
		public int[] SortIntList(int[] list) {
			for (int i = 0; i < list.Length - 1; i++) {
				int jMin = i;

				for (int j = i + 1; j < list.Length; j++) {
					if (list[j] < list[jMin]) {
						jMin = j;
					}
				}

				if (jMin != i) {
					int tmpVal = list[i];
					list[i] = list[jMin];
					list[jMin] = tmpVal;
				}
			}

			return list;
		}
	}
}
