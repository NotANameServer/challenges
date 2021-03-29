using System;

using Elanis.Sorting.Algorithm;

using Xunit;

namespace Elanis.Sorting.Test {
	public abstract class BaseTestClass {
		public ISortingAlgorithm algorithm;

		public BaseTestClass(ISortingAlgorithm algorithm) {
			this.algorithm = algorithm;
		}

		[Fact]
		public void EmptyArray() {
			Assert.Equal(
				algorithm.SortIntList(Array.Empty<int>()),
				Array.Empty<int>()
			);
		}

		[Fact]
		public void SortedSize2Array() {
			int[] array = new[] { 1, 2 };

			Assert.Equal(
				algorithm.SortIntList(array),
				array
			);
		}

		[Fact]
		public void UnsortedSize2Array() {
			int[] input = new[] { 2, 1 };
			int[] output = new[] { 1, 2 };

			Assert.Equal(
				algorithm.SortIntList(input),
				output
			);
		}

		[Fact]
		public void UnsortedSize3Array() {
			int[] input = new[] { 2, 1, 3 };
			int[] output = new[] { 1, 2, 3 };

			Assert.Equal(
				algorithm.SortIntList(input),
				output
			);
		}

		[Fact]
		public void UnsortedSize4Array() {
			int[] input = new[] { 2, 1, 4, 3 };
			int[] output = new[] { 1, 2, 3, 4 };

			Assert.Equal(
				algorithm.SortIntList(input),
				output
			);
		}

		[Fact]
		public void UnsortedSize5Array() {
			int[] input = new[] { 5, 2, 1, 4, 3 };
			int[] output = new[] { 1, 2, 3, 4, 5 };

			Assert.Equal(
				algorithm.SortIntList(input),
				output
			);
		}

		[Fact]
		public void UnsortedSize10ArrayWithDuplicates() {
			int[] input = new[] { 1, 5, 38, 2, 1, 1, 4, 3, 199, 23, 2 };
			int[] output = new[] { 1, 1, 1, 2, 2, 3, 4, 5, 23, 38, 199 };

			Assert.Equal(
				algorithm.SortIntList(input),
				output
			);
		}

		[Fact]
		public void UnsortedSize10ArrayWithDuplicatesAndNegatives() {
			int[] input = new[] { 1, 5, 38, 2, 1, -1, 4, 3, 199, -23, 2 };
			int[] output = new[] { -23, -1, 1, 1, 2, 2, 3, 4, 5, 38, 199 };

			Assert.Equal(
				algorithm.SortIntList(input),
				output
			);
		}
	}
}
