using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading.Tasks;

namespace AlgoTri
{
    class Program
    {
        static void Main(string[] args)
        {
            var elementNumber = 3000;
            var arrayNumber = 10000;
            Console.WriteLine($"Generating {arrayNumber} arrays of {elementNumber} elements each");
            var allArrays = new List<int[]>();
            var benchmarks = new List<long>();
            Console.WriteLine($"Generation done");
            var processedArrays = new List<int[]>();

            var nan = new NanChallenge();

            for (var i = 0; i < arrayNumber; ++i)
            {
                var array = nan.GenerateRandomArray(elementNumber);
                allArrays.Add(array);
            }

            foreach (var array in allArrays)
            {
                var stopwatch = new Stopwatch();
                stopwatch.Start();
                nan.Sort(array, array.Length - 1);
                stopwatch.Stop();

                benchmarks.Add(stopwatch.ElapsedMilliseconds);
                processedArrays.Add(array);
            }
            Console.WriteLine();

            var sorted = 0;
            foreach (var array in processedArrays)
            {
                if (nan.CheckArray(array))
                {
                    ++sorted;
                }
            }
            Console.WriteLine($"{sorted} arrays sorted out of {arrayNumber}.");
            Console.WriteLine($"Average execution time : {benchmarks.Sum() / arrayNumber}ms");
            Console.WriteLine($"Total execution time : {benchmarks.Sum()}ms");
            Console.WriteLine();
        }

        public class NanChallenge
        {
            public bool CheckArray(int[] array)
            {
                bool isOk = true;

                for (var i = 0; i < array.Length - 1; ++i)
                {
                    isOk = isOk && array[i] <= array[i + 1];
                }

                return isOk;
            }

            public int[] GenerateRandomArray(int size)
            {
                var randomIntegers = new List<int>();
                var rand = new Random();
                for (var i = 0; i < size; ++i)
                {
                    randomIntegers.Add(rand.Next(size));
                }
                return randomIntegers.ToArray();
            }

            public int[] Sort(int[] array, int size)
            {
                ParallelQuickSort(array, 0, size);

                return array;
            }

            public void ParallelQuickSort(int[] array, int startIndex, int endIndex)
            {
                if (endIndex - startIndex < 2000)
                {
                    QuickSort(array, startIndex, endIndex);
                }
                else
                {
                    var pivot = Partition(array, startIndex, endIndex);
                    Parallel.Invoke(
                        () => ParallelQuickSort(array, startIndex, pivot - 1),
                        () => ParallelQuickSort(array, pivot + 1, endIndex));
                }
            }

            public void QuickSort(int[] array, int startIndex, int endIndex)
            {
                if (startIndex < endIndex)
                {
                    var pivot = Partition(array, startIndex, endIndex);

                    QuickSort(array, startIndex, pivot - 1);
                    QuickSort(array, pivot + 1, endIndex);
                }
            }

            public int Partition(int[] array, int low, int high)
            {
                var pivot = array[high];
                var i = low - 1;
                for (var j = low; j < high; ++j)
                {
                    if (array[j] <= pivot)
                    {
                        ++i;
                        Swap(array, i, j);
                    }
                }
                Swap(array, i + 1, high);
                return i + 1;
            }

            public void Swap(int[] array, int sourceIndex, int destinationIndex)
            {
                var temp = array[sourceIndex];
                array[sourceIndex] = array[destinationIndex];
                array[destinationIndex] = temp;
            }
        }
    }
}
