/*
 * Bubble sort the array of number `a`
 * of size `n` into a new array of the
 * same size.
 * @param a unsorted array of number
 * @param n size of `a`
 * @returns sorted array of number
 *
 */
function sort(a: number[], n: number): number[] {
  /**
   * [Mestery] original line:
   * result: number[] = [...a];
   */
  const result: number[] = [...a];
  for (let i: number = 0; i < n; ++i) {
    for (let j: number = i + 1; j < n; ++j) {
      if (result[j] < result[i]) {
        const tmp: number = result[i];
        result[i] = result[j];
        result[j] = tmp;
      }
    }
  }
  return result;
}
