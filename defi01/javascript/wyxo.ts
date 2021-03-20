function sort(array: number[]): number[] {
  for (let index = 0; index < array.length; index++) {
    if (index === array.length - 1) {
      break;
    }

    if (array[index] > array[index + 1]) {
      [array[index], array[index + 1]] = [array[index + 1], array[index]];

      return sort(array);
    }
  }

  return array;
}
