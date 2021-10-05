
/**
 * 
 * @param {any[]} array 
 * @returns {[any[] any[]]}
 */
function halve(array) {
  switch (array.length) {
    case 0: return [[], []]
    case 1: return [[array[0]], []]
    default:
      const [l, r] = halve(array.slice(2))
      return [[array[0], ...l], [array[1], ...r]]
  }
}

/**
 * 
 * @param {any[]} array1 
 * @param {any[]} array2 
 * @return {any[]}
 */
function merge(array1, array2) {
  if (!array1.length) {
    return array2
  }
  else if (!array2.length) {
    return array1
  }
  else if (array1[0] <= array2[0]) {
    return [array1[0], ...merge(array1.slice(1), array2)]
  }
  else {
    return [array2[0], ...merge(array1, array2.slice(1))]
  }
}

/**
 * 
 * @param {any[]} array 
 * @returns {any[]}
 */
function mergeSort(array) {
  const [l, r] = halve(array)
  if (!l.length) {
    return r
  }
  else if (!r.length) {
    return l
  }
  else {
    return merge(mergeSort(l), mergeSort(r))
  }
}

// --------------------------------------------------

const input = document.querySelector('#input')
const output = document.querySelector('#output')

document.querySelector('#as-strings').addEventListener('click', () => {
  const strings = input.value.split(',').map(s => s.trim())
  output.value = mergeSort(strings)
})

document.querySelector('#as-numbers').addEventListener('click', () => {
  const numbers = input.value.split(',').map(s => +s)
  output.value = mergeSort(numbers)
})
