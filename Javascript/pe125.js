'use strict';
/**
 * Created by yxy on 7/18/2015.
 */

var N;
var tStart, tEnd;
var i, max, solutions, cache, numbers;


function addThird(a, b) {
    return a + b[2];
}

function isPalindrome(number) {
    var str = String(number);
    for (var i = 0, j = str.length-1; i < j; i++, j--) {
        if (str.charAt(i) !== str.charAt(j)) return false;
    }
    return true;
}

tStart = process.hrtime();
N = 100000000;

solutions = [];
cache = [];


for (i = 1, max = Math.floor(Math.sqrt(N)) ; i < max; i++) {
    var sum = i*i;

    for (var j = i+1; ; j++) {
        sum += j * j;

        if (sum >= N) break;

        cache.push([i, j, sum]);
    }
}

numbers = new Set();
for (i = 0; i < cache.length; i++ ) {
    if (isPalindrome(cache[i][2])) {
        solutions.push(cache[i]);
        numbers.add(cache[i][2]);
    }
}

console.log();
console.log(solutions.length);
console.log(solutions.reduce(addThird, 0));

console.log(numbers.size);
console.log(numbers.reduce(function(a, b) {
    return a + b;
}));

tEnd = process.hrtime(tStart);
console.log("%ds %dms", tEnd[0], tEnd[1]/1000000);
