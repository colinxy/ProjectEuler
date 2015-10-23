'use strict';
/**
 * Created by yxy on 7/21/2015.
 */

function getPrimes(ceiling) {
    var primality = new Array(ceiling+1);
    var primes = [];
    var i, j;

    for (i = 0; i <= ceiling; i++) primality[i] = true;

    for (i = 2; i <= ceiling; i++) {
        if (primality[i]) {
            primes.push(i);
            for (j = i*i; j <= ceiling; j += i) {
                primality[j] = false;
            }
        }
    }
    return primes;
}

function hamming(current, candidate) {
    if (candidate.length == 1) {
        // console.log(current, Math.floor(Math.log(N / current) / Math.log(candidate[0])));
        return Math.floor(Math.log(N/current) / Math.log(candidate[0])) + 1;
    }
    var curr = current;
    var total = 0;
    var slice = candidate.slice(1, candidate.length);

    for (var i = 1; curr <= N; i++ ) {
        total += hamming(curr, slice);
        curr *= candidate[0];
    }
    return total;
}

var N = 1000000000;
var tStart = process.hrtime();
var primes = getPrimes(100);

console.log(hamming(1, primes));

var tEnd = process.hrtime(tStart);
console.log("%ds %dms", tEnd[0], tEnd[1]/1000000);
