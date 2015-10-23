'use strict';
/**
 * Created by yxy on 7/19/2015.
 */

function isPrime(n) {
    if ( isNaN(n) || !isFinite(n) || n%1 || n<2 ) return false;
    if (n % 2 == 0) return (n == 2);
    if (n % 3 == 0) return (n == 3);
    if (n % 5 == 0) return (n == 5);
    for (var i = 7, max = Math.sqrt(n); i <= max; i += 30) {
        if (n % i == 0) return false;
        if (n % (i+4) == 0) return false;
        if (n % (i+6) == 0) return false;
        if (n % (i+10) == 0) return false;
        if (n % (i+12) == 0) return false;
        if (n % (i+16) == 0) return false;
        if (n % (i+22) == 0) return false;
        if (n % (i+24) == 0) return false;
    }
    return true;
}

function factorize(n) {
    var factors = [];
    if (n % 2 == 0) factors.push(2);
    while (n % 2 == 0) n /= 2;

    var fac = 3;
    while (fac * fac <= n) {
        if (n % fac == 0) {
            factors.push(fac);
            while (n % fac == 0) n /= fac;
        }

        fac += 2;
    }
    if (n != 1) factors.push(n);

    return factors;
}

function leastFactor(n){
    if (isNaN(n) || !isFinite(n)) return NaN;
    if (n==0) return 0;
    if (n%1 || n*n<2) return 1;
    if (n%2==0) return 2;
    if (n%3==0) return 3;
    if (n%5==0) return 5;
    var m = Math.sqrt(n);
    for (var i=7;i<=m;i+=30) {
        if (n%i==0)      return i;
        if (n%(i+4)==0)  return i+4;
        if (n%(i+6)==0)  return i+6;
        if (n%(i+10)==0) return i+10;
        if (n%(i+12)==0) return i+12;
        if (n%(i+16)==0) return i+16;
        if (n%(i+22)==0) return i+22;
        if (n%(i+24)==0) return i+24;
    }
    return n;
}


function eliminateFactor(n, factor) {
    while (n % factor === 0) {
        n /= factor;
    }
    return n;
}


var N = 50000000;
var tStart = process.hrtime();
var i, j, factor;
var numbers = new Array(N+1);
var primality = new Array(N+1);

for (i = 0; i <= N; i++) numbers[i] = 2*i*i-1;
for (i = 0; i <= N; i++) primality[i] = true;

/*
for (i = 2; i <= N; i++) {
    if (primality[i] === false) {
        continue;
    }

    num = 2*i*i-1;
    //if (num > max) break;

    var factor = leastFactor(num);
    primality[i] = factor == num;

    // console.log(i, num, factor);
    j = factor + i;
    while (j <= N) {
        primality[j] = false;
        j += factor;
    }
}*/


for (i = 2; i <= N; i++) {
    if (numbers[i] === 1) continue;

    // if (!isPrime(numbers[i])) {
    //     console.log(i, numbers[i], factorize(numbers[i]));
    // }

    factor = numbers[i];

    j = i + factor;
    while (j <= N) {
        primality[j] = false;
        numbers[j] = eliminateFactor(numbers[j], factor);

        j += factor;
    }

    j = factor - i;
    while (j <= N) {
        primality[j] = false;
        numbers[j] = eliminateFactor(numbers[j], factor);

        j += factor;
    }
}


console.log(N-1 - primality.filter(function(a) {
        return a === false;
    }).length);

var tEnd = process.hrtime(tStart);
console.log("%ds %dms", tEnd[0], tEnd[1]/1000000);

/*
same as hk's method on page 1.
PROOF:

 */