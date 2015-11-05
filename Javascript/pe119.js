'use strict';
/**
 * Created by yxy on 7/18/2015.
 */

function digitSum(num) {
    var str = String(num);
    return str.split('').map(Number).reduce(function (a, b) {
        return a + b;
    }, 0);
}

var tStart = process.hrtime();
var max = 150;
var sequence = [];

for (var i = 2; i < max; i++) {
    var power = 1;
    for (var j = 1; power < 1000000000000000; j ++ ) {
        power *= i;
        if (digitSum(power) == i) {
            // console.log(power + " " + digitSum(power));
            // console.log(typeof power);
            if (String(power).length > 1)
                sequence.push(power);
        }
    }
}

sequence.sort(function(a, b) {
    return a - b;
});

console.log(sequence.length);
console.log(sequence[9]);
console.log(sequence[29]);

var tEnd = process.hrtime(tStart);
console.log("%ds %dms", tEnd[0], tEnd[1]/1000000);
