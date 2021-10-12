

const collatzNextNumber = n => n % 2 == 0 ? n / 2 : 3 * n + 1;

const collatz = n => {
    let length = 1;

    while (n > 1) {
        ++length;
        n = collatzNextNumber(n);
    }

    return length;
};


let maxN = 0;
let maxLength = 0;
for (let n = 1; n <= 1000000; ++n) {
    const length = collatz(n);

    if (maxLength < length) {
        maxLength = length;
        maxN = n;
    }
}


console.log(`The longest Collatz sequence is started from ${maxN} and of length ${maxLength}`);
