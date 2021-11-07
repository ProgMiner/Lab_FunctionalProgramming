
const div = (n, d) => (n / d) >> 0;

const countNumberLetters = n => {
    switch (n) {
        case 1: return 3;
        case 2: return 3;
        case 3: return 5;
        case 4: return 4;
        case 5: return 4;
        case 6: return 3;
        case 7: return 5;
        case 8: return 5;
        case 9: return 4;
        case 10: return 3;
        case 11: return 6;
        case 12: return 6;
        case 13: return 8;
        case 14: return 8;
        case 15: return 7;
        case 16: return 7;
        case 17: return 9;
        case 18: return 8;
        case 19: return 8;
        case 20: return 6;
        case 30: return 6;
        case 40: return 5;
        case 50: return 5;
        case 60: return 5;
        case 70: return 7;
        case 80: return 6;
        case 90: return 6;
        case 1000: return 11;
    }

    if (n < 100) {
        return countNumberLetters(div(n, 10) * 10) + countNumberLetters(n % 10);
    }

    if (n < 1000) {
        if (n % 100 === 0) {
            return countNumberLetters(div(n, 100)) + 7;
        }

        return countNumberLetters(div(n, 100) * 100) + 3 + countNumberLetters(n % 100);
    }
};

let sum = 0;
for (let n = 1; n < 1001; ++n) {
    sum += countNumberLetters(n);
}

console.log(`Result: ${sum}`);
