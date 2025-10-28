#include <iostream>
#include <cstdint>
#include <ctime>
#include <chrono>
#include <random>
using namespace std;
using namespace chrono;

static const int Length = 10;

class BigInteger{

private:
    static const int SizeOfNumber = 2048;
    static const int WordLength = 32;
    static const uint64_t Base = 1ULL << WordLength;
public:
    uint32_t Word[SizeOfNumber];

//public:

BigInteger() {
    fill(Word, Word+SizeOfNumber, 0);
}

void Convert(uint64_t x) {
    fill(Word, Word+SizeOfNumber, 0);
    Word[0] = (uint32_t)(x & (Base - 1));
    Word[1] = (uint32_t)(x >> WordLength);
}

BigInteger Generate() {
    BigInteger C;
    static mt19937_64 rng(random_device{}());
    uniform_int_distribution<uint32_t> dist(0, Base - 1);

    for (int i = 0; i < Length; i++) {
        Word[i] = dist(rng);
    }
    return C;
}

void Print() const {
    bool zero = true;
    for (int i = SizeOfNumber - 1; i >= 0; i--){
        if (Word[i] != 0) zero = false;
        if (!zero) cout << Word[i] << " ";
    }
    if (zero) cout << "0";
    cout << "\n___________\n";
}

int LongCompare(const BigInteger &x, const BigInteger &y)const{
    for (int i = SizeOfNumber - 1; i >= 0; i--){
        if (x.Word[i] > y.Word[i]) return 1;
        if (x.Word[i] < y.Word[i]) return -1;
        }
    return 0;
}

BigInteger Addition(const BigInteger &x)const{
    uint64_t carry = 0;
    BigInteger C;

    for (int i = 0; i < SizeOfNumber; i++){
        uint64_t temp = (uint64_t)Word[i] + x.Word[i] + carry;
        C.Word[i] = (uint64_t)(temp & (Base - 1));
        carry = temp >> WordLength;
    }
    return C;
}

BigInteger Subtraction(const BigInteger &x)const{
    BigInteger C;
    if (LongCompare(*this, x) < 0){
        cerr << "Negative values not supported. \n";
        return C;
    };

    uint64_t borrow = 0;

    for (int i = 0; i < SizeOfNumber; i++){
        uint64_t a1 = Word[i];
        uint64_t a2 = (uint64_t)x.Word[i] + borrow;

        if (a1 >= a2){
            C.Word[i] = (uint32_t)(a1 - a2);
            borrow = 0;
        }
        else {
            C.Word[i] = (uint32_t)(Base + a1 - a2);
            borrow = 1;
        }
    }
    return C;
}

BigInteger ShiftToHigh(int k)const{
    BigInteger C;
    if (k <= 0) return *this;
    if (k >= SizeOfNumber) return C;
    for (int i = SizeOfNumber - 1; i >= k; i--) {
        C.Word[i] = Word[i - k];
    }
    fill(C.Word, C.Word + k, 0);
    return C;
}

BigInteger LongMulOneDigit(uint32_t j) const {
    BigInteger C;
    uint64_t carry = 0;

    for (int i = 0; i < SizeOfNumber; ++i) {
        uint64_t temp = (uint64_t)Word[i] * j + carry;
        C.Word[i] = (uint32_t)(temp & (Base - 1));
        carry = temp >> WordLength;
    }

    return C;
}

BigInteger LongMul(const BigInteger &x) const {
    BigInteger C;
    C.Convert(0);
    uint64_t temp[2 * SizeOfNumber] = {0};

    for (int i = 0; i < SizeOfNumber; i++) {
        if (Word[i] == 0) continue;

        uint64_t carry = 0;
        for (int j = 0; j < SizeOfNumber; j++) {
            if (i + j >= 2 * SizeOfNumber) break;

            unsigned __int128 mul = (unsigned __int128)Word[i] * x.Word[j];
            unsigned __int128 sum = (unsigned __int128)temp[i + j] + mul + carry;

            temp[i + j] = (uint64_t)(sum & (Base - 1));
            carry = (uint64_t)(sum >> WordLength);
        }
        if (i + SizeOfNumber < 2 * SizeOfNumber) {
            temp[i + SizeOfNumber] += carry;
        }
    }

    for (int i = 0; i < SizeOfNumber; i++) {
        C.Word[i] = (i < 2 * SizeOfNumber) ? (uint32_t)temp[i] : 0;
    }

    return C;
}

string ToBinaryString() const {
    string result;
    result.reserve(SizeOfNumber * WordLength);

    bool started = false;
    for (int i = SizeOfNumber - 1; i >= 0; --i) {
        uint64_t word = Word[i];
        for (int Bit = WordLength - 1; Bit >= 0; --Bit) {
            bool BitVal = (word >> Bit) & 1U;
            if (BitVal) started = true;
            if (started) result.push_back(BitVal ? '1' : '0');
        }
    }

    if (!started) return "0";
    return result;
}

BigInteger FromBinaryString(const string& Bin) const {
    BigInteger C;
    fill(C.Word, C.Word + SizeOfNumber, 0);

    int Len = Bin.size();
    uint64_t BitPos = 0;

    for (int i = Len - 1; i >= 0 && BitPos < (uint64_t)SizeOfNumber * WordLength; i--, BitPos++) {
        if (Bin[i] == '1') {
            int TargetWord = BitPos / WordLength;
            int Bit  = BitPos % WordLength;
            C.Word[TargetWord] |= (1U << Bit);
            }
    }
    return C;
}

BigInteger ShiftBitsToHigh(uint64_t Bits) const {
    BigInteger C;
    if (Bits == 0) return *this;

    int WordShift = Bits / WordLength;
    int BitShift  = Bits % WordLength;

    for (int i = SizeOfNumber - 1; i >= WordShift; i--) {
        uint64_t Val = 0;

        if (i - WordShift >= 0) Val = Word[i - WordShift] << BitShift;
        if (BitShift && i - WordShift - 1 >= 0) Val |= Word[i - WordShift - 1] >> (WordLength - BitShift);

        C.Word[i] = Val & (Base - 1);
    }
    return C;
}

BigInteger ShiftBitsToLow(uint64_t Bits) const {
    BigInteger C;
    if (Bits == 0) return *this;

    int WordShift = Bits / WordLength;
    int BitShift  = Bits % WordLength;

    for (int i = 0; i < SizeOfNumber - WordShift; ++i) {
        uint64_t val = Word[i + WordShift] >> BitShift;
        if (BitShift && i + WordShift + 1 < SizeOfNumber)
            val |= (uint64_t)Word[i + WordShift + 1] << (WordLength - BitShift);

        C.Word[i] = (uint32_t)(val & (Base - 1));
    }

    for (int i = SizeOfNumber - WordShift; i < SizeOfNumber; ++i)
        C.Word[i] = 0;

    return C;
}

int BitLength() const {
    for (int i = SizeOfNumber - 1; i >= 0; i--) {
        uint64_t w = Word[i];
        if (w != 0) {
            int bits = 0;
            while (w) { w >>= 1; bits++; }
            return i * WordLength + bits;
        }
    }
    return 1;
}

bool GetBit(int Position) const {
    int TargetWord = Position / WordLength;
    int bit  = Position % WordLength;

    if (TargetWord >= SizeOfNumber) return false;
    return (Word[TargetWord] >> bit) & 1U;
}

void SetBit(int Position, bool value) {
    int TargetWord = Position / WordLength;
    int bit  = Position % WordLength;

    if (TargetWord >= SizeOfNumber) return;
    if (value) Word[TargetWord] |= (1U << bit);
    else Word[TargetWord] &= ~(1U << bit);
}

BigInteger LongDivRemainder(const BigInteger &Divisor) const {
    BigInteger Quotient, Remainder;
    BigInteger Dividend = *this;
    BigInteger Two; Two.Convert(2);
    Remainder.Convert(0);
    Quotient.Convert(0);

    int n = Dividend.BitLength();
    int m = Divisor.BitLength();

    if (m == 0) {
        cerr << "Division by zero.\n";
        return Quotient;
    }

    if (LongCompare(Dividend, Divisor) < 0) {
        return Dividend;
    }
    if (LongCompare(Divisor, Two) == 0) {
        BigInteger Remainder1;
        Remainder1.Convert(GetBit(0));
        return Remainder1;
    }
    for (int i = n - 1; i >= 0; i--) {
        Remainder = Remainder.ShiftBitsToHigh(1);

        if (Dividend.GetBit(i)) Remainder.Word[0] |= 1;

        if (LongCompare(Remainder, Divisor) >= 0) {
            Remainder = Remainder.Subtraction(Divisor);
            Quotient.SetBit(i, true);
        }
    }
    return Remainder;
}

BigInteger LongDivQuotient(const BigInteger &Divisor) const {
    BigInteger Quotient, Remainder;
    BigInteger Dividend = *this;
    Remainder.Convert(0);
    Quotient.Convert(0);

    int n = Dividend.BitLength();
    int m = Divisor.BitLength();

    if (m == 0) {
        cerr << "Division by zero.\n";
        return Quotient;
    }

    if (LongCompare(Dividend, Divisor) < 0) {
        Quotient.Convert(0);
        return Quotient;
    }

    for (int i = n - 1; i >= 0; i--) {
        Remainder = Remainder.ShiftBitsToHigh(1);
        if (Dividend.GetBit(i))
            Remainder.Word[0] |= 1;

        if (LongCompare(Remainder, Divisor) >= 0) {
            Remainder = Remainder.Subtraction(Divisor);
            Quotient.SetBit(i, true);
        }
    }

    return Quotient;
}

BigInteger LongPow(const BigInteger &x) const{
    BigInteger C;
    C.Convert(1);
    BigInteger A = *this;

    int Bits = x.BitLength();

    for (int i = 0; i < Bits; i++){
        C = C.LongMul(C);
        if (x.GetBit(i)){
            C = C.LongMul(A);
        }
    }
    return C;
}


BigInteger ModAddition(const BigInteger &x, const BigInteger &mod) const {
    return (this->Addition(x)).LongDivRemainder(mod);
}

BigInteger ModSubtraction(const BigInteger &x, const BigInteger &mod) const {
    BigInteger a = this->LongDivRemainder(mod);
    BigInteger b = x.LongDivRemainder(mod);
    if (LongCompare(a, b) >= 0)
        return a.Subtraction(b);
    else
        return mod.Subtraction(b.Subtraction(a));

}

BigInteger BarettReduction(const BigInteger &n, const BigInteger &Mu) const {
    BigInteger x = *this;
    BigInteger C;

    int k = (n.BitLength() + WordLength - 1) / WordLength;
    if (x.BitLength() == 0) return x;

    BigInteger q1 = x.ShiftBitsToLow((k - 1) * WordLength);
    BigInteger q2 = q1.LongMul(Mu);
    BigInteger q3 = q2.ShiftBitsToLow((k + 1) * WordLength);

    BigInteger q3n = q3.LongMul(n);

    if (LongCompare(x, q3n) >= 0) {
        C = x.Subtraction(q3n);
    } else {
        BigInteger r = x;
        while (LongCompare(r, q3n) < 0) {
            r = r.Addition(n);
        }
        C = r.Subtraction(q3n);
    }

    return C.LongDivRemainder(n);
}

BigInteger ComputeMu()const {
    BigInteger mod = *this;
    int k = (mod.BitLength() + WordLength - 1) / WordLength;
    BigInteger Beta; Beta.Convert(Base);

    BigInteger Pow; Pow.Convert(1);
    for (int i = 0; i < 2 * k; i++) Pow = Pow.LongMul(Beta);
    return Pow.LongDivQuotient(mod);
}

BigInteger ModMultiplication(const BigInteger &x, const BigInteger &mod) const {
    BigInteger Mu = mod.ComputeMu();
    return (this->LongMul(x)).BarettReduction(mod, Mu);
}

BigInteger ModMultiplicationBlakley(const BigInteger &x, const BigInteger &mod) const {
    BigInteger C, X;
    C.Convert(0);
    BigInteger A = *this;

    C = C.LongDivRemainder(mod);
    A = A.LongDivRemainder(mod);
    X = x.LongDivRemainder(mod);

    int bits = A.BitLength();

    for (int i = bits ; i >= 0; i--) {
        C = C.ShiftBitsToHigh(1);
        if (A.GetBit(i)) {
            C = C.Addition(X);
        }
        if(LongCompare(C, mod) != -1) C = C.Subtraction(mod);
        if(LongCompare(C, mod) != -1) C = C.Subtraction(mod);
    }

    return C;
}

BigInteger ModLongPowBarett(const BigInteger &y, const BigInteger &mod) const {
    BigInteger x = *this;
    BigInteger C; C.Convert(1);
    BigInteger Mu = mod.ComputeMu();

    for (int i = y.BitLength() - 1; i >= 0; i--) {
        C = C.LongMul(C).BarettReduction(mod, Mu);
        if (y.GetBit(i)) C = C.LongMul(x).BarettReduction(mod, Mu);
    }

    return C;
}

BigInteger GCD(BigInteger y) const {
    BigInteger x = *this;
    BigInteger Zero; Zero.Convert(0);
    BigInteger D; D.Convert(1);

    int shift = 0;
    while (!x.GetBit(0) && !y.GetBit(0)) {
        x = x.ShiftBitsToLow(1);
        y = y.ShiftBitsToLow(1);
        shift++;
    }

    while (!x.GetBit(0)) x = x.ShiftBitsToLow(1);

    do {while (!y.GetBit(0)) y = y.ShiftBitsToLow(1);
        if (LongCompare(x, y) == 1) swap(x, y);
        y = y.Subtraction(x);
    } while (LongCompare(y, Zero) != 0);

    if (shift > 0) x = x.ShiftToHigh(shift);

    return x;
}


BigInteger LCM(BigInteger y){
    BigInteger x = *this;
    BigInteger D = x.GCD(y);

    return (x.LongMul(y)).LongDivQuotient(D);
}

int Test1(const BigInteger &x, const BigInteger &y, const BigInteger &z) const {
    int err = 0;

    BigInteger test11 = x.LongMul(y);
    BigInteger test12 = y.LongMul(x);
    if (LongCompare(test11, test12) != 0) {
        err++;
    }

    BigInteger test21 = x.Addition(y).LongMul(z);
    BigInteger test22 = x.LongMul(z).Addition(y.LongMul(z));
    if (LongCompare(test21, test22) != 0) {
        err++;
    }

    BigInteger test31 = x.LongMul(y).LongMul(z);
    BigInteger test32 = x.LongMul(y.LongMul(z));
    if (LongCompare(test31, test32) != 0) {
        err++;
    }

    int t = 256;
    BigInteger test41; test41.Convert(t);
    BigInteger test42 = x.LongMul(test41);
    BigInteger test43; test43.Convert(0);
    for (int i = 0; i < t; i++){
        test43 = test43.Addition(x);
    }
    if(LongCompare(test42, test43) != 0){
        err++;
    }

    return err;
}

int Test2(const BigInteger &y, const BigInteger &z, const BigInteger &mod){
    BigInteger x = *this;
    int err = 0;

    BigInteger test11 = ((x.ModAddition(y, mod)).ModMultiplication(z, mod));
    BigInteger test12 = z.ModMultiplication(x.ModAddition(y, mod), mod);
    BigInteger test13 = (x.ModMultiplication(z, mod)).ModAddition(y.ModMultiplication(z, mod), mod);

    cout << "Distribution: \n";
    test11.Print(); test12.Print(); test13.Print();
    if (LongCompare(test11, test12) + LongCompare(test12, test13) + LongCompare(test11, test13) != 0){
        err++;
    }

    cout << "Addition: \n";
    int t = 256;
    BigInteger test21; test21.Convert(t);
    BigInteger test22 = x.ModMultiplication(test21, mod);
    BigInteger test23; test23.Convert(0);
    for (int i = 0; i < t; i++){
        test23 = test23.ModAddition(x, mod);
    }
    test22.Print(); test23.Print();
    if(LongCompare(test22, test23) != 0){
        err++;
    }

    cout << "Fermat theorem: \n";
    BigInteger Prime; Prime.Convert(524287); //19-те число Мерсенна
    BigInteger test31; test31.Convert(1);
    BigInteger test32; test32 = x.ModLongPowBarett(Prime.Subtraction(test31), Prime);
    if(LongCompare(test32, test31) != 0){
        err++;
    }
    test31.Print(); test32.Print();

    cout << "Euler property: \n";
    BigInteger test41; test41.Convert(3);
    BigInteger test42; test42.Convert(600);
    BigInteger test43 = (test41.LongPow(test42.Subtraction(test31))).LongMulOneDigit(2);
    BigInteger test44 = x.ModLongPowBarett(test43, test41.LongPow(test42));
    if (LongCompare(x.GCD(test41), test31) == 0) {
        if(LongCompare(test44, test31) != 0){
           err++;
        }
    }
    test44.Print(); test31.Print();

    cout << "Blakley test: \n";
    BigInteger test51; test51 = x.ModMultiplication(y, mod);
    BigInteger test52; test52 = x.ModMultiplicationBlakley(y, mod);
    if (LongCompare(test51, test52) != 0){
        err++;
    }
    test51.Print(); test52.Print();

    return err;
}

};

int main(){

//BigInteger X0, X1, X2, Xtest;

BigInteger Xmod;

/*
X0.Generate();
X1.Generate();
X2.Generate();
*/
//Xmod.Convert(4383941);

//X0.Print();
//X1.Print();
//X2.Print();
//Xmod.Print();

//Xtest.Convert(X0.Test2(X1, X2, Xmod));
//Xtest.Print();
Xmod.Generate();

//Xmod.Word[0] = 49434524;
//Xmod.Word[1] = 28429847;
//X1.GCD(X2).Print();
/*
X1.Addition(X2).Print();
X1.Subtraction(X2).Print();
X1.LongMul(X2).Print();
X1.LongDivRemainder(X2).Print();
X1.LongPow(X2).Print();
*/
/*
X1.GCD(X2).Print();
X1.LCM(X2).Print();

X1.ModAddition(X2, Xmod).Print();
X1.ModSubtraction(X2, Xmod).Print();
X1.ModMultiplication(X2, Xmod).Print();
X1.ModLongPowBarett(X2, Xmod).Print();
*/

auto time1 = nanoseconds::zero();
auto time2 = nanoseconds::zero();
auto time3 = nanoseconds::zero();
auto time4 = nanoseconds::zero();
auto time5 = nanoseconds::zero();

int M = 100;

for (int i = 0; i < M; i++){
    BigInteger X, Y;
    X.Generate(); Y.Generate();

    auto Start1 = high_resolution_clock::now();
        BigInteger Z1 = X.ModAddition(Y, Xmod);
    auto End1 = high_resolution_clock::now();
    auto duration1 = duration_cast<nanoseconds>(End1 - Start1);
    time1 = time1 + duration1;

    auto Start2 = high_resolution_clock::now();
        BigInteger Z2 = X.ModMultiplicationBlakley(Y, Xmod);
    auto End2 = high_resolution_clock::now();
    auto duration2 = duration_cast<nanoseconds>(End2 - Start2);
    time2 = time2 + duration2;

    auto Start3 = high_resolution_clock::now();
        BigInteger Z3 = X.ModMultiplication(Y, Xmod);
    auto End3 = high_resolution_clock::now();
    auto duration3 = duration_cast<nanoseconds>(End3 - Start3);
    time3 = time3 + duration3;

    auto Start4 = high_resolution_clock::now();
        BigInteger Z4 = X.ModLongPowBarett(Y, Xmod);
    auto End4 = high_resolution_clock::now();
    auto duration4 = duration_cast<nanoseconds>(End4 - Start4);
    time4 = time4 + duration4;

    auto Start5 = high_resolution_clock::now();
        BigInteger Z5 = X.GCD(Y);
    auto End5 = high_resolution_clock::now();
    auto duration5 = duration_cast<nanoseconds>(End5 - Start5);
    time5 = time5 + duration5;
}

cout << "Average time per operation: \n";
cout << "Addition: " << time1.count()/M << " nanoseconds;\n";
cout << "GCD: " << time5.count()/M << " nanoseconds;\n";
cout << "Multiplication (Blackley): " << time2.count()/M << " nanoseconds;\n";
cout << "Multiplication (normal): " << time3.count()/M << " nanoseconds;\n";
cout << "Exponentiation: " << time4.count()/M << " nanoseconds;\n";


return 0;
}
