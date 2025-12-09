#include <iostream>
#include <string>
#include <cstdint>
#include <random>
#include <chrono>

using namespace std;
using namespace chrono;

class GF2_233{

public:
    uint64_t limb[4];

    static constexpr uint64_t POLY[4] = {
        0x00000000000213ULL,
        0x0000000000000000ULL,
        0x0000000000000000ULL,
        1ULL << 41
    };
    static constexpr uint64_t TOP_MASK = (1ULL << 41) - 1;
    static constexpr int FIELD_BITS = 233;


    GF2_233(){
    fill(limb, limb+4, 0);
    }

    void SetZero(){for (int i = 3; i >= 0; i--) limb[i] = 0;};
    void SetOne(){ this->limb[0] = 1;}

    void SetInt(int x) {this->limb[0] = x;}

    void PrintPoly() const {
        for (int i = 3; i >= 0; i--) {
            printf("%016llX", POLY[i]);
        }
        cout << "\n_________________________\n";
    }

    void Random() {
        random_device rd;
        mt19937_64 gen(rd());
        for (int i = 0; i < 4; i++) limb[i] = gen();
        limb[3] &= TOP_MASK;
    }

    void reduce(uint64_t t[8]) const {
        for (int i = 7; i >= 0; i--) {
            uint64_t w = t[i];
            t[i] = 0;

            while (w) {
                int b = __builtin_ctzll(w);
                int k = i*64 + b;
                if (k < 233) {
                    t[k >> 6] |= 1ULL << (k & 63);
                    w &= w - 1;
                    continue;
                }

                int d = k - 233;
                t[d >> 6]     ^= 1ULL << (d & 63);
                t[(d+1)>>6]   ^= 1ULL << ((d+1)&63);
                t[(d+4)>>6]   ^= 1ULL << ((d+4)&63);
                t[(d+9)>>6]   ^= 1ULL << ((d+9)&63);

                w &= w - 1;
            }
        }
        t[3] &= TOP_MASK;
    }


    void Reduce() {
        limb[3] &= TOP_MASK;
    }

    GF2_233 Addition(const GF2_233& b) const {
        GF2_233 r;
        r.limb[0] = limb[0] ^ b.limb[0];
        r.limb[1] = limb[1] ^ b.limb[1];
        r.limb[2] = limb[2] ^ b.limb[2];
        r.limb[3] = limb[3] ^ b.limb[3];
        r.Reduce();
        return r;
    }

    int Comparison(const GF2_233 &a, const GF2_233 &b){
        if (a.limb[0] == b.limb[0] && a.limb[1] == b.limb[1] &&a.limb[2] == b.limb[2] && a.limb[3] == b.limb[3]) return 0;
        return 1;
    }

    GF2_233 Multiplication(const GF2_233& b) const {
        uint64_t tmp[8] = {0};

        for (int i = 0; i < FIELD_BITS; i++) {
            int shiftWords = i >> 6;
            int shiftBits  = i & 63;
            if (((limb[shiftWords] >> shiftBits) & 1ULL) == 0) continue;

            for (int j = 0; j < 4; ++j) {
                uint64_t v = b.limb[j];
                uint64_t low = v << shiftBits;
                tmp[j + shiftWords] ^= low;
                if (shiftBits)
                    tmp[j + shiftWords + 1] ^= v >> (64 - shiftBits);
            }
        }

        reduce(tmp);
        GF2_233 r;
        for (int i = 0; i < 4; i++) r.limb[i] = tmp[i];
        return r;
    }

    GF2_233 Square() const {
        uint64_t tmp[8] = {0};
        for (int i = 0; i < FIELD_BITS; i++) {
            if ((limb[i >> 6] >> (i & 63)) & 1ULL) {
                int pos = 2 * i;
                tmp[pos >> 6] ^= 1ULL << (pos & 63);
            }
        }

        reduce(tmp);
        GF2_233 r;
        for(int i=0;i<4;i++) r.limb[i]=tmp[i];
        return r;
    }

    GF2_233 Pow(const GF2_233& e) const {
        GF2_233 result;
        result.SetOne();

        GF2_233 base = *this;
        GF2_233 exp = e;

        for (int word = 0; word < 4; word++) {
            uint64_t x = exp.limb[word];
            for (int bit = 0; bit < 64; bit++) {
                if (x & 1ULL)
                    result = result.Multiplication(base);
                base = base.Square();
                x >>= 1;
            }
        }

        return result;
    }

    int Trace() const {
        GF2_233 t = *this;
        GF2_233 x = *this;

        for (int i = 1; i < FIELD_BITS; i++) {
            x = x.Square();
            t = t.Addition(x);
        }
        return (t.limb[0] & 1);
    }


    string Print() const {
        static const char* hex = "0123456789ABCDEF";
        string s;
        s.reserve(64);

        for (int i = 3; i >= 0; i--) {
            uint64_t v = limb[i];
            for (int j = 60; j >= 0; j -= 4) s.push_back(hex[(v >> j) & 15]);
        }

        int p = 0;
        while (p+1 < s.size() && s[p] == '0') p++;
        cout << s.substr(p) << "\n_________________________\n";
        return s.substr(p);
    }


    int degree(const uint64_t x[4]) const {
        for (int i = 3; i >= 0; --i) {
            if (x[i] != 0) {
                return 64 * i + (63 - __builtin_clzll(x[i]));
            }
        }
        return -1;
    }

     void shift_left(uint64_t out[4], const uint64_t in[4], int j) const {
        uint64_t t[4] = {0,0,0,0};

        int wordShift = j >> 6;
        int bitShift  = j & 63;

        if (wordShift >= 4) {
            out[0] = out[1] = out[2] = out[3] = 0;
            return;
        }

        for (int i = 3; i >= 0; --i) {
            int dest = i + wordShift;
            if (dest >= 4) continue;

            uint64_t v = in[i];

            if (bitShift == 0) {
                t[dest] ^= v;
            } else {
                t[dest] ^= (v << bitShift);
                if (dest + 1 < 4) {
                    t[dest + 1] ^= (v >> (64 - bitShift));
                }
            }
        }

        out[0] = t[0];
        out[1] = t[1];
        out[2] = t[2];
        out[3] = t[3];
    }

    void xor_poly(uint64_t out[4], const uint64_t in[4]) const {
        out[0] ^= in[0];
        out[1] ^= in[1];
        out[2] ^= in[2];
        out[3] ^= in[3];
    };

    GF2_233 Inverse() const {
        uint64_t u[4], v[4], g1[4], g2[4], temp[4];
        for (int i=0;i<4;i++) {
            u[i] = limb[i];
            v[i] = POLY[i];
            g2[i] = 0;
        }
        g1[0] = 1; g1[1] = g1[2] = g1[3] = 0;

        while (true) {
            int du = degree(u);
            if (du == 0) break;

            int dv = degree(v);
            if (du < dv) {
                swap_ranges(u, u+4, v);
                swap_ranges(g1, g1+4, g2);
                swap(du, dv);
            }

            int j = du - dv;
            shift_left(temp, v, j);
            xor_poly(u, temp);
            shift_left(temp, g2, j);
            xor_poly(g1, temp);
        }

        uint64_t t[8] = { g1[0], g1[1], g1[2], g1[3], 0, 0, 0, 0 };
        reduce(t);

        GF2_233 result;
        for (int i=0;i<4;i++) result.limb[i] = t[i];
        return result;
    }

    int Test(const GF2_233 &A, const GF2_233 &B, const GF2_233 &C){
        int err = 0;

        GF2_233 x11, x12;
        x11 = C.Multiplication(A.Addition(B));
        x12 = B.Multiplication(C).Addition(A.Multiplication(C));
        if (Comparison(x11, x12) != 0) err++;

        GF2_233 x2 = A;
        for (int i = 0; i < FIELD_BITS; i++) {
            x2 = x2.Square();
        }
        if (Comparison(x2, A) != 0) err++;


        return err;
    }
};

int main()
{
GF2_233 a, b, n, c, d, e, f, g;

a.Random(); b.Random(); n.Random();

a.Print(); b.Print(); n.Print();

c = a.Addition(b);
cout << "a + b: \n";
c.Print();

d = a.Multiplication(b);
cout << "a * b: \n";
d.Print();

e = a.Square();
cout << "a ^ 2: \n";
e.Print();

f = a.Inverse();
cout << "a ^ {-1}: \n";
f.Print();

g = a.Pow(n);
cout << "a ^ n: \n";
g.Print();

cout << "Trace of a: \n";
cout << a.Trace() << "\n";

cout << "Errors: \n";
cout << a.Test(a, b, n) << "\n";

auto time1 = nanoseconds::zero();
auto time2 = nanoseconds::zero();
auto time3 = nanoseconds::zero();
auto time4 = nanoseconds::zero();
auto time5 = nanoseconds::zero();

int M = 100;

for (int i = 0; i < M; i++){
    GF2_233 X, Y;
    X.Random(); Y.Random();

    auto Start1 = high_resolution_clock::now();
        GF2_233 Z1 = X.Addition(Y);
    auto End1 = high_resolution_clock::now();
    auto duration1 = duration_cast<nanoseconds>(End1 - Start1);
    time1 = time1 + duration1;

    auto Start2 = high_resolution_clock::now();
        GF2_233 Z2 = X.Multiplication(Y);
    auto End2 = high_resolution_clock::now();
    auto duration2 = duration_cast<nanoseconds>(End2 - Start2);
    time2 = time2 + duration2;

    auto Start3 = high_resolution_clock::now();
        GF2_233 Z3 = X.Inverse();
    auto End3 = high_resolution_clock::now();
    auto duration3 = duration_cast<nanoseconds>(End3 - Start3);
    time3 = time3 + duration3;

    auto Start4 = high_resolution_clock::now();
        GF2_233 Z4 = X.Pow(Y);
    auto End4 = high_resolution_clock::now();
    auto duration4 = duration_cast<nanoseconds>(End4 - Start4);
    time4 = time4 + duration4;

    auto Start5 = high_resolution_clock::now();
        int Z5 = X.Trace();
    auto End5 = high_resolution_clock::now();
    auto duration5 = duration_cast<nanoseconds>(End5- Start5);
    time5 = time5 + duration5;
}

cout << "\n\nAverage time per operation: \n";
cout << "Addition: " << time1.count()/M << " nanoseconds;\n";
cout << "Multiplication: " << time2.count()/M << " nanoseconds;\n";
cout << "Inverse: " << time3.count()/M << " nanoseconds;\n";
cout << "Exponentiation: " << time4.count()/M << " nanoseconds;\n";
cout << "Trace: " << time5.count()/M << " nanoseconds;\n";

return 0;
}
