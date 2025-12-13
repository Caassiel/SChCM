#include <iostream>
#include <cstdint>
#include <chrono>
#include <random>
using namespace std;
using namespace chrono;

static constexpr int FIELD_BITS = 233;

class GF2_233_NB {
public:
    uint8_t limb[FIELD_BITS];
    static uint8_t M[FIELD_BITS][FIELD_BITS];
    static bool MultMatrixBuilt;

    GF2_233_NB() { SetZero(); }

    void SetZero() { fill(limb, limb + FIELD_BITS, 0); }
    void SetOne()  { fill(limb, limb + FIELD_BITS, 1); }

    void Random() {
        static mt19937_64 gen(random_device{}());
        for (int i = 0; i < FIELD_BITS; i++) limb[i] = gen() & 1;
    }

    void PrintBinary() const {
        for (int i = 0; i < FIELD_BITS; i++)
            cout << (int)limb[i];
        cout << "\n";
    }

    GF2_233_NB Addition(const GF2_233_NB &b) const {
        GF2_233_NB r;
        for (int i = 0; i < FIELD_BITS; i++)
            r.limb[i] = limb[i] ^ b.limb[i];
        return r;
    }

    GF2_233_NB CyclicShiftRight(int s) const {
        GF2_233_NB r;
        s %= FIELD_BITS;
        for (int i = 0; i < FIELD_BITS; i++)
            r.limb[i] = limb[(i - s + FIELD_BITS) % FIELD_BITS];
        return r;
    }

    GF2_233_NB CyclicShiftLeft(int s) const {
        GF2_233_NB r;
        s %= FIELD_BITS;
        for (int i = 0; i < FIELD_BITS; i++)
            r.limb[i] = limb[(i + s) % FIELD_BITS];
        return r;
    }

    GF2_233_NB Square() const {
        return CyclicShiftRight(1);
    }

    void BuildMatrix() const {
        const int p = 2 * FIELD_BITS + 1;

        for (int i = 0; i < FIELD_BITS; i++)
            for (int j = 0; j < FIELD_BITS; j++)
                M[i][j] = 0;

        int pow2[FIELD_BITS]; pow2[0] = 1;
        for (int i = 1; i < FIELD_BITS; i++){
            pow2[i] = (pow2[i - 1] * 2) % p;
        }

        for (int i = 0; i < FIELD_BITS; i++) {
            for (int j = 0; j < FIELD_BITS; j++) {
                int a = pow2[i]; int b = pow2[j];
                int Results[4] = {(a + b) % p, (a - b + p) % p, (-a + b + p) % p, (-a - b + 2*p) % p};

                for (int k = 0; k < 4; k++) {
                    if (Results[k] == 1 || Results[k] == p - 1) {
                        M[i][j] = 1;
                        break;
                    }
                }
            }
        }
    }

    GF2_233_NB Multiplication(const GF2_233_NB &b) const {
        GF2_233_NB z;
        z.SetZero();
        BuildMatrix();

        for (int i = 0; i < FIELD_BITS; i++) {
            GF2_233_NB u_shifted = this->CyclicShiftLeft(i);
            GF2_233_NB v_shifted = b.CyclicShiftLeft(i);

            GF2_233_NB u_prime;
            u_prime.SetZero();
            for (int row = 0; row < FIELD_BITS; row++) {
                uint8_t val = 0;
                for (int col = 0; col < FIELD_BITS; col++) {
                    val ^= u_shifted.limb[col] & M[col][row];
                }
                u_prime.limb[row] = val;
            }

            uint8_t z_i = 0;
            for (int k = 0; k < FIELD_BITS; k++) {
                z_i ^= u_prime.limb[k] & v_shifted.limb[k];
            }
            z.limb[i] = z_i;
        }

        return z;
    }

    int Trace() const {
        GF2_233_NB x = *this;
        int T = 0;
            for (int i = 0; i < FIELD_BITS; i++){
                T ^= (int)x.limb[i];
            }
        return T;
    }

    static int toBits(int x, int bits[]) {
        int len = 0;
        while (x > 0) {
            bits[len++] = x & 1;
            x >>= 1;
        }
        return len;
    }

    GF2_233_NB InverseItohTsujii() const {
        const int m = FIELD_BITS;
        int bits[FIELD_BITS >> 1];
        int len = toBits(m - 1, bits);

        GF2_233_NB alpha = *this;
        GF2_233_NB beta  = alpha;

        int k = 1;

        for (int i = len - 2; i >= 0; i--) {
            GF2_233_NB gamma = beta;
            for (int j = 0; j < k; j++)
                beta = beta.Square();
            beta = beta.Multiplication(gamma);

            k *= 2;
            if (bits[i]) {
                beta = beta.Square();
                beta = beta.Multiplication(alpha);
                k += 1;
            }
        }

        beta = beta.Square();
        return beta;
    }

    GF2_233_NB Inverse() const {
        GF2_233_NB t = *this;
        GF2_233_NB r; r.SetOne();
        for (int i = 1; i < FIELD_BITS; i++) {
            t = t.Square();
            r = r.Multiplication(t);
        }

        return r;
    }

    GF2_233_NB Pow(const GF2_233_NB& e) const {
        GF2_233_NB r;
        r.SetOne();

        GF2_233_NB b = *this;

        for (int i = FIELD_BITS - 1; i >= 0; i--) {
            if (e.limb[i]) {
                r = r.Multiplication(b);
            }
            b = b.Square();
        }
        return r;
    }

    string PrintHex() const {
        static const char* hex = "0123456789ABCDEF";
        string s;
        int n = (FIELD_BITS+3)/4;
        s.reserve(n);

        for (int h = n - 1; h >= 0; h--) {
            int v = 0;
            for (int b = 0; b < 4; b++) {
                int Index = h * 4 + b;
                if (Index < FIELD_BITS) {
                    int Reverse = FIELD_BITS - 1 - Index;
                    v |= limb[Reverse] << b;
                }
            }
            s.push_back(hex[v]);
        }

        size_t p = s.find_first_not_of('0');
        string out;
        if (p != string::npos) out = s.substr(p);
        else out = "0";
        cout << out << "\n_________________\n";
        return out;
    }

    void PrintMatrix() const {
    GF2_233_NB a; a.BuildMatrix();
        for (int i = 0; i < FIELD_BITS; i++){
            for (int j = 0; j < FIELD_BITS; j++){
                cout << (int)a.M[i][j] << " ";
            }
            cout << "\n";
        }
    }
};

uint8_t GF2_233_NB::M[FIELD_BITS][FIELD_BITS];
bool GF2_233_NB::MultMatrixBuilt = false;

int main() {
    GF2_233_NB a, b, c, d, e, f, g, n;
    cout << "a: ";
    a.Random(); a.PrintHex();
    cout << "b: ";
    b.Random(); b.PrintHex();
    cout << "n: ";
    n.Random(); n.PrintHex();

    c = a.Addition(b);
    cout << "a + b: "; c.PrintHex();

    d = a.Multiplication(b);
    cout << "a * b: "; d.PrintHex();

    f = a.Square();
    cout << "a ^ 2: "; f.PrintHex();

    e = a.InverseItohTsujii();
    cout << "a ^ -1: "; e.PrintHex();

    g = a.Pow(n);
    cout << "a ^ n: "; g.PrintHex();

    cout << "a * a ^ -1: "; (a.Multiplication(e)).PrintHex();

    cout << "Trace of a: " << a.Trace() << "\n";

    auto time1 = nanoseconds::zero();
    auto time2 = milliseconds::zero();
    auto time3 = milliseconds::zero();
    auto time4 = milliseconds::zero();
    auto time5 = milliseconds::zero();

    int M = 100;

    for (int i = 0; i < M; i++){
        GF2_233_NB X, Y;
        X.Random(); Y.Random();

        auto Start1 = high_resolution_clock::now();
            GF2_233_NB Z1 = X.Addition(Y);
        auto End1 = high_resolution_clock::now();
        auto duration1 = duration_cast<nanoseconds>(End1 - Start1);
        time1 = time1 + duration1;

        auto Start2 = high_resolution_clock::now();
            GF2_233_NB Z2 = X.Multiplication(Y);
        auto End2 = high_resolution_clock::now();
        auto duration2 = duration_cast<milliseconds>(End2 - Start2);
        time2 = time2 + duration2;

        auto Start3 = high_resolution_clock::now();
            GF2_233_NB Z3 = X.Inverse();
        auto End3 = high_resolution_clock::now();
        auto duration3 = duration_cast<milliseconds>(End3 - Start3);
        time3 = time3 + duration3;

        auto Start4 = high_resolution_clock::now();
            GF2_233_NB Z4 = X.InverseItohTsujii();
        auto End4 = high_resolution_clock::now();
        auto duration4 = duration_cast<milliseconds>(End4 - Start4);
        time4 = time4 + duration4;

        auto Start5 = high_resolution_clock::now();
            GF2_233_NB Z5 = X.Pow(Y);
        auto End5 = high_resolution_clock::now();
        auto duration5 = duration_cast<milliseconds>(End5 - Start5);
        time5 = time5 + duration5;
    }

    cout << "\n\nAverage time per operation: \n";
    cout << "Addition: " << time1.count()/M << " nanoseconds;\n";
    cout << "Multiplication: " << time2.count()/M << " milliseconds;\n";
    cout << "Inverse (Naive): " << time3.count()/M << " milliseconds;\n";
    cout << "Inverse (Itoh-Tsujii): " << time4.count()/M << " milliseconds;\n";
    cout << "Power: " << time5.count()/M << " milliseconds;\n";


    return 0;
}





















