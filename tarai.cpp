// tarai.cpp
// $ g++ -o tarai -02 - std=c++11 tarai.cpp
#include <iostream>
#include <cstdlib>
#include <functional>

// 積極評価版たらい回し関数
int tarai(int x, int y, int z) {
    return (x <= y)
        ? y
        : tarai(tarai(x - 1, y, z),
                tarai(y - 1, z, x),
                tarai(z - 1, x, y));
}

// 遅延評価版たらい回し関数
int lazy_tarai(int x, int y, int z) {
    auto lt = [&lt] (auto thunk_x, auto thunk_y, auto thunk_z) -> int {
        const int x = thunk_x();
        const int y = thunk_y();
        if (x <= y) {
            return y;
        } else {
            const int z = thunk_z();
            auto const_x = [&x] { return x; };
            auto const_y = [&y] { return y; };
            auto const_z = [&z] { return z; };
            return lt([&]{ return lt([&x] { return x - 1; }, const_y, const_z); },
                    [&]{ return lt([&x] { return x - 1; }, const_y, const_z); },
                    [&]{ return lt([&x] { return x - 1; }, const_y, const_z); });
        }
    };
    return lt([=] { return x; }, [=] { return y; }, [=] { return z; });
}

int main(int argc, char* argv[]) {
    if (argc < 4) return 1;
    int x = std::atoi(argv[1]);
    int y = std::atoi(argv[2]);
    int z = std::atoi(argv[3]);
    std::cout << lazy_tarai(x, y, z) << std::endl;
    return 0;
}
