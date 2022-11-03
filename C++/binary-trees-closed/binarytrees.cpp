#include <iostream>

int f(int d) { return (1 << (d + 1)) - 1; }

int main(int argc, char *argv[]) {
  int min_depth = 4;
  int max_depth = std::max(min_depth + 2, (argc == 2 ? atoi(argv[1]) : 10));
  int stretch_depth = max_depth + 1;

  std::cout << "stretch tree of depth " << stretch_depth
            << "\t check: " << f(stretch_depth) << std::endl;

  for (int d = min_depth; d <= max_depth; d += 2) {
    int iterations = 1 << (max_depth - d + min_depth);
    int c = f(d) * iterations;
    std::cout << iterations << "\t trees of depth " << d << "\t check: " << c
              << std::endl;
  }

  std::cout << "long lived tree of depth " << max_depth
            << "\t check: " << f(max_depth) << "\n";
}
