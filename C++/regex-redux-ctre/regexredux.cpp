#include <array>
#include <cassert>
#include <chrono>
#include <cstdio>
#include <iostream>

#include <ctre.hpp>
#include <iterator>

template <ctll::fixed_string RE>
constexpr std::string replace_all(const std::string &str,
                                  const std::string &replacement) {
  std::string copy;
  auto i = str.begin();
  while (const auto match = ctre::search<RE>(i, str.end())) {
    copy.append(i, match.begin());
    copy.append(replacement);
    i = match.end();
  }

  copy.append(i, str.end());
  return copy;
}

// This version is faster, but only works when the replacement size is always at
// most as long as the match.
template <ctll::fixed_string RE>
constexpr void replace_all_ip(std::string &str,
                              const std::string &replacement) {

  auto i = str.begin();
  auto j = str.begin();

  while (const auto match = ctre::search<RE>(i, str.end())) {
    assert(replacement.size() <= std::distance(match.begin(), match.end()));
    while (i != match.begin()) {
      *j++ = *i++;
    }
    for (const auto c : replacement) {
      *j++ = c;
    }
    i = match.end();
  }
}

template <ctll::fixed_string RE> constexpr int count(const std::string &str) {
  int count = 0;
  for (auto i = str.begin(); const auto match = ctre::search<RE>(i, str.end());
       i = match.end()) {
    ++count;
  }
  return count;
}

#define PATTERN_COUNT_0 "agggtaaa|tttaccct"
#define PATTERN_COUNT_1 "[cgt]gggtaaa|tttaccc[acg]"
#define PATTERN_COUNT_2 "a[act]ggtaaa|tttacc[agt]t"
#define PATTERN_COUNT_3 "ag[act]gtaaa|tttac[agt]ct"
#define PATTERN_COUNT_4 "agg[act]taaa|ttta[agt]cct"
#define PATTERN_COUNT_5 "aggg[acg]aaa|ttt[cgt]ccct"
#define PATTERN_COUNT_6 "agggt[cgt]aa|tt[acg]accct"
#define PATTERN_COUNT_7 "agggta[cgt]a|t[acg]taccct"
#define PATTERN_COUNT_8 "agggtaa[cgt]|[acg]ttaccct"

int main() {
  int len1, len2, len3;

  fseek(stdin, 0, SEEK_END);
  [[maybe_unused]] int read_size = ftell(stdin);
  assert(read_size > 0);

  auto str = [&]() {
    std::string str;
    str.resize(read_size);
    rewind(stdin);
    read_size = fread(&str[0], 1, read_size, stdin);
    assert(read_size);
    return str;
  }();

  len1 = str.size();
  replace_all_ip<">[^\\n]+\\n|[\\n]">(str, "");
  len2 = str.size();

  std::array<int, 9> counts = {0, 0, 0, 0, 0, 0, 0, 0, 0};

#pragma omp parallel sections
  {
#pragma omp section
    counts[0] = count<PATTERN_COUNT_0>(str);
#pragma omp section
    counts[1] = count<PATTERN_COUNT_1>(str);
#pragma omp section
    counts[2] = count<PATTERN_COUNT_2>(str);
#pragma omp section
    counts[3] = count<PATTERN_COUNT_3>(str);
#pragma omp section
    counts[4] = count<PATTERN_COUNT_4>(str);
#pragma omp section
    counts[5] = count<PATTERN_COUNT_5>(str);
#pragma omp section
    counts[6] = count<PATTERN_COUNT_6>(str);
#pragma omp section
    counts[7] = count<PATTERN_COUNT_7>(str);
#pragma omp section
    counts[8] = count<PATTERN_COUNT_8>(str);
#pragma omp section
    {
      auto str2 = str;
      replace_all_ip<"tHa[Nt]">(str2, "<4>");
      replace_all_ip<"aND|caN|Ha[DS]|WaS">(str2, "<3>");
      str2 = replace_all<"a[NSt]|BY">(str2, "<2>");
      replace_all_ip<"<[^>]*>">(str2, "|");
      replace_all_ip<"\\|[^|][^|]*\\|">(str2, "-");
      len3 = str2.size();
    }
  }

  std::cout << PATTERN_COUNT_0 << " " << counts[0] << "\n";
  std::cout << PATTERN_COUNT_1 << " " << counts[1] << "\n";
  std::cout << PATTERN_COUNT_2 << " " << counts[2] << "\n";
  std::cout << PATTERN_COUNT_3 << " " << counts[3] << "\n";
  std::cout << PATTERN_COUNT_4 << " " << counts[4] << "\n";
  std::cout << PATTERN_COUNT_5 << " " << counts[5] << "\n";
  std::cout << PATTERN_COUNT_6 << " " << counts[6] << "\n";
  std::cout << PATTERN_COUNT_7 << " " << counts[7] << "\n";
  std::cout << PATTERN_COUNT_8 << " " << counts[8] << "\n";

  std::cout << "\n";
  std::cout << len1 << "\n";
  std::cout << len2 << "\n";
  std::cout << len3 << "\n";
}
