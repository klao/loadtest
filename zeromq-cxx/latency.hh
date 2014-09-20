#include <algorithm>
#include <cmath>
#include <ctime>
#include <limits>
#include <vector>

inline int64_t gettime() {
  struct timespec t;
  clock_gettime(CLOCK_REALTIME, &t);
  return static_cast<int64_t>(t.tv_sec) * 1000000000
    + static_cast<int64_t>(t.tv_nsec);
}

class latency_t {
private:
  std::vector<int64_t> _samples;
  int64_t _sum;
  bool _sorted;

  int64_t _start;

private:

  void sort() { if (!_sorted) std::sort(_samples.begin(), _samples.end()); }

public:
  latency_t() : _samples(), _sum(0), _sorted(false), _start(-1) {}

  void start() {
    assert(_start == -1);
    _start = gettime();
  }

  void stop() {
    assert(_start > 0);
    int64_t diff = gettime() - _start;

    _samples.push_back(diff);
    _sorted = false;
    _sum += diff;
    _start = -1;
  }

  int64_t size() { return _samples.size(); }

  int64_t mean() {
    assert(size() > 0);
    return _sum / size();
  }

  int64_t minimum() {
    assert(size() > 0);
    sort();
    return _samples.front();
  }

  int64_t maximum() {
    assert(size() > 0);
    sort();
    return _samples.back();
  }

  int64_t percentile(double pct) {
    assert(size() > 0);
    sort();
    int k = floor(pct * size() / 100.0);
    // k = std::min(size() - 1, std::max(0, k));
    return _samples.at(k);
  }

  double mean_msec() { return static_cast<double>(mean()) / 1000000.0; }
  double minimum_msec() { return static_cast<double>(minimum()) / 1000000.0; }
  double maximum_msec() { return static_cast<double>(maximum()) / 1000000.0; }
  double percentile_msec(double pct) {
    return static_cast<double>(percentile(pct) / 1000000.0);
  }
};
