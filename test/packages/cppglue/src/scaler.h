// C++ only (included by scale.cpp alone): not for the C parser.
#pragma once
namespace cppglue {
class Scaler {
public:
  explicit Scaler(double k) : k_(k) {}
  double operator()(double x) const { return x * k_; }
private:
  double k_;
};
}
