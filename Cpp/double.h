// double.h


#ifndef DOUBLE_H
#define DOUBLE_H

#include <iostream>

static double EPSILON = 0.000001;

void set_precision(double eps) {
    EPSILON = eps;
}


struct Double {
    double m_d;

    Double(double d) : m_d(d) {}
    bool operator==(const Double d) const { return std::abs(m_d - d.m_d) < EPSILON; }
};

namespace std {
    template <>
    struct hash<Double> {
        std::size_t operator() (Double d) const {
            return std::hash<double>()(d.m_d);
        }
    };
}

#endif  // DOUBLE_H
