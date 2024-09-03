#pragma once

#include <fenv.h>

namespace core {

template <typename Float> struct float_convert {
  static constexpr uint16_t significand_width = std::numeric_limits<Float>::digits;
  static constexpr uint16_t exponent_width = std::bit_width((unsigned int)std::numeric_limits<Float>::max_exponent);
  static constexpr uint16_t sign_width = 1;
  static constexpr bool has_hidden_bit = ((sign_width + exponent_width + significand_width) % 8) != 0;
  static constexpr uint16_t storage_width = sign_width + exponent_width + significand_width + ((has_hidden_bit) ? -1 : 0);
  static constexpr int32_t exponent_bias = std::numeric_limits<Float>::max_exponent + significand_width - 2;
  static constexpr int32_t max_exponent = std::numeric_limits<Float>::max_exponent - significand_width;
  static constexpr int32_t min_exponent = 2 - exponent_bias - significand_width;
  static constexpr int32_t min_normalized_exponent = 1 - exponent_bias;
  using uint_t =
      std::conditional_t<storage_width <= 8, uint8_t,
                         std::conditional_t<storage_width <= 16, uint16_t,
                                            std::conditional_t<storage_width <= 32, uint32_t,
                                                               std::conditional_t<storage_width <= 64, uint64_t, __uint128_t>>>>;
  static constexpr uint_t hidden_bit = has_hidden_bit ? uint_t{1} << (significand_width - 1) : uint_t{0};
  static constexpr uint16_t exponent_shift = storage_width - sign_width - exponent_width;
  static constexpr uint16_t sign_shift = storage_width - sign_width;
  static constexpr uint_t significand_mask = (uint_t{1} << (significand_width + ((has_hidden_bit) ? -1 : 0))) - uint_t{1};
  static constexpr uint_t payload_mask = (uint_t{1} << (significand_width + ((has_hidden_bit) ? -2 : -1))) - uint_t{1};
  static constexpr int32_t non_finite_exponent = static_cast<int32_t>(((uint_t{1} << exponent_width) - uint_t{1}));
  static constexpr uint_t exponent_mask = ((uint_t{1} << exponent_width) - uint_t{1}) << exponent_shift;
  static constexpr uint_t sign_mask = ((uint_t{1} << sign_width) - uint_t{1}) << sign_shift;
  static constexpr uint_t nan_type_mask = uint_t{1} << (significand_width + ((has_hidden_bit) ? -2 : -1));

  enum class category { finite, quiet_nan, signaling_nan, infinity };

  typedef union {
    Float f;
    uint_t b;
  } convert_t;

  static inline uint_t to_bits(Float f) {
    convert_t convert = {.f = f};
    return convert.b;
  }

  static inline Float from_bits(uint_t b) {
    convert_t convert = {.b = b};
    return convert.f;
  }

  struct quadruple {
    category category;
    uint_t significand;
    int32_t exponent;
    int16_t sign;
  };

  static quadruple to_quadruple(Float f) {
    quadruple q;
    uint_t b = to_bits(f);

    q.significand = b & significand_mask;
    q.exponent = static_cast<int32_t>((b & exponent_mask) >> exponent_shift);
    q.sign = (b & sign_mask) ? int32_t{-1} : int32_t{1};

    if (q.exponent != non_finite_exponent) {
      q.category = category::finite;
      if (q.exponent != 0) {
        // Normal non-zero
        q.significand |= hidden_bit;
        q.exponent -= exponent_bias;
      } else if (q.significand != 0) {
        // Subnormal
        int32_t shift = significand_width - std::bit_width(q.significand);
        q.significand = q.significand << shift;
        q.exponent = 1 - exponent_bias - shift;
      }
    } else if (q.significand == 0) {
      q.category = category::infinity;
    } else if (q.significand & nan_type_mask) {
      q.category = category::quiet_nan;
      q.significand &= ~nan_type_mask;
    } else {
      q.category = category::signaling_nan;
    }

    return q;
  }

  static Float from_quadruple(quadruple q) {
    uint_t b = 0;

    if (q.sign < 0)
      b |= sign_mask;

    switch (q.category) {
    case category::infinity:
      b |= exponent_mask;
      break;
    case category::quiet_nan:
      b |= exponent_mask | nan_type_mask | (q.significand & payload_mask);
      break;
    case category::signaling_nan:
      b |= exponent_mask | ((q.significand == 0) ? uint_t{1} : (q.significand & payload_mask));
      break;
    default:
      if (q.significand != 0) {
        uint_t significand = q.significand;
        int32_t exponent = q.exponent;
        int32_t shift = std::bit_width(significand) - significand_width;

        // If we don't have enough bits then right shift.
        if (shift < 0) {
          significand = significand << -shift;
          exponent += shift;
          shift = 0;
        }

        // Check for subnormals and set the shift needed to normalize.
        if ((exponent + shift) < min_normalized_exponent) {
          shift = min_normalized_exponent - exponent;
        }

        // If we shift away all of the bits that is an underflow.
        if (shift > std::bit_width(significand)) {
          feraiseexcept(FE_UNDERFLOW);
          // Return +/- zero if traps masked.
          return from_bits(b);
        }

        // Round if we have extra bits.
        if (shift > 0) {
          if (significand & ((1 << shift) - 1))
            feraiseexcept(FE_INEXACT);
          significand = ((significand & (1 << (shift - 1))) ? (significand + (1 << shift)) : significand) >> shift;
          exponent += shift;
        }

        // Check one more time to ensure rounding hasn't increased the width.
        shift = std::max(static_cast<int>(std::bit_width(significand) - significand_width), 0);
        significand = significand >> shift;
        exponent += shift;

        // Check for overflow.
        if (exponent > max_exponent) {
          feraiseexcept(FE_OVERFLOW);
          // Return +/- infinity if traps masked.
          return from_bits(b | exponent_mask);
        }

        if (std::bit_width(significand) < significand_width) {
          // Subnormals
          b |= significand & significand_mask;
        } else {
          // Normal
          b |= (significand & significand_mask) |
               ((static_cast<uint_t>(exponent + exponent_bias) << exponent_shift) & exponent_mask);
        }
      }
      break;
    }

    return from_bits(b);
  };

  template <std::integral Integral> static Float from_integral(Integral i) {
    quadruple q = {.category = category::finite, .sign = (i < 0) ? -1 : 1};
    i = std::abs(i);
    int32_t shift = std::bit_width(i) - std::bit_width(std::numeric_limits<uint_t>::max);
    if (shift > 0) {
      q.significand = i >> shift;
      q.exponent = shift;
    } else {
      q.significand = i;
    }

    return from_quadruple(q);
  }
};

} // namespace core
